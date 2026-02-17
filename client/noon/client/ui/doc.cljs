(ns noon.client.ui.doc
  (:require [clojure.string :as str]
            [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]
            [noon.client.state :refer [<< >>]]
            [noon.client.doc :as doc]
            [noon.client.ui.section :as ui.section]
            [noon.client.ui.misc :as ui.misc]
            [noon.client.ui.code-editor :as ui.code-editor]
            [noon.client.ui.sidebar :as ui.sidebar]
            [noon.client.ui.breadcrumbs :as ui.breadcrumbs]
            [noon.client.constants :as constants]))

(defui connected-code-editor [props]
  (let [show-piano-roll? (<< [:piano-rolls.get])]
    (uix/$ ui.code-editor/code-editor (assoc-in props [:options :show-piano-roll?] show-piano-roll?))))

(defn render-doc-node [node]
  (case (:type node)
    :section (uix/$ ui.section/section
                    (assoc node :has-subsections
                           (boolean (seq (:children node))))
                    (mapv (fn [i c] (render-doc-node (assoc c :key (str i))))
                          (range)
                          (concat (:content node)
                                  (sort-by :idx (vals (:children node))))))
    :raw (uix/$ ui.misc/raw node)
    :code (uix/$ connected-code-editor node)))

(def doc-content
  (render-doc-node doc/doc-data))

(def doc-styles {:p 0
                 :text [:sans {:leading :normal}]
                 ".cm-editor" {:bg {:color "transparent"}}
                 "code" {:bg {:color [:gray {:a 0.1}]}
                         :color [:black {:a 0.8}]
                         :p [1 0.5]
                         :text :bold
                         :rounded 1}
                 "code.focus" {:bg {:color [:tomato {:a 0.5}]}}
                 ".cm-editor.cm-focused" {:border {:width 0}
                                          :outline :none
                                          :box-shadow :none}
                 :height "100vh"})

(defui doc []

  ;; if url contains hash part, we scroll to it
  (uix/use-effect (fn []
                    (let [hash (-> js/window .-location .-hash (str/replace #"^#" ""))]
                      (when-let [element (.getElementById js/document hash)]
                        (.scrollIntoView element #js {:behavior "instant"}))))
                  [])

  (sc :div.doc
      (merge doc-styles
             {:height :full
              :flex [:row {:gap 1 :items :stretch}]})
      (c ui.sidebar/sidebar)
      (sc {:p 0
           :height "100vh"
           :flex :column
           :width {:max 800
                   :min 400}}
          (c ui.breadcrumbs/breadcrumbs)
          (c :div.no-scrollbar
             {:id constants/DOC_CONTAINER_ID
              :on-scroll (fn [event] (>> [:doc.ui.scrolling.position.set (.-scrollTop (.-target event))]))
              :style {:flex-shrink 1
                      :overflow :scroll
                      :p {:right 3}}}
             doc-content))))

(comment
  (>> [:upd [:ui :sidebar] not])
  (>> [:doc.ui.nodes.pp])
  (<< [:get [:ui :breadcrumbs]])
  ())
