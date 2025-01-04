(ns noon.client.ui.doc
  (:require [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]
            [noon.client.state :refer [<< >>]]
            [noon.client.ui.section :as ui.section]
            [noon.client.ui.misc :as ui.misc]
            [noon.client.ui.code-editor :as ui.code-editor]
            [noon.client.ui.navigation :as ui.navigation]))

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
    :code (uix/$ ui.code-editor/code-editor node)))

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
  (let [mode (<< [:get [:doc :ui :navigation-mode]])]
    (case mode
      :sidebar (sc (merge doc-styles
                          {:height :full
                           :flex [:row {:gap 2 :items :stretch}]
                           :width {:max 1100}})
                   (c ui.navigation/sidebar)
                   (sc {:p [2 3 0 0] :height "100vh"
                        :overflow :scroll}
                       (render-doc-node (<< [:doc.tree.get []]))))
      :breadcrumbs (sc (merge doc-styles
                              {:p [2 0]
                               :width {:max 800}})
                       (c ui.navigation/breadcrumbs)
                       (render-doc-node (<< [:doc.tree.get []]))))))

(comment
  (>> [:upd [:ui :sidebar] not])
  (>> [:doc.ui.nodes.pp])
  (<< [:get [:ui :breadcrumbs]])
  ())
