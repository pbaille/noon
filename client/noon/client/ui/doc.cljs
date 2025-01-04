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

(defui doc []
  (let [mode (<< [:get [:doc :ui :navigation-mode]])]
    (case mode
      :sidebar (sc {:height :full :flex [:row {:gap 2 :items :stretch}]}
                   (c ui.navigation/sidebar)
                   (sc {:height "100vh"
                        :overflow :scroll}
                       (render-doc-node (<< [:doc.tree.get []]))))
      :breadcrumbs (sc {:p [2 0]}
                       (c ui.navigation/breadcrumbs)
                       (render-doc-node (<< [:doc.tree.get []]))))))

(comment
  (>> [:upd [:ui :sidebar] not])
  (>> [:doc.ui.nodes.pp])
  (<< [:get [:ui :breadcrumbs]])
  ())
