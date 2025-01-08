(ns noon.client.state
  (:require [uic.state :as state :refer [signal sub dbf event]]
            [noon.client.doc :as doc]
            [noon.client.constants :as constants]
            [noon.client.utils.flat-tree :as flat-tree]))

(defn breadcrumbs [at nodes]
  (mapv (fn [path]
          (let [node (get nodes (vec path))]
            {:path path
             :inline-code (:inline-code node)
             :text (:title node)
             :level (count path)
             :href (->> path
                        (interpose "/")
                        (cons "#/")
                        (apply str))}))
        (next (reductions conj [] at))))

(def tree
  {:init (event [_ _] {:pp ["init"]})
   :doc {:ui
         {:nodes {:get
                  (sub [db [_ path k]]
                       (get-in db [:doc :ui :nodes path k]))

                  :set
                  (dbf [db [_ path v]]
                       (assoc-in db [:doc :ui :nodes path] v))

                  :upd
                  (dbf [db [_ path f]]
                       (update-in db [:doc :ui :nodes path] f))

                  :pp
                  (event [cofx [_ k]]
                         (let [nodes (get-in cofx [:db :doc :ui :nodes])]
                           {:pp [(if k (get nodes k) k)]}))}

          :folding {:get
                    (sub [db [_ path]]
                         (or (get-in db [:doc :ui :nodes path :folding])
                             :expanded))

                    :set
                    (dbf [db [_ path value]]
                         (case value
                           :expanded (update-in db [:doc :ui :nodes]
                                                (fn [nodes]
                                                  (println :expand path (flat-tree/parent-path path))
                                                  (-> nodes
                                                      (flat-tree/update-recursively-from
                                                       path (fn [node] (assoc node :folding :expanded)))
                                                      (assoc-in [(flat-tree/parent-path path) :folding]
                                                                :expanded))))

                           :summary (update-in db [:doc :ui :nodes]
                                               (fn [nodes]
                                                 (-> nodes
                                                     (assoc-in [path :folding] :summary)
                                                     (assoc-in [(flat-tree/parent-path path) :folding] :expanded)
                                                     (flat-tree/update-children
                                                      path (fn [node] (assoc node :folding :folded))))))

                           :folded (assoc-in db [:doc :ui :nodes path :folding] :folded)))

                    :visible-nodes
                    (sub [db [_ _]]
                         (let [nodes (get-in db [:doc :ui :nodes])
                               folded-paths (keep (fn [[path {:keys [folding]}]]
                                                    (when (= :folded folding) path))
                                                  nodes)]
                           (remove (fn [[path _]]
                                     (some (fn [p] (flat-tree/subpath? path p))
                                           folded-paths))
                                   nodes)))}

          :sidebar {:folding {:get (signal [{nodes [:get [:doc :ui :nodes]]
                                             current-path [:doc.ui.current-path]}
                                            [_ path]]
                                           (if (flat-tree/subpath? current-path path)
                                             :expanded
                                             (or (get-in nodes [path :sidebar :folding])
                                                 :folded)))
                              :set (dbf [db [_ path value]]
                                        (case value
                                          :expanded (update-in db [:doc :ui :nodes]
                                                               (fn [nodes]
                                                                 (-> (assoc-in nodes [path :sidebar :folding] :expanded)
                                                                     (flat-tree/update-children
                                                                      path (fn [node] (assoc-in node [:sidebar :folding] :folded))))))
                                          :folded (assoc-in db [:doc :ui :nodes path :sidebar :folding] :folded)))}}

          :current-path (signal [{mode [:doc.ui.navigation-mode.get]
                                  nodes [:doc.ui.folding.visible-nodes]
                                  _scrolling-pos [:doc.ui.scrolling.position.get]}
                                 _]
                                (->> nodes
                                     (sort-by (comp :idx val))
                                     (reduce (fn [ret [path node]]
                                               (if (>= (case mode :sidebar 0 :breadcrumbs constants/BREADCRUMBS_HEIGHT)
                                                       (js/Math.floor (.-top (.getBoundingClientRect @(:header-ref node)))))
                                                 (cons path ret)
                                                 (reduced ret)))
                                             ())
                                     (first)))

          :breadcrumbs {:get (signal [{nodes [:get [:doc :ui :nodes]]
                                       current-path [:doc.ui.current-path]} _]
                                     (breadcrumbs current-path nodes))}

          :navigation-mode {:get (sub [db _] (get-in db [:doc :ui :navigation-mode]))
                            :set (dbf [db [_ value]]
                                      (case value
                                        (:sidebar :breadcrumbs)
                                        (assoc-in db [:doc :ui :navigation-mode] value)))}
          :scrolling {:position {:set (dbf [db [_ pos]]
                                           (assoc-in db [:doc :ui :scrolling :position] pos))
                                 :get (sub [db _]
                                           (get-in db [:doc :ui :scrolling :position]))}}}
         :tree {:get (sub [db [_ path]]
                          (get-in db (concat [:doc :tree]
                                             (interleave (repeat :subsections) path))))}}})

(def initial-db
  {:doc {:ui {:navigation-mode :sidebar #_:breadcrumbs
              :scrolling {:position 0}
              :nodes {}}
         :tree doc/doc-data}})

(let [[subscribe dispatch]
      (state/init-frame {:id :noon-doc
                         :tree tree
                         :db initial-db
                         :init [:init]})]
  (def >> dispatch)
  (def << subscribe))
