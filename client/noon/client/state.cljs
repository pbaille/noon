(ns noon.client.state
  (:require [uic.state :as state :refer [signal sub dbf effect event]]
            [noon.client.doc :as doc]
            [noon.client.utils.flat-tree :as flat-tree]))

(defn breadcrumbs [at]
  (mapv (fn [path]
          {:path path
           :text (last path)
           :level (count path)
           :href (->> path
                      (interpose "/")
                      (cons "#/")
                      (apply str))})
        (next (reductions conj [] at))))

(def tree
  {:init (event [_ _] {:pp ["init"]})
   :doc {:ui
         {:nodes {:get
                  (sub [db [_ path k]]
                       (get-in db [:doc :ui :nodes path k]))

                  :set
                  (dbf [db [_ path k v]]
                       (assoc-in db [:doc :ui :nodes path k] v))

                  :pp
                  (event [cofx [_ k]]
                         (let [nodes (get-in (:db cofx) [:doc :ui :nodes])]
                           {:pp [(if k (get nodes k) k)]}))}

          :folding {:get
                    (sub [db [_ path]]
                         (or (get-in db [:doc :ui :nodes path :folding])
                             :expanded))

                    :set
                    (dbf [db [_ path value]]
                         (case value
                           :expanded (update-in db [:doc :ui :nodes]
                                                flat-tree/update-recursively-from path
                                                (fn [node] (assoc node :folding :expanded)))

                           :summary (update-in db [:doc :ui :nodes]
                                               (fn [nodes]
                                                 (-> (assoc-in nodes [path :folding] :expanded)
                                                     (flat-tree/update-children
                                                      path (fn [node] (assoc node :folding :folded))))))

                           :folded (assoc-in db [:doc :ui :nodes path :folding] :folded)))}

          :sidebar {:folding {:get (sub [db [_ path]]
                                        (or (get-in db [:doc :ui :nodes path :sidebar :folding])
                                            :folded))
                              :set (dbf [db [_ path value]]
                                        (case value
                                          :expanded (update-in db [:doc :ui :nodes]
                                                               (fn [nodes]
                                                                 (-> (assoc-in nodes [path :sidebar :folding] :expanded)
                                                                     (flat-tree/update-children
                                                                      path (fn [node] (assoc-in node [:sidebar :folding] :folded))))))
                                          :folded (assoc-in db [:doc :ui :nodes path :sidebar :folding] :folded)))}}

          :breadcrumbs {:get
                        (sub [db _]
                             (->> (get-in db [:doc :ui :nodes])
                                  (sort-by (comp count key) >)
                                  (some (fn [[path {:keys [content-visible header-visible]}]]
                                          (if (and content-visible (not header-visible))
                                            (breadcrumbs path))))))}}
         :tree {:get (sub [db [_ path]]
                          (get-in db (concat [:doc :tree]
                                             (interleave (repeat :subsections) path))))}}})

(def initial-db
  {:doc {:ui {:navigation-mode :sidebar #_:breadcrumbs
              :nodes {}}
         :tree doc/doc-data}})

(let [[subscribe dispatch]
      (state/init-frame {:id :noon-doc
                         :tree tree
                         :db initial-db
                         :init [:init]})]
  (def >> dispatch)
  (def << subscribe))
