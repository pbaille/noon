(ns noon.utils.emacs.hydra
  (:require [backtick :refer [template]]
            [clojure.string :as str]))

(def sample-hydra-def
  (template
   [:root "H"
    {:description "this is the root"}
    [:one "a"
     [:a "a" (message "one a")]
     [:b "b" (message "one b")]]
    [:two "b" {:wrap-head ~(fn [x] (list 'progn x x))}
     [:a "a" (message "two a")]
     [:b "b" (message "two b")]
     [:three "c" {:foo bar}
      [:deep "d" (message "deep")]]]]))


(def default-hydra-options
  {:color 'teal
   :wrap-head identity})

(def hydra-option-keys [:color :pre :post :exit :foreign-keys :bind :hint :timeout])
(def hydra-head-option-keys [:color :exit :bind :column])

(do :help
    (defn symjoin [sep xs] (->> (map name xs) (str/join sep) symbol))
    (defn map->plist [m]
      (mapcat identity m))
    (defn hydra-sym [segments] (symjoin "/" (cons :noon-hydra segments)))
    (defn hydra-body-varsym [segments] (symbol (str "#'" (hydra-sym segments) "/body")))
    (defn hydra-description [h]
      (or (:description (:hydra/options h))
          (name (:hydra/name h))))
    (defn options->plist [options]
      (map->plist (select-keys options hydra-option-keys)))
    (defn options->head-plist [options]
      (map->plist (select-keys options hydra-head-option-keys)))
    (defn merge-hydra-options [options {:as more :keys [wrap-head]}]
      (-> (merge (dissoc options :description) (dissoc more :wrap-head))
          (update :wrap-head (fn [x] (if wrap-head
                                      (comp x wrap-head)
                                      x))))))

(comment :first-try
         (defn parse-hydra [options [hydra-name key x & xs]]
           (let [[opts body] (if (map? x) [x xs] [{} (cons x xs)])
                 next-options (merge-hydra-options options opts)]
             (merge {:hydra/name hydra-name
                     :hydra/key key
                     :hydra/options next-options}
                    (if (vector? (first body))
                      {:hydra/body true
                       :hydra/children (mapv (partial parse-hydra next-options) body)}
                      {:hydra/head true
                       :hydra/impl (first body)}))))
         (defn compile-hydra-child
           [at {:as h :hydra/keys [head options key impl name]}]
           (if head
             (let [wrap-head (:wrap-head options)]
               (list* key (wrap-head impl) (hydra-description h) (options->head-plist options)))
             (list key (hydra-body-varsym (conj at name)) (hydra-description h))))

         (defn compile-hydra
           [at {:as hydra :hydra/keys [options body head children wrap-head]}]
           (let [at (conj at (:hydra/name hydra))]
             (cons (template (defhydra ~(hydra-sym at) ~(options->plist options)
                               ~(hydra-description hydra)
                               ~@(map (partial compile-hydra-child at) children)))
                   (mapcat (partial compile-hydra at)
                           (filter :hydra/body children)))))

         (->> sample-hydra-def
              (parse-hydra default-hydra-options)
              (compile-hydra [])))

(defn parse-hydra [from options [hydra-name key x & xs]]
  (let [[opts body] (if (map? x) [x xs] [{} (cons x xs)])
        next-options (merge-hydra-options options opts)
        from (conj from hydra-name)
        base {:hydra/name (hydra-sym from)
              :hydra/key key
              :hydra/path from}]
    (if (vector? (first body))
      (cons (assoc base
                   :hydra/options (options->plist next-options)
                   :hydra/docstring (:doc opts)
                   :hydra/heads (map first body))
            (mapcat (partial parse-hydra from next-options) body))
      [(assoc base
              :hydra/hint (or (:hint opts) (name hydra-name))
              :hydra/options (options->head-plist opts)
              :hydra/expr ((:wrap-head options) (first body)))])))

(defn hydra-path-map [hydras]
  (reduce (fn [ret h] (assoc ret (:hydra/path h) h))
          {} hydras))

(defn hydra-compile-one [path path-map]
  (let [{:hydra/keys [heads name options]} (get path-map path)]
    (when heads
      (template (defhydra ~name ~options
                  ~@(map (fn [h] (let [{:hydra/keys [key hint expr name options path]} (get path-map (conj path h))]
                                  (if expr
                                    (list* key expr hint options)
                                    (list key (hydra-body-varsym path) hint))))
                         heads))))))

(defn hydra-compile [spec]
  (let [hydras (parse-hydra [] default-hydra-options spec)
        path-map (hydra-path-map hydras)]
    (keep (fn [[path h]] (hydra-compile-one path path-map)) path-map)))

(hydra-compile sample-hydra-def)
