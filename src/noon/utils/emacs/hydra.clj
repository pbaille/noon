(ns noon.utils.emacs.hydra
  (:require [backtick :refer [template]]
            [clojure.string :as str]))

(def hydra-option-keys [:color :pre :post :exit :foreign-keys :bind :hint :timeout])
(def hydra-head-option-keys [:color :exit :bind :column])

(def default-hydra-options
  {:color 'teal})

(defn default-head-wrapper [parent child]
  (if (:hydra/expr child)
    child
    (update child :hydra/options
            (fn [o] (merge (select-keys (:hydra/options parent)
                                       [:color :exit :foreign-keys :timeout])
                          o)))))


(def sample-hydra-def
  (template
   [:root nil
    {:color teal
     :description "this is the root"
     :wrap-head ~default-head-wrapper}
    [:one "a"
     [:a "a" (message "one a")]
     [:b "b" (message "one b")]]
    [:two "b" {:wrap-head ~(fn [_ h] (if (:hydra/expr h)
                                    (update h :hydra/expr #(list 'progn % %))
                                    h))}
     [:a "a" (message "two a")]
     [:b "b" (message "two b")]
     [:three "c" {:foo bar}
      [:deep "d" (message "deep")]]]]))

(do :help
    (defn symjoin [sep xs] (->> (map name xs) (str/join sep) symbol))
    (defn map->plist [m]
      (mapcat identity m))
    (defn hydra-sym [segments] (symjoin "/" (cons :noon-hydra segments)))
    (defn hydra-body-varsym [segments] (symbol (str "#'" (hydra-sym segments) "/body")))
    (defn options->plist [options]
      (map->plist (select-keys options hydra-option-keys)))
    (defn options->head-plist [options]
      (map->plist (select-keys options hydra-head-option-keys))))

(defn parse-hydra [from [hydra-name key x & xs]]
  (let [[{:as opts :keys [wrap-head] :or {wrap-head (fn [_ x] x)}}
         body] (if (map? x) [x xs] [{} (cons x xs)])
        from (conj from hydra-name)
        base {:hydra/name (hydra-sym from)
              :hydra/key key
              :hydra/path from
              :hydra/hint (or (:hint opts) (name hydra-name))}]
    (if (vector? (first body))
      (let [parent (assoc base
                          :hydra/body-varsym (hydra-body-varsym from)
                          :hydra/options (dissoc opts :wrap-head)
                          :hydra/docstring (:doc opts)
                          :hydra/heads (map first body))]
        (cons parent (map (partial wrap-head parent) (mapcat (partial parse-hydra from) body))))
      [(assoc base
              :hydra/options (options->head-plist opts)
              :hydra/expr (first body))])))

(defn hydra-path-map [hydras]
  (reduce (fn [ret h] (assoc ret (:hydra/path h) h))
          {} hydras))

(defn hydra-compile-head
  [{:hydra/keys [key hint expr name options path body-varsym]}]
  (if expr
    (list* key expr hint options)
    (list key body-varsym hint)))

(defn hydra-compile-one [path path-map]
  (let [{:hydra/keys [heads name options]} (get path-map path)]
    (when heads
      (list* 'defhydra name (options->plist options)
             (map (fn [h] (hydra-compile-head (get path-map (conj path h))))
                  heads)))))

(defn hydra-compile [spec]
  (let [hydras (parse-hydra [] spec)
        path-map (hydra-path-map hydras)]
    (keep (fn [[path h]] (hydra-compile-one path path-map)) path-map)))

(parse-hydra [] sample-hydra-def)
(hydra-compile sample-hydra-def)
