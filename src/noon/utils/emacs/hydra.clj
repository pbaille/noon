(ns noon.utils.emacs.hydra
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(def hydra-option-keys [:color :pre :post :exit :foreign-keys :bind :hint :timeout])
(def hydra-head-option-keys [:color :exit :bind :column])
(def inherited-options [:color :exit :foreign-keys :timeout])

(def default-hydra-options
  {:color 'teal})

(do :help
    (defn symjoin [sep xs]
      (->> (map name xs) (str/join sep) symbol))
    (defn pretty-str [x]
      (with-out-str (pp/pprint x)))
    (defn map->plist [m]
      (mapcat identity m))
    (defn hydra-sym [segments]
      (symjoin "/" (cons :noon-hydra segments)))
    (defn hydra-body-varsym [segments]
      (symbol (str "#'" (hydra-sym segments) "/body")))
    (defn hydra-docstring [options]
      (:hydra/docstring options))
    (defn options->plist [options]
      (map->plist (select-keys options hydra-option-keys)))
    (defn options->head-plist [options]
      (map->plist (merge {:column " "} (select-keys options hydra-head-option-keys))))
    (defn hydra-body-prelude [path-map hydra]
      (concat (when-let [docstring (hydra-docstring hydra)]
                [docstring])
              [(list "q" nil "quit" :exit true :column "Nav")]
              (if-let [parent (get path-map (vec (butlast (:hydra/path hydra))))]
                [(list "ESC" (:hydra/body-varsym parent)
                       (str ".." (clojure.core/name (last (:hydra/path parent)))) :column "Nav")])))

    (defn without-qualified-keys [m]
      (->> (remove (comp qualified-keyword? key) m)
           (into {}))))

(defn parse-hydra [{:as parent :hydra/keys [path]} [hydra-name key x & xs]]
  (let [[opts body] (if (map? x) [x xs] [{} (cons x xs)])
        path (conj path hydra-name)
        wrappers (:hydra/wrappers parent [])
        wrap-self (reduce comp identity (reverse wrappers))
        base {:hydra/name (hydra-sym path)
              :hydra/key key
              :hydra/path path
              :hydra/hint (or (:hint opts) (name hydra-name))
              :hydra/wrappers (:hydra/wrappers opts [])}]
    (if (vector? (first body))
      (let [options (merge (select-keys (:hydra/options parent) inherited-options)
                           (without-qualified-keys opts))
            self (wrap-self (assoc base
                                   :hydra/body-varsym (hydra-body-varsym path)
                                   :hydra/options options
                                   :hydra/docstring (:doc opts)
                                   :hydra/heads (map first body)))]
        (cons self (mapcat (partial parse-hydra self)
                           body)))
      [(wrap-self
        (assoc base
               :hydra/options (options->head-plist opts)
               :hydra/expr (first body)))])))

(defn recursive-wrapper [f]
  (fn [c]
    (update (f c)
            :hydra/wrappers
            (fn [xs] (vec (cons (recursive-wrapper f) xs))))))

(defn head-wrapper [f]
  (recursive-wrapper (fn [x] (if (:hydra/heads x) x (f x)))))

(do :compile

    (defn hydra-path-map [hydras]
      (reduce (fn [ret h] (assoc ret (:hydra/path h) h))
              {} hydras))

    (defn hydra-compile-head
      [{:hydra/keys [key hint expr options body-varsym]}]
      (if expr
        (list* key expr hint options)
        (list key body-varsym hint :column "Sub")))

    (defn hydra-compile-one [path path-map]
      (let [{:as hydra :hydra/keys [heads name options]} (get path-map path)]
        (when heads
          (list* 'defhydra name (options->plist options)
                 (concat (hydra-body-prelude path-map hydra)
                         (map (fn [h] (hydra-compile-head (get path-map (conj path h))))
                              heads))))))

    (defn hydra-compile [spec]
      (let [hydras (parse-hydra {:hydra/path [] :hydra/options default-hydra-options} spec)
            path-map (hydra-path-map hydras)]
        (cons 'progn (keep (fn [[path _]] (hydra-compile-one path path-map)) path-map)))))

(do
  (def sample-hydra-def
    [:root nil
     {:color 'teal
      :doc "this is the root"
      :hydra/wrappers [(head-wrapper (fn [c] (update c :hydra/expr (fn [e] (list 'wrapped e)))))]}
     [:one "a"
      [:a "a" '(message "one a")]
      [:b "b" '(message "one b")]]
     [:two "b"
      {:hydra/wrappers [(fn [c] (assoc c :hydra/q 42))
                        (recursive-wrapper (fn [c] (assoc c :hydra/pouet :naze)))]}
      [:a "a" '(message "two a")]
      [:b "b" '(message "two b")]
      [:three "c"
       {:hint "this will be deep"
        :foo 'bar
        :hydra/wrappers [(fn [c] (update c :hydra/pouet name))]}
       [:deep "d" '(message "deep")]]]])

  '(spit "emacs/compiled/test-hydra.el" (pretty-str (hydra-compile sample-hydra-def)))
  (parse-hydra {:hydra/path [] :hydra/options default-hydra-options}
               sample-hydra-def))
