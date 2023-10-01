(ns noon.utils.emacs.hydra
  (:require [backtick :refer [template]]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

(def hydra-option-keys [:color :pre :post :exit :foreign-keys :bind :hint :timeout])
(def hydra-head-option-keys [:color :exit :bind :column])
(def inherited-options [:color :exit :foreign-keys :timeout])

(def default-hydra-options
  {:color 'teal})

(do :help
    (defn symjoin [sep xs] (->> (map name xs) (str/join sep) symbol))
    (defn pretty-str [x] (with-out-str (pp/pprint x)))
    (defn map->plist [m]
      (mapcat identity m))
    (defn hydra-sym [segments] (symjoin "/" (cons :noon-hydra segments)))
    (defn hydra-body-varsym [segments] (symbol (str "#'" (hydra-sym segments) "/body")))
    (defn hydra-docstring [options] (:hydra/docstring options))
    (defn options->plist [options]
      (map->plist (select-keys options hydra-option-keys)))
    (defn options->head-plist [options]
      (map->plist (merge {:column " "} (select-keys options hydra-head-option-keys))))
    (defn hydra-body-prelude [path-map hydra]
      (concat (when-let [docstring (hydra-docstring hydra)]
                [docstring])
              [(list "q" nil "quit" :exit true :column "Nav")]
              (if-let [parent (get path-map (vec (butlast (:hydra/path hydra))))]
                [(list "ESC" (:hydra/body-varsym parent) (str ".." (clojure.core/name (last (:hydra/path parent)))) :column "Nav")]))))

(defn parse-hydra [{:as parent :hydra/keys [path]} [hydra-name key x & xs]]
  (let [[opts body] (if (map? x) [x xs] [{} (cons x xs)])
        path (conj path hydra-name)
        base {:hydra/name (hydra-sym path)
              :hydra/key key
              :hydra/path path
              :hydra/hint (or (:hint opts) (name hydra-name))}]
    (if (vector? (first body))
      (let [options (merge (select-keys (:hydra/options parent) inherited-options) opts)
            parent (assoc base
                          :hydra/body-varsym (hydra-body-varsym path)
                          :hydra/options options
                          :hydra/docstring (:doc opts)
                          :hydra/heads (map first body))]
        (cons parent (mapcat (partial parse-hydra parent) body)))
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
    (cons 'progn (keep (fn [[path h]] (hydra-compile-one path path-map)) path-map))))

(def sample-hydra-def
  (template
   [:root nil
    {:color teal
     :doc "this is the root"}
    [:one "a"
     [:a "a" (message "one a")]
     [:b "b" (message "one b")]]
    [:two "b" [:a "a" (message "two a")]
     [:b "b" (message "two b")]
     [:three "c" {:foo bar}
      [:deep "d" (message "deep")]]]]))

(parse-hydra {:hydra/path [] :hydra/options default-hydra-options} sample-hydra-def)
(spit "emacs/compiled/test-hydra.el" (pretty-str (hydra-compile sample-hydra-def)))
