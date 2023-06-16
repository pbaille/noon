(ns noon.utils.cider-keybindings
  (:require [backtick :refer [template]]
            [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn all-paths
      ([m] (all-paths m []))
      ([x at]
       (if (map? x)
         (->> (mapcat (fn [[k v]] (all-paths v [k])) x)
              (map (fn [[p v]] [(concat at p) v])))
         [[at x]])))

(defn path->binding-description [p]
  (str/join "-" (map name p)))

(defn compile-cider-map!
  [mode-map-sym tree]
  (let [bindings (all-paths tree)]
    (with-out-str
      (clojure.pprint/pprint
       (template (map! (:map ~mode-map-sym
                             ~@(mapcat (fn [[path [binding code]]]
                                         (if binding
                                           [:desc (path->binding-description path) :n binding
                                            (template (lambda ()
                                                              (interactive)
                                                              (my-cider/eval! ~(str code))))]))
                                       bindings))))))))

(defn emit-bindings [filename keymap-sym tree]
  (spit filename (compile-cider-map! keymap-sym tree)))

(spit "try-cider-keybindings.el"
      (compile-cider-map!
  'my-first-cider-map
  '{:foo ["a" (+ 1 2)]
    :bar {:qux ["M-w" (println "M-w")]}}))
