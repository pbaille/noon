(ns noon.utils.emacs.cider-keybindings
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [noon.utils.misc :as u]))

(defn path->binding-description [p]
  (str/join "-" (map name p)))

(defn compile-cider-map!
  [mode-map-sym tree]
  (let [bindings (u/all-paths tree)]
    (with-out-str
      (clojure.pprint/pprint
       (u/template (map! (:map ~mode-map-sym
                             ~@(mapcat (fn [[path [binding code]]]
                                         (if binding
                                           [:desc (path->binding-description path) :n binding
                                            (u/template (lambda ()
                                                              (interactive)
                                                              (my-cider/eval! (format ~(str (walk/prewalk-replace '{*expr* %s} (list 'do code :ok)))
                                                                                      (pb/thing-at-point)))))]))
                                       bindings))))))))

(defn emit-bindings [filename & keymap-tree-pairs]
  (spit filename
        (str/join "\n"
                  (map (fn [[keymap-sym tree]] (compile-cider-map! keymap-sym tree))
                       (partition 2 keymap-tree-pairs)))))

(comment
  (spit "try-cider-keybindings.el"
        (compile-cider-map!
         'my-first-cider-map
         '{:foo ["a" (+ 1 2)]
           :bar {:qux ["M-w" (println "M-w")]}})))
