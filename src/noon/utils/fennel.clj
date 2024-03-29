(ns noon.utils.fennel
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.shell :refer [sh]]
            [noon.utils.misc :refer [template]]
            [clojure.string :as str]))

(def compile-string
  (memoize
   (fn [code-string]
     (let [code-string (str/replace code-string "," " ")
           {:keys [out err]}
           (->> (template (let [fennel (require :fennel)
                                (compiled) (fennel.compile-string ~code-string)]
                            compiled))
                (str)
                (sh "fennel" "-e"))]
       (if (= "" err)
         out
         (throw (Exception. (str [:lua-error err]))))))))

(defn compile [code]
  (compile-string (str code)))

(defmacro lua [& forms]
  (compile (list 'do forms)))

(comment
  (compile '(+ 1 2))
  "there is some bug with comment"
  (spit "comment-bug.lua"
        (compile '(comment (let [s [1 2]]
                             s)))))
