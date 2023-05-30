(ns noon.utils.fennel
  (:import (java.io ByteArrayOutputStream))
  (:require [bencode.core :as bc]
            [clojure.java.shell :refer [sh]]
            [backtick :refer [template]]
            [clojure.string :as str]))

(defn compile-string [code-string]
  (let [{:keys [out err]}
        (->> (template (let [fennel (require :fennel)
                             (compiled) (fennel.compile-string ~code-string)]
                         compiled))
             (str)
             (sh "fennel" "-e"))]
    (if (= "" err)
      out
      (throw (Exception. err)))))

(defn compile [code]
  (compile-string (str/replace (str code) "," " ")))

(defmacro lua [& forms]
  (compile (list 'do forms)))

(comment
  (compile '(+ 1 2)))
