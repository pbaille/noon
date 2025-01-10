(ns noon.score
  (:require noon.constants))

(defmacro lambda
  [arg & body]
  `(fn [~arg] ~@body))

(defmacro lambda_ [& body]
  `(fn [~'_] ~@body))
