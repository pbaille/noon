(ns hooks.noon.score
  (:require hooks.noon.constants))

(defmacro lambda
  [arg & body]
  `(fn [~arg] ~@body))

(defmacro lambda_ [& body]
  `(fn [~'_] ~@body))
