(ns noon.score)

(defmacro efn
  "just a tagged lambda that represents an event update function"
  [arg & body]
  `(fn [~arg] ~@body))

(defmacro ef_ [& body]
  `(efn ~'_ ~@body))

(defmacro sfn
  "just a tagged lambda that represents a score update function"
  [arg & body]
  `(fn [~arg] ~@body))

(defmacro sf_ [& body]
  `(sfn ~'_ ~@body))

(defmacro import-wrap-harmony-update-constructors [& xs]
  `(do ~@(mapv (fn [x]
                 (macroexpand `(defn ~x ~'[& _] nil)))
               xs)))

(defmacro import-wrap-harmony-updates [& xs]
  `(do ~@(mapv (fn [x]
                 (macroexpand `(def ~x nil)))
               xs)))
