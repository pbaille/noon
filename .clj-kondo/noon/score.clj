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

(defmacro defclosure*
  "Like defclosure but last argument is bound to the variadicaly.
       it defines two functions,
       - one that binds the last argument as to variadic arguments.
       - one (postfixed by *) that takes it as a seq.
       This is somehow analogous to #'list and #'list*"
  [name doc argv & body]
  (let [applied-name (symbol (str name "*"))
        variadic-argv (vec (concat (butlast argv) ['& (last argv)]))]
    `(do (defn ~applied-name ~doc
           ~argv
           ~@body)
         (defn ~name ~doc
           ~variadic-argv
           (~applied-name ~@argv)))))

(defmacro import-wrap-harmony-update-constructors [& xs]
  `(do ~@(mapv (fn [x]
                 (macroexpand `(defn ~x ~'[& _] nil)))
               xs)))

(defmacro import-wrap-harmony-updates [& xs]
  `(do ~@(mapv (fn [x]
                 (macroexpand `(def ~x nil)))
               xs)))
