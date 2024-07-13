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

(defn parse-defn [[name x & xs]]
  (let [[doc [x & xs]] (if (string? x) [x xs] [nil (cons x xs)])
        [attrs body] (if (map? x) [x xs] [nil (cons x xs)])
        arities (if (vector? (first body)) (list body) body)]
    {:name name
     :doc doc
     :attrs attrs
     :arities arities}))

(defmacro defclosure*
  "Like 'noon.utils/defclosure but last argument is bound variadicaly.
       it defines two functions,
       - one that binds the last ARGV pattern to variadic arguments.
       - one (postfixed by *) that expect it as a seq.
       This is somehow analogous to #'list and #'list*"
  [& form]
  (let [{:keys [name doc attrs arities]} (parse-defn form)
        applied-name (symbol (str name "*"))
        [argv & body] (first arities)
        variadic-argv (vec (concat (butlast argv) ['& (last argv)]))]
    `(do (defclosure ~applied-name
           ~@(if doc [doc])
           ~@(if attrs [attrs])
           ~argv
           ~@body)
         (defn ~name
           ~@(if doc [doc])
           ~@(if attrs [attrs])
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
