(ns hooks.noon.utils.misc)

(defmacro >_
      "shorthand for (as-> x _ ...)"
      [seed & forms]
      `(as-> ~seed ~'_ ~@forms))

(defmacro f_
  "Unary lambda with threading body.
   arity 1: simple unary lambda with arg bound to _
   arity 2+: shorthand for: (fn [x] (as-> x _ ...))."
  ([ret]
   (macroexpand `(fn [~'_] ~ret)))
  ([x & xs]
   (macroexpand `(fn [x#] (noon.utils.misc/>_ x# ~x ~@xs)))))

(defn parse-defn [[name x & xs]]
  (let [[doc [x & xs]] (if (string? x) [x xs] [nil (cons x xs)])
        [attrs body] (if (map? x) [x xs] [nil (cons x xs)])
        arities (if (vector? (first body)) (list body) body)]
    {:name name
     :doc doc
     :attrs attrs
     :arities arities}))

(defmacro defreduction
  [name x & xs]
  (let [[doc [argv & body]]
        (if (string? x) [x xs] [nil (cons x xs)])]
    `(def ~name
       ~@(if doc [doc])
       (noon.utils.misc/reduction (fn ~argv ~@body)))))

(defmacro defn*
  [& form]
  (let [{:keys [name doc attrs arities]} (parse-defn form)
        applied-name (symbol (str name "*"))
        [argv & body] (first arities)
        variadic-argv (vec (concat (butlast argv) ['& (last argv)]))]
    `(do (defn ~applied-name
           ~@(if doc [doc])
           ~@(if attrs [attrs])
           ~argv
           ~@body)
         (defn ~name
           ~@(if doc [doc])
           ~@(if attrs [attrs])
           ~variadic-argv
           (~applied-name ~@argv)))))
