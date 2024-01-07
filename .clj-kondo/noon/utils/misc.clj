(ns noon.utils.misc)

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

(defn reduction
  "Turn a binary fn 'f into a variadic function that use 'f and reduce to produce a result,
       shortcircuiting on first nil intermediate result"
  [f]
  (fn [this & xs]
    (reduce (fn [this x]
              (if this
                (f this x)
                (reduced nil)))
            this xs)))

(defmacro defreduction
  [name x & xs]
  (let [[doc [argv & body]]
        (if (string? x) [x xs] [nil (cons x xs)])]
    `(def ~name
       ~@(if doc [doc])
       (reduction (fn ~argv ~@body)))))
