(ns noon.utils.multi-val
  (:refer-clojure :exclude [compile cat])
  (:require [clojure.core :as c]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(deftype Pair [car cdr])

(defmacro multi-val [& body]
  `(with-meta
     (fn [] ~@body)
     {:type ::multi-val
      ::multi-val true}))

(defn multi-val? [x]
  (and (fn? x)
       (::multi-val (meta x))))

(def none
  (multi-val nil))

(defn once
  [v]
  (multi-val [v none]))

(defn ->multi-val
  [x]
  (cond
    (multi-val? x) x
    (nil? x) none
    :else (once x)))

(defn step
  [g]
  ((->multi-val g)))

(defn fmap
  [g f]
  (multi-val (if-let [[x k] (step g)]
               [(f x) (fmap k f)])))

(defn such-that
  [g f]
  (multi-val (if-let [[v k] (step g)]
               (if (f v)
                 [v (such-that k f)]
                 (step (such-that k f))))))

(defn join
  {::combinator true}
  ([a b]
   (multi-val (if-let [[v k] (step a)]
                [v (join k b)]
                (step b))))
  ([a b & xs]
   (reduce join (join a b) xs)))

(defn join*
  {::combinator true
   ::stared true}
  [xs]
  (if (seq xs)
    (reduce join xs)
    none))

(defn bind
  [g f]
  (multi-val (if-let [[x k] (step g)]
               (step (join (f x) (bind k f))))))

(def ^{::combinator true} cat* join*)

(def ^{::combinator true} cat join)

(defn branch
  {::combinator true}
  ([g h]
   (multi-val (or (step g)
                  (step h))))
  ([g h & xs]
   (reduce branch (branch g h) xs)))

(defn branch*
  {::combinator true
   ::stared true}
  [xs]
  (if (seq xs)
    (reduce branch xs)
    none))

(defn pair
  {::combinator true}
  [a b]
  (multi-val (if-let [[va ka] (step a)]
               (if-let [[vb kb] (step b)]
                 [(Pair. va vb)
                  (join (pair (once va) kb)
                        (pair ka (once vb))
                        (pair ka kb))]))))

(do :collections

    (defn lst->tup
      ([p] (lst->tup [] p))
      ([acc p]
       (if (instance? Pair p)
         (recur (conj acc (.car p))
                (.cdr p))
         acc)))

    (defn lst*
      {::combinator true
       ::stared true}
      [xs]
      (if-not (seq xs)
        (multi-val [() none])
        (pair (first xs) (lst* (rest xs)))))

    (defn lst
      {::combinator true}
      [& xs]
      (lst* xs))

    (defn tup*
      {::combinator true
       ::stared true}
      [xs]
      (fmap (lst* xs) lst->tup))

    (defn tup
      {::combinator true}
      [& xs]
      (tup* xs))

    (defn pairs
      {::combinator true}
      [& {:as h}]
      (lst* (map (fn [e] (pair (key e) (val e)))
                 (seq h))))

    (defn hmap
      {::combinator true}
      [& {:as h}]
      (fmap (pairs h)
            (fn [l]
              (reduce (fn [m p] (assoc m (.car p) (.cdr p)))
                      {} (lst->tup l))))))

(do :lift

    (defn lift [f]
      (fn [& xs]
        (bind (tup* xs)
              (fn [xs] (apply f xs)))))

    (defn lift-1 [f]
      (fn [x]
        (bind x
              (fn [x] (f x)))))

    (defn lift-2 [f]
      (fn [x y]
        (bind (pair x y)
              (fn [p] (f (.car p) (.cdr p))))))

    (defn lift-3 [f]
      (fn [x y z]
        (bind (tup x y z)
              (fn [[x y z]] (f x y z))))))

(do :utils

    (defn from-seq [s]
      (if (seq s)
        (multi-val [(first s)
                    (from-seq (rest s))])
        none))

    (defn int-range [a b]
      (from-seq (range a (inc b))))

    (defn coll [x]
      (cond (map? x) (hmap x)
            (seq? x) (fmap (tup* x) seq)
            (vector? x) (tup* x)
            (set? x) (fmap (tup* x) set))))

(do :extraction

    (defn consume [n g]
      (loop [ret [] g g n n]
        (if (zero? n)
          [ret g]
          (if-let [[v k] (step g)]
            (recur (conj ret v) k (dec n))))))

    (defn get-1 [g]
      (first (step g)))

    (defn get-all [g]
      (if-let [[u k] (step g)]
        (cons u (get-all k)))))

(do :print

    (defmethod print-method Pair [x w]
      (print-method (list (.car x) '. (.cdr x)) w))

    (defn preview [g max]
      (if-let [[firsts _] (consume max g)]
        (str (str/join ", " firsts) "...")
        (str (str/join ", " (get-all g)))))

    (defmethod print-method ::multi-val [g w]
      (print-method (symbol (str "<[multi-val: " (preview g 4) "]>"))
                    w)))

(do :non-deterministic

    (defn- safe-shuffle
      "like clojure.core/shuffle but returns nil if given an empty seq or nil"
      [xs]
      (and (seq xs) (shuffle xs)))

    (defn- first-that [f xs]
      (if-let [[x & xs] (seq xs)]
        (if (f x)
          x
          (first-that f xs))))

    (defn mix* [xs]
      (multi-val
       (if-let [[x & xs] (safe-shuffle xs)]
         (if-let [[v k] (step x)]
           [v (mix* (cons k xs))]
           (step (mix* xs))))))

    (defn mix [& xs]
      (mix* xs))

    (defn int-between [a b]
      (mix* (range a (inc b))))

    (def coin
      (mix true false))

    (defn probs
      "takes a map of multi-val -> probability
   ex:
   (probs {(one-of true false) 1 \"youpi\" 3})
   returns a multi-val that has 3/4 chances to return \"youpi\"
   and 1/4 chance to return either true or false "
      [m]
      (if (empty? m)
        none
        (multi-val
         (let [steps (next (reductions + 0 (vals m)))
               parts (map vector steps (keys m))
               t (partial <= (rand (last steps)))
               g (some-> (first-that (comp t first) parts) peek)]
           (if-let [[v k] (step g)]
             [v (probs (-> (dissoc m g) (assoc k (get m g))))]
             (step (probs (dissoc m g)))))))))

:macros
(letfn [(parse [[x & xs]]
          (let [[fname [x & xs]] (if (symbol? x) [x xs] [nil (cons x xs)])
                [meta [x & xs]] (cond (map? x) [x xs] (string? x) [{:doc x} xs] :else [nil (cons x xs)])
                arities (if (vector? x) (list (cons x xs)) (cons x xs))]
            {:name fname
             :meta meta
             :arities arities
             :variadic? (some (fn [argv] (-> argv butlast last (= '&) boolean))
                              (map first arities))}))

        (no-main-binding-error [binding]
          (throw (Exception. (str `binding-error
                                  "\n destructured bindings should have a main binding (:as)\n"
                                  binding))))

        (main-binding [binding]
          (cond (symbol? binding) binding
                (map? binding) (or (:as binding) (no-main-binding-error binding))
                (vector? binding) (let [[a b] (take-last 2 binding)]
                                    (or (and (= :as a) (main-binding b))
                                        (no-main-binding-error binding)))))

        (compile-fixed-arity [[argv & body]]
          `(~argv
            (bind (tup* ~(mapv main-binding argv))
                  (fn [~argv] ~@body))))

        (compile-star-arity [[argv & body]]
          `(~argv
            (bind (tup* ~(conj (mapv main-binding (butlast argv)) `(tup* ~(main-binding (last argv)))))
                  (fn [~argv] ~@body))))

        (variadify-argv [argv]
          (vec (concat (butlast argv)
                       (list '& (last argv)))))]

  (defmacro bind-fn [& body]
    (let [{:keys [name meta arities variadic?]} (parse body)
          _ (assert (not variadic?)
                    "variadism not supported here")
          decl `(fn ~@(if name [name])
                  ~@(map compile-fixed-arity arities))]
      (if meta
        `(with-meta ~decl ~meta)
        decl)))

  (defmacro lift1 [fsym]
    `(bind-fn [x#] (~fsym x#)))

  (defmacro lift2 [fsym]
    `(bind-fn [x# y#] (~fsym x# y#)))

  (defmacro lift3 [fsym]
    `(bind-fn [x# y# z#] (~fsym x# y# z#)))

  (defmacro defbind [& body]
    (let [{:keys [name meta arities]} (parse body)]
      `(defn ~name
         ~@(if meta [meta])
         ~@(map compile-fixed-arity arities))))

  (defmacro defbind* [& body]
    (let [{:keys [name meta arities]} (parse body)
          _ (assert (= 1 (count arities))
                    (str `defbind* " only accept one arity definition."))
          star-name (symbol (str name "*"))
          argv (ffirst arities)]
      `(do (defn ~star-name
             ~@(if meta [meta])
             ~(compile-star-arity (first arities)))
           (defn ~name
             ~@(if meta [meta])
             ~(variadify-argv argv)
             (~star-name
              ~@(map main-binding argv))))))

  (defmacro with [bindings & body]
    (if-let [[binding expression & bindings] (seq bindings)]
      `(bind ~expression
             (fn [~binding] (with ~(vec bindings) ~@body)))
      `(do ~@body)))

  (defn compile-bif-step
    [[pat expr] return exit]
    `(multi-val
      (step
       (if-let [[v# k#] (step ~expr)]
         (bind (cat (once v#) k#)
               (fn [~pat] ~return))
         ~exit))))

  (defmacro bif
    "cond-let like macro for multi-vals.
     the first branch which bindings succeed at least once is taken."
    ([] nil)
    ([then] then)
    ([bs then] `(with ~bs ~then))
    ([bs then x & xs]
     (let [couples (partition 2 bs)
           pattern (mapv first couples)
           else (list* `bif x xs)]
       (compile-bif-step [pattern (list `with bs (mapv main-binding pattern))]
                         then else)))))

(do :eval-xp

    (defn prob [& xs]
      (mapv println xs)
      (last xs))

    (defn value-kind [env s]
      (let [{::keys [combinator stared]} (when (symbol? s) (-> (resolve s) meta))]
        (cond stared :stared-combinator
              combinator :combinator
              (get env s) :local
              :else :value)))

    (declare compile)

    (defn compile-application [env [verb & args :as expr]]
      (let [argv (vec (repeatedly (count args) gensym))]
        `(bind (tup ~@(mapv (partial compile env) args))
               (fn [~argv] ~(cons (compile env verb) argv)))))

    (defn compile-let* [env [_ bindings & body]]
      (let [[env bindings]
            (reduce (fn [[env bindings] [s e]]
                         [(assoc env s :local)
                          (conj bindings s (compile env e))])
                       [env []]
                       (partition 2 bindings))]
        (concat (list 'let* bindings)
                (mapv (partial compile env) body))))

    (defn compile-fn* [env [_ & arities]]
      (cons 'fn* (mapv (fn [[argv & body]] (cons argv (mapv (partial compile env) body)))
                       arities)))

    (defn compile [env x]
      (cond (seq? x) (let [[verb & args] x]
                       (case (value-kind env verb)
                         (:local :combinator) (cons verb (mapv (partial compile env) args))
                         :stared-combinator (throw (Exception. (str "not supported " x)))
                         (condp = verb
                           'fn* (compile-fn* env x)
                           'let* (compile-let* env x)
                           (compile-application env x))))
            (coll? x) (cond (vector? x) (compile env `(vector ~@x))
                            (map? x) (compile env `(hash-map ~@(mapcat identity x)))
                            (set? x) (compile env `(hash-set ~@x)))
            :else x))

    (defmacro meval [code]
      (compile {} (walk/macroexpand-all code)))

    (meval (let [a 1
                 b (join 2 3 4)
                 c (+ b 1)
                 f (fn [x y] (+ b x y))]
             (f a (join 4 8))))

    )

(comment :tries

         (do :expressions

             (mix 1 2)
             (int-between 0 34)

             (get-all (mix 1 4 (mix 3 9)))

             (get-all  (probs {(mix true false) 1 "youpi" 3}))

             (get-all  (tup 4 (mix true false)
                            (mix 9 8)))

             (-> (fmap (int-between 0 10) dec)
                 (such-that pos?)
                 (get-1))

             (get-all  (bind (int-between 3 7)
                             (fn [n] (int-between 0 n))))

             (hmap :a coin :b coin {:c (int-between 4 9)})
             (get-all (tup coin 1 2 (int-between 4 6)
                           (hmap :a coin :b (mix 6 9))))
             (get-all ((lift (fn [a b] (if (= a b) [a b]))) (int-between 0 10) (int-between 0 10)))

             (do (get-all (lst (int-range 0 2) (int-range 0 2) (int-range 0 2)))
                 (get-all (tuple (int-between 0 2) (int-between 0 2)))
                 (get-all (hmap :a (int-between 3 8) {:b (from-seq (range 0 4))}))))

         (do :macros

             (defbind plus [x y]
               (+ x y))

             (defbind* sum [xs]
               (reduce + 0 xs))

             (sum 1 (mix 2 4) 3)
             (sum* [1 (mix 2 4) 3])

             (println "ui")

             (get-all  (plus (mix 1 2) (mix 4 5)))

             (plus (from-seq (range)) none)

             (defbind tup2 [a b]
               (mix [a b] [b a]))

             (get-all (tup2 (mix 1 2) (mix 4 5)))

             (get-all (with [a (int-between 10 20)
                             b (mix 1 a)]
                            (+ a b)))

             (get-all (with [a none
                             b (mix 1 a)]
                            (+ a b)))

             (get-all (bif [a none
                            b (mix 1 a)] (+ a b)
                           [x coin] (tup x coin)))

             (get-all (bif [a 9
                            b (mix 1 a)] (+ a b)
                           coin))

             (get-all (bif [a coin]
                           (if (int? a) a)
                           :ok))

             (get-all (bif [a (mix 1 2)
                            b (mix 1 2)
                            n none]
                           (if (= 5 (+ a b)) :OK)
                           coin))

             (get-all (bif [a (mix 1 2)
                            b (mix 1 2)
                            n none]
                           (if (= 5 (+ a b)) :OK)
                           coin))

             (get-all (bif [a (mix 1 2)
                            b (mix 1 2)]
                           (if (= 5 (+ a b)) :OK)
                           coin))

             (get-all (bif [a (mix 1 2 4)
                            b (mix 0 a)]
                           (if (= 2 (+ a b)) [:OK a b])
                           coin))

             (get-all (bif [a (mix 1 2 4)
                            b (mix 0 a)]
                           (if (= 2 (+ a b)) [a b])
                           coin))

             (bind coin (fn [a] (int? a)))

             (get-all (branch none coin))
             (get-all (branch coin none))
             (get-all (branch (int-between 0 10) coin))
             (get-all (branch coin (int-between 0 10)))

             ()))

(comment :deprecated
         (defn tuple* [xs]
           (if-not (seq xs)
             (multi-val [() none])
             (multi-val (let [[x & xs] xs]
                          (if-let [[v k] (step x)]
                            (step (cat (tuple* (cons k xs))
                                       (fmap (tuple* xs) (partial cons v)))))))))

         (defn tuple [& xs]
           (tuple* xs))d)
