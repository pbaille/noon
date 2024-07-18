(ns noon.utils.chance
  (:refer-clojure :exclude [keep])
  (:require [clojure.walk :as walk]
            [noon.utils.misc :as u]
            [noon.utils.pseudo-random :as pr]))

(do :base

    (defn fn->gen
      [f]
      (with-meta
        (fn self
          ([] (f))
          ([_] (u/throw* "gen arity one is deprecated, use sample instead")))
        {:generator true}))

    (defn- realise-n
      "This could be done more concisely using `clojure.core/repeatedly`
       but it was breaking the pseudo randomness."
      [g n]
      (if-not (zero? n)
        (cons (g) (realise-n g (dec n)))))

    (defn gen?
      [x] (:generator (meta x)))

    (defn realise
      [x] (if (gen? x) (x) x))

    (declare data)

    (defn sample
      [n g] (cond (gen? g) (realise-n g n)
                  (coll? g) (sample n (data g))
                  :else (repeat n g))))

(do :syntax

    (defmacro gen
      [& body]
      `(fn->gen (fn [] ~@body)))

    (defmacro defgen
      ([name return]
       `(def ~name (gen ~return)))
      ([name x & xs]
       (let [[doc [x & xs]] (if (string? x) [x xs] [nil (cons x xs)])
             [attrs body] (if (map? x) [x xs] [nil (cons x xs)])
             arities (if (vector? (first body)) (list body) body)]
         `(defn ~name
            ~@(if doc [doc])
            ~@(if attrs [attrs])
            ~@(map (fn [[argv & body]] `(~argv (gen ~@body))) arities))))))

(do :combine

    (defgen data
      [x]
      (walk/prewalk realise x))

    (defgen $
      [g f]
      (f (realise g)))

    (defn $*
      [g f]
      ($ (data g)
         (partial apply f)))

    (defgen bind
      [g h]
      (realise (h (realise g))))

    (defn bind*
      [g h]
      (bind (data g)
            (partial apply h)))

    (defgen keep
      [g test]
      (let [v (realise g)]
        (if (test v) v (recur))))

    (defgen one-of* [gens]
      (realise (pr/rand-nth gens)))

    (defgen one-of
      [& gens]
      (realise (one-of* gens)))

    (defn weighted
      "takes a map of generator(able)/probability
       ex:
       (weighted {coin 1 \"youpi\" 3})
       returns a generator that has 3/4 chances to return \"youpi\"
       and 1/4 chance to return either true or false "
      [m]
      (let [steps (next (reductions + 0 (vals m)))
            parts (map vector steps (keys m))
            total (last steps)]
        (gen
         (let [t (partial <= (pr/rand total))]
           (->> parts
                (filter (comp t first))
                first peek realise))))))

(do :simple

    (defgen coin
      (pr/rand-nth [true false]))

    (defgen cube
      (pr/rand-nth [1 2 3 4 5 6]))

    (defgen nat
      [min max]
      (pr/rand-nth (range min (inc max))))

    (defgen decimal [min max]
      (+ min (* (pr/rand) (- max min))))

    (defn dice
      [n] (nat 1 n))

    (defgen bag
      [xs] (pr/rand-nth xs)))

(do :collections

    (defgen tup
      [& xs]
      (-> xs vec data realise))

    (defgen collection
      [x & [{:keys [empty min-size max-size size]
             :or {min-size 0 max-size 50}}]]
      (let [size (or size (realise (nat min-size max-size)))]
        (into empty (sample size x))))

    (defmacro defcoll
      "Helper to define collection generators.
       The main point is to handle extra options,
       resulting generators will be callable with or without options,
       options being given as a map or as a flat sequence of key values."
      ([name empty]
       (let [sym (gensym)]
         `(defcoll ~name ~empty ~[sym] ~sym)))
      ([name empty argv gen-expr]
       (let [[opts k v kvs] (repeatedly gensym)]
         `(defn ~name
            (~argv (~name ~@argv {}))
            (~(conj argv opts)
             (collection ~gen-expr (assoc ~opts :empty ~empty)))
            (~(conj argv k v '& kvs)
             (~name ~@argv (apply hash-map ~k ~v ~kvs)))))))

    (defcoll seqof ())
    (defcoll vecof [])
    (defcoll setof #{})
    (defcoll mapof {} [k v] (data [k v]))

    (defgen rep
      [g n]
      (realise (vecof g :size n))))

(comment :interpreter

         (defn compile-expr [x]
           (cond (symbol? x) `(realise ~x)
                 (seq? x) `(realise ~(map compile-expr x))
                 (coll? x) (u/$ x compile-expr)
                 :else x))

         (defmacro expr [x]
           `(gen ~(compile-expr x)))

         (sample 10 (expr (+ (nat 5 7) (nat (dice 6) 7))))

         (sample 10
                 (expr
                  [{:a (vecof coin) :b cube}
                   (+ (nat 5 7) (nat (dice 6) 7))]))

         )

(comment :tries

         (sample 10 (data [coin (nat 0 10)]))

         (frequencies
          (sample 5000
                  (weighted
                   {(bag [8 6 4]) 2
                    coin 1})))

         (sample 50 (bind (rep (dice 12) 3) bag))
         (sample 50 (bind* (rep (dice 12) 3) one-of))

         (sample 10 (keep (dice 10) even?))

         (sample 10 ($ (bag [0 3 6]) inc))

         (sample 10 (bind (bag [0 3 6])
                          (fn [n] (one-of n coin))))

         (sample 20 (data [cube coin {:a (nat 4 9)}]))
         (sample 20 (data coin))

         (sample 20 (seqof coin :max-size 6))
         (sample 10 cube)
         (sample 10 (rep (dice 12) 2))
         (sample 10 (vecof dice :size 4))
         (sample 10 (tup coin (nat 5 10)))
         (sample 10 (mapof (one-of :a :b :c) coin))
         (sample 10 (vecof coin {:max-size 5})))
