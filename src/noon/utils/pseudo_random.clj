(ns noon.utils.pseudo-random
  (:refer-clojure :exclude [rand rand-nth rand-int shuffle])
  (:require [noon.utils.misc :as u]
            [clojure.core :as core]))

;; from clojure.data.generators

(def ^:dynamic ^java.util.Random
  *rnd*
  (java.util.Random. (Math/floor (* (core/rand) Long/MAX_VALUE))))

(defmacro with-rand [s & exprs]
  `(let [r# ~s]
     (binding [~`*rnd* (cond (number? r#) (java.util.Random. r#)
                             (string? r#) (u/unserialize-from-base64 r#)
                             (instance? java.util.Random r#) r#)]
       ~@exprs)))

(defn rand
  "Generate a float between 0 and 1 based on *rnd*"
  (^double []
   (.nextFloat *rnd*))
  (^double [max]
   (* max (rand))))

(defn rand-int [max]
  (Math/floor (rand max)))

(defn rand-int-between
  "Uniform distribution from lo (inclusive) to hi (exclusive).
   Defaults to range of Java long."
  (^long [] (.nextLong *rnd*))
  (^long [lo hi] {:pre [(< lo hi)]}
                 (clojure.core/long (Math/floor (+ lo (* (.nextDouble *rnd*) (- hi lo)))))))

(defn rand-nth [xs]
  (nth xs (rand-int-between 0 (count xs))))

(defn ^:private fisher-yates
  "http://en.wikipedia.org/wiki/Fisher–Yates_shuffle#The_modern_algorithm"
  [xs]
  (let [as (object-array xs)]
    (loop [i (dec (count as))]
      (if (<= 1 i)
        (let [j (rand-int-between 0 (inc i))
              t (aget as i)]
          (aset as i (aget as j))
          (aset as j t)
          (recur (dec i)))
        (vec as)))))

(defn shuffle
  "Shuffle coll based on *rnd*"
  [xs]
  (fisher-yates xs))

(comment
  (binding [*rnd* (java.util.Random. 42)]
    (vec (repeatedly 3 rand)))
  (with-rand 42
    (vec (repeatedly 10 #(rand-nth (range 10)))))
  (with-rand 42
    (shuffle [1 2 3 4]))
  (do *rnd*)
  (with-rand 43
    (vec (repeatedly 10 #(shuffle [1 2 3 4])))))
