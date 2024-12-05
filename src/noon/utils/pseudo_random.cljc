(ns noon.utils.pseudo-random
  "A pseudo random utility namespace.
   When non deterministic functions are used to produce a score,
   it is valuable to be able to capture the seed of randomnes in order to reproduce at a later point."
  (:refer-clojure :exclude [rand rand-nth rand-int shuffle])
  (:require [clojure.core :as core])
  #?(:clj [noon.utils.misc :as u]
     :cljs (:require ["prando" :as Prando]))
  #?(:cljs (:require-macros [noon.utils.pseudo-random :refer [with-rand]])))

(def MAX_LONG #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER))

;; from clojure.data.generators

(def random*
  (atom #?(:clj (java.util.Random.
                 (Math/floor (* (core/rand) MAX_LONG)))
           :cljs (new Prando (Math/floor (* (core/rand) MAX_LONG))))))

#?(:clj (defmacro with-rand [s & exprs]
          `(let [r# ~s]
             (reset! random* (cond (number? r#) (java.util.Random. r#)
                                   (string? r#) (u/unserialize-from-base64 r#)
                                   (instance? java.util.Random r#) r#))
             ~@exprs)))

(defn rand
  "Generate a float between 0 and 1 based on random*"
  (^double []
   #?(:clj (.nextFloat @random*)
      :cljs (.next @random*)))
  (^double [max]
   (* max (rand))))

(defn rand-between
  "Generate a float between min and max based on random*"
  (^double [min max]
   (+ min (rand (- max min)))))


(defn rand-int [max]
  (int (Math/floor (rand max))))

(defn rand-int-between
  "Uniform distribution from lo (inclusive) to hi (exclusive).
   Defaults to range of Java long."
  (^long [] #?(:clj (.nextLong @random*)
               :cljs (.nextInt @random* js/Number.MIN_SAFE_INTEGER js/Number.MAX_SAFE_INTEGER)))
  (^long [lo hi]
   {:pre [(< lo hi)]}
   #?(:clj (clojure.core/long (Math/floor (+ lo (* (.nextDouble @random*) (- hi lo)))))
      :cljs (int (rand-between lo hi)))))

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
  "Shuffle coll based on random*"
  [xs]
  (fisher-yates xs))
