(ns noon.utils.mapsets
  "A utility ns to deal with sets of maps.
   It is a thin layer built on top of `noon.utils.maps`."
  (:require [noon.utils.misc :as u :refer [f_ defreduction]]
            [noon.utils.maps :as m]
            [clojure.set :as set]))

(defreduction ++
  "Merge several mapset(able) things into a mapset.
   Valid arguments are:
   - regular mapsets
   - maps (that will be interpreted as a mapset of 1 element).
   - seqs of mapset(able) that will be merged on after the other using this function.
   - nil, that is equivalent to the empty set."
  [s x]
  (cond (nil? x) s
        (map? x) (conj s x)
        (set? x) (set/union s x)
        (seq? x) (apply ++ s x)
        :else (u/throw* "mapset/++: " x)))

(def mk
  (partial ++ #{}))

(defreduction $
  "Maps the given updates over elements of `s`.
   Each update has to be a valid `noon.utils.maps/->upd` argument."
  [s f]
  (mk (map (m/->upd f) s)))

(defn ->upd
  "Build a mapset update using given arguments.
  arguments have to be either:
  - functions, that will be applied to the received mapset
  - maps, that will be turned into map-updates using `noon.utils.maps/->upd`
    and mapped over the received mapset."
  ([x]
   (cond (fn? x) x
         (map? x) (f_ ($ _ x))
         :else (u/throw* "noon.utils.mapsets/->upd :: bad argument: " x)))
  ([x & xs]
   (reduce (fn [a b]
             (comp (->upd b) a))
           (->upd x) xs)))

(defreduction upd
  "Thread the mapset `s` through given updates."
  [s f]
  ((->upd f) s))

(defn split
  "split mapset `s` according to `x` using the `noon.utils.maps/match` function."
  [s x]
  (let [splitted (group-by (f_ (m/match _ x)) s)]
    [(set (get splitted true))
     (set (get splitted false))]))

(defn split-upd
  "`noon.utils.mapsets/split` mapset `s` using `check`,
   `noon.utils.mapsets/upd` the matching subset using `f`,
    merge the updated subset into the remaining elements using `noon.utils.mapsets/++`."
  [s check f]
  (let [[s1 s2] (split s check)]
    (into s2 (upd s1 f))))

(defn shrink
  "Filters a mapset using the `noon.utils.maps/match` mecanism."
  [s x]
  (set (filter (f_ (m/match _ x)) s)))
