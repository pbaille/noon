(ns noon.utils.mapsets
  (:require [noon.utils.misc :as u :refer [f_ defreduction]]
            [noon.utils.maps :as m]
            [clojure.set :as set]))

(defreduction ++
  [s x]
  (cond (nil? x) s
        (map? x) (conj s x)
        (set? x) (set/union s x)
        (seq? x) (apply ++ s x)
        :else (u/throw* "mapset/++: " x)))

(def mk
  (partial ++ #{}))

(defreduction $
  [s f]
  (mk (map (m/->upd f) s)))

(defmacro $_ [& xs]
  `(fn [this#]
     (-> this#
         ~@(map (f_ `($ (f_ ~_))) xs))))

(defn ->upd
  ([x]
   (cond (fn? x) x
         (map? x) (f_ ($ _ x))
         :else ()))
  ([x & xs]
   (reduce (fn [a b]
             (comp (->upd b) a))
           (->upd x) xs)))

(defreduction upd
  [s f]
  ((->upd f) s))

(defn split
  "split the given set into two using 'check (hash-map check(able))"
  [s x]
  (let [splitted (group-by (f_ (m/match _ x)) s)]
    [(set (get splitted true))
     (set (get splitted false))]))

(defn split-upd
  "use 'check to split the set,
       use 'f to upd the matching subset,
       merge the updated subset into the remaining elements"
  [s check f]
  (let [[s1 s2] (split s check)]
    (into s2 (upd s1 f))))

(defn shrink
  "like filter"
  [s x]
  (set (filter (f_ (m/match _ x)) s)))

(comment :tries2

         (require '[noon.utils.chance :as c])

         (def sampler
           (c/setof (c/mapof (c/bag [:a :b :c :d :e :f])
                             (c/nat -10 10)
                             :size 3)
                    :size 20))

         (-> (sampler)
             ($ (f_ (assoc _ :sum (reduce + (vals _)))))
             (split {:sum pos?}))

         (split (sampler)
                {:a number?})

         (split (sampler)
                {:a 0})

         (-> (sampler)
             (split-upd (f_ (contains? _ :a))
                        ($_ (assoc _ :double-a (* (:a _) 2))))))
