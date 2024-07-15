(ns noon.utils.maps
  "A namespace to deal with simple maps (nestable) holding data only,
   nested functions are interpreted as updates not values."
  (:require [noon.utils.misc :as u :refer [f_ defreduction]]
            [noon.utils.chance :as g]))

(declare ++)

(defn value-merge
  "The function used to merge values of maps"
  [x y]
  (cond (nil? y) x
        (g/gen? x) (value-merge (g/realise x) y)
        (g/gen? y) (value-merge x (g/realise y))
        (fn? y) (y x)
        (and (map? y) (or (nil? x) (map? x))) (++ (or x {}) y)
        :else y))

(defreduction ++
  "Deeply merge several maps together using `noon.utils.maps/value-merge`."
  [x y]
  (reduce (fn [m k]
            (let [v (value-merge (get x k) (get y k))]
              (if (nil? v) ;; we dissoc on nil only, false is ok
                (dissoc m k)
                (assoc m k v))))
          {} (into (set (keys x)) (keys y))))

(defn ->upd
  "Turns a function or a map into a map update."
  [x]
  (cond (fn? x) x
        (map? x) (f_ (++ _ x))
        :else (u/throw* "mapset/->hm-upd: " x)))

(defreduction upd
  "Thread `m` through given updates.
Updates has to be valid arguments to the `noon.utils.maps/->upd` function."
  [m f]
  ((->upd f) m))

(defn check
  "Check if the result of updating `m` using `b` is a deeply truthy map."
  [m b]
  (u/deep-check (upd m b)))

(defn match
  "Check if the map `m` is matching with `x`.
   `x` can either be
    - a function that,when applied to `m` can return a deeply truthy map indicating a match,
    - another map from which each subvalues will be match against corresponding subvalues of `m`
      subvalues of `x` can be regular values that will be check for equality
      or functions that will be applyed to corresponding subvalues of `m` in order to determine
      if the match is successful or not."
  [m x]
  (cond (fn? x) (u/deep-check (x m))
        (map? x) (every? (fn [[p v]] (if-let [mv (get-in m p)]
                                      (if (fn? v) (v mv) (= v mv))))
                         (u/hm-leaves x))))

(defn upd_
  "Compose several map updates together."
  [& fs]
  (f_ (apply upd _ fs)))
