(ns noon.utils.maps
  "simple maps (nestable) holding data only,
   nested functions will be interpreted as updates not values"
  (:require [noon.utils.misc :as u :refer [f_ defreduction]]
            [noon.utils.chance :as g]))

(declare ++)

(defn value-merge
  "the function used to merge values of maps"
  [x y]
  (cond (nil? y) x
        (g/gen? x) (value-merge (g/realise x) y)
        (g/gen? y) (value-merge x (g/realise y))
        (fn? y) (y x)
        (and (map? y) (or (nil? x) (map? x))) (++ (or x {}) y)
        :else y))

(defreduction ++
  [x y]
  (reduce (fn [m k]
            (let [v (value-merge (get x k) (get y k))]
              (if (nil? v) ;; we dissoc on nil only, false is ok
                (dissoc m k)
                (assoc m k v))))
          {} (into (set (keys x)) (keys y))))

(defn ->upd
  [x]
  (cond (fn? x) x
        (map? x) (f_ (++ _ x))
        :else (u/throw* "mapset/->hm-upd: " x)))

(defreduction upd
  [m f]
  ((->upd f) m))

(defn check
  [m b]
  (u/deep-check (upd m b)))

(defn match
  [m x]
  (cond (fn? x) (u/deep-check (x m))
        (map? x) (every? (fn [[p v]] (if-let [mv (get-in m p)]
                                      (if (fn? v) (v mv) (= v mv))))
                         (u/hm-leaves x))))

(defn upd_
  [& fs]
  (f_ (apply upd _ fs)))

(comment
  (++ {:a {}} {:a {:b (fnil (partial + 3) 0)}})
  (++ {} {:a {:b (fnil (partial + 3) 0)}})
  ;; the p update returns nil so the key is dissociated
  (upd {:a 1} {:p (f_ _)})
  (upd {:a 1 :p true} {:p not})
  (u/hm-leaves {:a {:b (fnil (partial + 3) 0)}}))
