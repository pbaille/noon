(ns noon.utils.euclidean-sums
  (:require [noon.utils.misc :as u :refer [f_ >_]]
            #_[clojure.core :as c]))

(defn euclidean-sum [n d]
  (>_ (mapv (f_ [(* _ d) n]) (range n))
      (mapv (fn [[n d]] (u/rounded-div n d)) _)
      (conj _ d)
      (partition 2 1 _)
      (mapv (fn [[a b]] (- b a)) _)))

(defn euclidean-sums [resolution]
  (map (fn [size] (euclidean-sum size resolution))
       (range 1 resolution)))

(def prime-euclidean-sums
  (into (sorted-map)
        (map (fn [resolution]
               [resolution
                (keep (fn [size]
                        (if (pos? (rem resolution size))
                          (euclidean-sum size resolution)))
                      (range 2 resolution #_(inc (/ resolution 2))))])
             (range 2 33))))

#_(euclidean-sum 5 12)
