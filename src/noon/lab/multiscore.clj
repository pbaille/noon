(ns noon.lab.multiscore
  "Experiment around using multivals and noon.score together."
  (:require [noon.score :as n]
            [noon.utils.multi-val :as m]
            [noon.utils.misc :as u]))

(defmacro multi-update
  {:clj-kondo/ignore true}
  [arg & body]
  `(with-meta
     (fn [~arg] ~@body)
     {:type ::multi-update}))

(defn multi-update? [x]
  (u/t? ::multi-update x))

(def ->score-update (m/lift1 n/->upd))

(defn update-score [score u]
  (if (multi-update? u)
    (u score)
    (m/bind (->score-update u)
            (partial m/bind score))))

(defn chain* [xs]
  (n/sfn score (n/?reduce update-score score xs)))

(defn mk* [xs]
  (update-score n/score0 (chain* xs)))

(defn mk [& xs]
  (mk* xs))

(def collect
  (multi-update multi-score
    (n/concat-scores (m/get-all multi-score))))

(comment
  (defn lin* [x]
    (n/sf_
     (m/bind x (fn [x]
                 (if (sequential? x) (m/fmap (m/tup* (map (partial update-score _) x))
                                             n/concat-scores)
                     (m/once (n/concat-scores (m/get-all (update-score _ x))))))))))

(defn play [& xs]
  (n/noon {:play true}
          (m/get-1 (mk* xs))))

(comment
  (play (n/par n/d0 n/d3 n/d6)
        (m/join n/d1 n/d3 n/d2))

  (play (n/par n/d0 n/d3 n/d6)
        (m/mix n/d1 n/d3 n/d2)))
