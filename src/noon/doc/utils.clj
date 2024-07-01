(ns noon.doc.utils
  (:require [noon.score :as n]
            [noon.harmony :as h]))

(defn elispify [x]
  (cond
    (map? x) (seq (mapcat (fn [[k v]]
                            [k (elispify v)])
                          x))
    (or (seq? x)
        (vector? x)) (seq (map elispify x))

    (ratio? x) (float x)

    (integer? x) (int x)

    :else x))

(defn ->piano-roll [score]
  (elispify
   {:notes (->> (filter :pitch score)
                (sort-by :position)
                (map (fn [{:as e
                           p :pitch}]
                       (assoc (select-keys e [:position :duration :channel])
                              :pitch (h/hc->chromatic-value p)
                              :kind (cond
                                      (h/tonic-equivalent? p) :tonic
                                      (h/structural-equivalent? p) :structural
                                      (h/diatonic-equivalent? p) :diatonic
                                      :else :chromatic)))))
    :bars (->> score
               (group-by :bar)
               (sort-by key)
               (map (comp set val))
               (map (fn [xs]
                      (let [s (n/score xs)]
                        {:beat 1
                         :length (- (n/score-duration s)
                                    (n/score-origin s))}))))}))

(comment
  (use 'noon.score)
  (spit "src/noon/doc/sample-pr.el"
        (with-out-str
          (clojure.pprint/pprint
           (->piano-roll
            (mk (cat [{:bar 1} (cat s0 s2)]
                     [{:bar 2} (cat s1 s3)])
                (cat s0 [{:bar (add 2)} s2])
                ($ (tup _ d1 c1- _))))))))
