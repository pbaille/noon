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

(defn harmonic-chunks [s & [strict]]
  (let [e->harmony (fn [e] (dissoc (:pitch e) :position))
        validate (fn [xs]
                   (when strict
                     (assert
                      (every? (fn [[a b]] (<= (:position b) (+ (:position a) (:duration a))))
                              (partition 2 1 xs))
                      "overlapping harmonies"))
                   xs)]
    (->> s
         (sort-by :position)
         (partition-by e->harmony)
         (map (fn [xs]
                (let [pos (:position (first xs))
                      dur (- (n/score-duration (set xs)) pos)]
                  {:position (:position (first xs))
                   :duration dur
                   :harmonic-ctx (e->harmony (first xs))})))
         (validate))))

(defn ->piano-roll
  "Turns a score into a datastructure suitable for elisp piano-roll display.
  X can be a score or something that holds a :score entry in metadata."
  [x]
  (if-let [score (if (n/score? x) x (some-> (meta x) :score))]
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
      :harmony (harmonic-chunks score)})))

(comment
  (use 'noon.score)
  (require '[noon.lib.harmony :as lh])
  (spit "src/noon/doc/sample-pr.el"
        (with-out-str
          (clojure.pprint/pprint
           (->piano-roll
            (mk (cat s0 s2 s1 s3)
                (cat s0 s2)
                ($ (chans o1-
                          (tup _ d1 c1- _))))))))

  (spit "src/noon/doc/sample-pr.el"
        (with-out-str
          (clojure.pprint/pprint
           (->piano-roll
            (mk harmonic-minor
                dur2
                (cat I V IV I)
                (lh/align-contexts :d)
                ($ (chans [o1 (shuftup s0 s1 s2 s4)]
                          (par s0 s1 s2)))))))))
