(ns visuals.generate
  "Evaluate article examples and emit JSON for the piano-roll renderer.
   Run with: clojure -M doc/visuals/generate.clj"
  (:require [noon.score :as n]
            [noon.harmonic-context :as hc]
            [noon.eval :refer [score]]
            [clojure.data.json :as json]))

;; ── Helpers ──────────────────────────────────────────────────────

(defn score->notes [score]
  (->> (filter :pitch score)
       (sort-by :position)
       (map (fn [{:as e p :pitch}]
              {:position (float (:position e))
               :duration (float (:duration e))
               :channel  (:channel e)
               :pitch    (hc/hc->chromatic-value p)
               :kind     (cond
                           (hc/tonic-equivalent? p)      "tonic"
                           (hc/structural-equivalent? p)  "structural"
                           (hc/diatonic-equivalent? p)    "diatonic"
                           :else                          "chromatic")}))))

(defn score->harmonies [score]
  (let [event->harmony (fn [e] (dissoc (:pitch e) :position))]
    (->> score
         (filter :pitch)
         (sort-by :position)
         (partition-by event->harmony)
         (map (fn [xs]
                (let [pos (:position (first xs))
                      end (apply max (map #(+ (:position %) (:duration %)) xs))]
                  {:position (float pos)
                   :duration (float (- end pos))}))))))

(defn score->data [s]
  {:notes     (score->notes s)
   :harmonies (score->harmonies s)})

;; ── Article examples ─────────────────────────────────────────────
;; Each key becomes an image file: doc/visuals/images/<key>.png
;;
;; Types:
;;   "single"  — one piano roll with title
;;   "grouped" — multiple sub-rolls stacked
;;     :sharedPitchRange true  → all sub-rolls use the same vertical scale
;;     :sharedPitchRange false → each sub-roll is independently scaled

(def images
  {"steps"
   {:type "grouped"
    :sharedPitchRange false
    :items [{:label "chromatic — (tup c0 c1 c2 c3 c4 c5 c6)"
             :data (score->data (score (tup c0 c1 c2 c3 c4 c5 c6)))}
            {:label "diatonic — (tup d0 d1 d2 d3 d4 d5 d6)"
             :data (score->data (score (tup d0 d1 d2 d3 d4 d5 d6)))}
            {:label "structural — (tup s0 s1 s2 s3)"
             :data (score->data (score (tup s0 s1 s2 s3)))}
            {:label "tonic — (tup t0 t1 t2)"
             :data (score->data (score (tup t0 t1 t2)))}]}

   "scales"
   {:type "grouped"
    :sharedPitchRange true
    :items [{:label "major (default)"
             :data (score->data (score dur:4 (lin d0 d1 d2 d3 d4 d5 d6 d7)))}
            {:label "dorian"
             :data (score->data (score dur:4 (scale :dorian) (lin d0 d1 d2 d3 d4 d5 d6 d7)))}
            {:label "hungarian"
             :data (score->data (score dur:4 (scale :hungarian) (lin d0 d1 d2 d3 d4 d5 d6 d7)))}]}

   "structures"
   {:type "grouped"
    :sharedPitchRange true
    :items [{:label "triad (default) — (tup s0 s1 s2 s3)"
             :data (score->data (score (tup s0 s1 s2 s3)))}
            {:label "tetrad — (structure :tetrad) (tup s0 s1 s2 s3)"
             :data (score->data (score (structure :tetrad) (tup s0 s1 s2 s3)))}]}

   "progression"
   {:type "single"
    :title "(lin I IV V I) (each (tup s0 s1 s2))"
    :data (score->data (score (lin I IV V I) (each (tup s0 s1 s2))))}

   "mixing"
   {:type "single"
    :title "dur:2 (tup s0 s1 s2 s3) (each (tup d1 d1- d0))"
    :data (score->data (score dur:2 (tup s0 s1 s2 s3) (each (tup d1 d1- d0))))}

   "harmonic-minor"
   {:type "single"
    :title "(scale :harmonic-minor) (lin I IV VII I) (each (tup s0 s1 s2))"
    :data (score->data (score (scale :harmonic-minor) (lin I IV VII I) (each (tup s0 s1 s2))))}})

;; ── Write output ─────────────────────────────────────────────────

(let [out-path "doc/visuals/data.json"]
  (spit out-path (json/write-str images))
  (println (str "Wrote " out-path " (" (count images) " visuals)")))
