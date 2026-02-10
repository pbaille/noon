(ns noon.viz.piano-roll-test
  (:require [noon.viz.piano-roll :as pr]
            [noon.eval :refer [score]]
            #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer-macros [deftest testing is]])))

(deftest score->notes-test
  (let [notes (pr/score->notes (score (tup s0 s1 s2)))]
    (testing "returns 3 notes for a triad arpeggio"
      (is (= 3 (count notes))))
    (testing "notes have required keys"
      (is (every? #(and (:position %) (:duration %) (:pitch %) (:kind %)) notes)))
    (testing "first note is tonic"
      (is (= :tonic (:kind (first notes)))))
    (testing "other notes are structural"
      (is (every? #(= :structural (:kind %)) (rest notes))))
    (testing "pitches are MIDI integers"
      (is (every? #(integer? (:pitch %)) notes)))
    (testing "positions are sequential"
      (is (apply < (map :position notes))))))

(deftest score->harmonies-test
  (let [h (pr/score->harmonies (score (lin I IV V I) (each (tup s0 s1 s2))))]
    (testing "4 harmony segments for I-IV-V-I"
      (is (= 4 (count h))))
    (testing "harmonies have position and duration"
      (is (every? #(and (:position %) (:duration %)) h)))
    (testing "harmonies are sequential"
      (is (apply <= (map :position h))))))

(deftest piano-roll-test
  (let [result (pr/piano-roll (score (tup s0 s1 s2)))]
    (testing "returns a hiccup div"
      (is (vector? result))
      (is (= :div (first result))))
    (testing "has kindly metadata"
      (is (= :kind/hiccup (:kindly/kind (meta result)))))
    (testing "contains an SVG element"
      (is (some #(and (vector? %) (= :svg (first %))) (tree-seq vector? rest result))))))

(deftest piano-roll-options-test
  (let [with-title (pr/piano-roll (score (tup s0 s1 s2)) {:title "Test"})
        no-legend  (pr/piano-roll (score (tup s0 s1 s2)) {:show-legend false})
        no-keys    (pr/piano-roll (score (tup s0 s1 s2)) {:show-keyboard false})]
    (testing "title appears in output"
      (is (some #(and (vector? %) (= "Test" (last %))) (tree-seq vector? rest with-title))))
    (testing "no legend omits legend div"
      (is (not (some #(and (vector? %) (= "Tonic" (last %)))
                     (tree-seq vector? rest no-legend)))))
    (testing "no keyboard changes SVG width"
      (let [svg-w (fn [h] (some #(when (and (vector? %) (= :svg (first %)))
                                    (:width (second %)))
                                (tree-seq vector? rest h)))]
        (is (< (svg-w no-keys) (svg-w (pr/piano-roll (score (tup s0 s1 s2))))))))))

(deftest piano-roll-group-test
  (let [result (pr/piano-roll-group
                [{:label "triad" :score (score (tup s0 s1 s2))}
                 {:label "tetrad" :score (score (structure :tetrad) (tup s0 s1 s2 s3))}]
                {:shared-pitch-range true})]
    (testing "returns a hiccup div"
      (is (vector? result))
      (is (= :div (first result))))
    (testing "has kindly metadata"
      (is (= :kind/hiccup (:kindly/kind (meta result)))))
    (testing "contains 2 SVG elements"
      (is (= 2 (count (filter #(and (vector? %) (= :svg (first %)))
                               (tree-seq vector? rest result))))))))
