(ns noon.score-test
  (:require [noon.score :as s]
            [clojure.test :refer [deftest testing is]]
            [noon.utils.pseudo-random :as pr]
            [noon.utils.chance :as g]))

(def E0 s/DEFAULT_EVENT)
(def S0 s/score0)

(defn E0> [& xs]
  (s/?reduce #(%2 %1) E0 xs))

(deftest helpers
  (testing "numeric-updates"

    (is (= ((s/sub 1) 1)
           0))
    (is (= ((s/add 1) 1)
           2))
    (is (= ((s/mul 2) 1)
           2))
    (is (= ((s/div 2) 1)
           1/2))

    (is ((s/eq 1) 1))
    (is (not ((s/eq 1) 0)))
    (is (not ((s/gt 1) 1)))
    (is ((s/gt 1) 2))
    (is ((s/gte 1) 1))
    (is ((s/gte 1) 2))
    (is (not ((s/gte 1) 0)))
    (is ((s/lt 1) 0))
    (is (not ((s/lt 1) 2)))
    (is ((s/lte 1) 1))
    (is (not ((s/lte 1) 2)))
    (is ((s/lte 1) 0))

    (is (= (s/hm* [:a 1 :b 2])
           {:b 2, :a 1}))

    (is (= (s/?reduce + 0 (range 5))
           10))
    (is (nil? (s/?reduce (fn [a e] (if (> e 2) (+ a e)))
                         0 (range 5))))
    (is (= (s/?reduce (fn [a e] (if (> e 2) (+ a e)))
                      0 (range 3 6))
           12))

    (is (= (s/->7bits-natural 1/3)
           0))
    (is (= (s/->7bits-natural 1000)
           127))
    (is (= (s/->7bits-natural -1000)
           0))
    (is (= (s/->7bits-natural 60.2)
           (s/->7bits-natural 59.7)
           (s/->7bits-natural 60)
           60))

    (is (= (s/->4bits-natural 1/3)
           0))
    (is (= (s/->4bits-natural 1000)
           15))
    (is (= (s/->4bits-natural -1000)
           0))

    (is (= (s/->4bits-natural 10.1)
           (s/->4bits-natural 9.7)
           (s/->4bits-natural 10)
           10))))

(deftest event-updates
  (testing "simples"

    (is (= (:duration ((s/dur 2) E0))
           2))
    (is (= (:duration ((s/dur 1/2) E0))
           1/2))

    (is (= (:velocity ((s/vel 23) E0))
           23))
    (is (= (:velocity ((s/vel inc) E0))
           (:velocity ((s/vel+ 1) E0))
           81))
    (is (= (:velocity ((s/vel- 10) E0))
           70))
    (is (= (:velocity ((s/vel (s/mul 2)) E0))
           127))
    (is (zero? (:velocity ((s/vel (s/sub 100)) E0))))

    (is (zero? (:channel E0)))
    (is (= (:channel ((s/chan inc) E0))
           1))
    (is (= (:channel ((s/chan+ 10) E0))
           10))
    (is (= (:channel ((s/chan+ 100) E0))
           15))
    (is (= (:channel ((s/chan- 100) E0))
           0))

    (is (= (:track E0)
           0))
    (is (= (:track ((s/track+ 1) E0))
           1))
    (is (= (:track ((s/track+ 1000) E0))
           1000))
    (is (= (:track ((s/track+ 100000) E0))
           65535))

    (is (= (:voice E0)
           0))
    (is (= (:voice ((s/voice+ 1) E0))
           1))
    (is (= (:voice ((s/voice+ 1000) E0))
           15))
    (is (= (:voice ((s/voice -1) E0))
           0))

    (is (= (:cc ((s/cc :volume 10) E0))
           (:cc ((s/cc "volume" 10) E0))
           (:cc ((s/cc "Volume" 10) E0))
           (:cc ((s/cc 'volume 10) E0))
           (:cc ((s/cc 'Volume 10) E0))
           {7 10}))

    (is (= (:cc ((s/cc :bank-select-1 10) E0))
           {0 10}))

    (is (= (:cc ((s/cc :bank-select-1 (s/mul 100))
                 ((s/cc :bank-select-1 10) E0)))
           {0 127}))

    (is (= (:cc ((s/cc :bank-select-1 (s/mul -100))
                 ((s/cc :bank-select-1 10) E0)))
           {0 0}))

    (is (thrown? Exception (s/cc :vlume 10)))

    (is (= (:patch ((s/patch :vibraphone) E0))
           (:patch ((s/patch :vibe) E0))
           [nil 11]))

    (is (= (:patch ((s/patch :chromaphone/smooth-carillon) E0))
           (:patch ((s/patch :chroma/smooth-carillon) E0))
           [8 1]))

    (is (= (pr/with-rand 0
             (:patch ((s/patch :chromaphone/short) E0)))
           [11 6]))

    (is (= (:patch ((s/patch :truc) E0))
           [nil 56])))

  (testing "aliases"
    (is (= (:velocity (s/vel2 E0))
           21))
    (is (= (:velocity (s/vel12 E0))
           127))
    (is (= (:velocity (s/vel0 E0))
           0))

    (is (= (:duration (s/dur2 E0))
           2))
    (is (= (:duration (s/dur:2 E0))
           1/2))
    (is (= (:duration (s/dur5:2 E0))
           5/2))

    (is (= (:channel (s/chan3 E0))
           3))

    (is (= (:track (s/track12 E0))
           12)))

  (testing "pitch"

    ;; see noon.harmony test for wrapped :pitch transformations testing

    (is (= (E0> s/t3 :pitch)
           (E0> s/s1 s/t3 :pitch)
           {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 3}}))

    (is (= (E0> s/t3- :pitch)
           {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t -3}}))

    (is (= (E0> s/s2 :pitch)
           (E0> s/d1 s/c1 s/s2 :pitch)
           {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2}}))

    (is (= (E0> s/s2- :pitch)
           {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s -2}}))

    (is (= (E0> s/d1 :pitch)
           (E0> s/c1 s/d1 :pitch)
           {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 1}}))

    (is (= (E0> s/d1- :pitch)
           {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d -1}}))

    (is (= (E0> s/c1 :pitch)
           {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 1}}))

    (is (= (E0> s/c1- :pitch)
           {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c -1}}))

    (is (= (E0> s/c1- s/o1 :pitch)
           {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 1, :s 0, :d 0, :c -1}}))

    (is (= (E0> s/s1- s/d2 s/o2- :pitch)
           {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t -2, :s -1, :d 2}}))))


(deftest score

  (is (= (s/score s/score0)
         s/score0))

  (is (s/score? (s/score s/score0)))

  (is (= (s/score E0)
         #{E0}))

  (is (s/score? (s/score E0)))

  (is (= (s/score (g/one-of E0))
         s/score0))

  (is (not (s/score? 23)))
  (is (not (s/score? {:a 1})))

  (testing "views"

    (is (= (s/score-duration S0)
           1))
    (is (= (s/score-duration (s/upd S0 s/dur2))
           2))
    (is (= (s/score-duration (s/mk (s/cat s/s0 s/s2 s/s4)))
           3))

    (is (= (s/score-track-count S0)
           1))
    (is (= (s/score-track-count (s/mk (s/superpose s/track1)))
           2))

    (is (= (s/score-bounds S0 :position)
           [{:position 0, :channel 0, :track 0, :duration 1, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}, :velocity 80, :voice 0, :patch [0 4]}
            {:position 0, :channel 0, :track 0, :duration 1, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}, :velocity 80, :voice 0, :patch [0 4]}]))

    (is (= (s/score-bounds (s/mk (s/cat s/s0 s/s2 s/s4)) :position)
           [{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0}}, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}
            {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 4}}, :voice 0, :duration 1, :position 2, :velocity 80, :track 0}]))

    (is (= (s/score-origin S0)
           0))
    (is (= (s/score-origin (s/upd S0 {:position (s/add 10)}))
           10))

    (is (= (s/pitch-value-bounds S0)
           [60 60]))
    (is (= (s/pitch-value-bounds (s/mk (s/cat s/s0 s/s2 s/s4)))
           [60 76])))

  (testing "transformations"

    (is (= (s/score-duration
            (s/scale-score (s/mk (s/cat s/s0 s/s2 s/s4))
                           1/3))
           1))
    (is (= (s/score-duration
            (s/scale-score (s/shift-score S0 10)
                           1/3))
           11/3))
    (is (= (s/score-origin
            (s/scale-score (s/shift-score S0 10)
                           1/3))
           10/3))
    (is (= (s/shift-score S0 10)
           (s/upd S0 {:position (s/add 10)})))
    (is (let [s0 (s/mk (s/cat s/s0 s/s2 s/s4))
              s1 (s/fit-score s0 E0)
              s2 (s/normalise-score s0)]
          (and (= s1 s2)
               (= 1 (s/score-duration s1))
               (= 0 (s/score-origin s2)))))
    (is (let [s (s/fit-score (s/mk (s/cat s/s0 s/s2 s/s4))
                             {:position 3 :duration 2})]
          (and (= 5 (s/score-duration s))
               (= 3 (s/score-origin s)))))

    (is (let [s (s/concat-score S0 S0)]
          (and (= 2 (s/score-duration s))
               (= 0 (s/score-origin s)))))
    (is (= #{} (s/concat-scores [])))
    (is (= S0 (s/concat-scores [S0])))
    (is (let [s (s/concat-score S0 S0)
              s (s/concat-scores [s s s])]
          (and (= 6 (s/score-duration s))
               (= 0 (s/score-origin s)))))

    (is (= (s/reverse-score (s/mk (s/cat s/s0 s/s2 s/s4)))
           (s/mk (s/cat s/s4 s/s2 s/s0))))

    (is (= (s/numerify-pitches (s/mk (s/cat s/s0 s/s2 s/s4)))
           #{{:patch [0 4], :channel 0, :pitch 60, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}
             {:patch [0 4], :channel 0, :pitch 67, :voice 0, :duration 1, :position 1, :velocity 80, :track 0}
             {:patch [0 4], :channel 0, :pitch 76, :voice 0, :duration 1, :position 2, :velocity 80, :track 0}}))

    (is (= (->> (s/dedupe-patches-and-program-changes (s/mk (s/cat s/s0 s/s2 s/s4)))
                (s/sort-score)
                (map (juxt :position :patch :pc)))
           (list [0 [0 4] nil] [1 nil nil] [2 nil nil])))))
