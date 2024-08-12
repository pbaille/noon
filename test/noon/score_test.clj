(ns noon.score-test
  (:use noon.score)
  (:require [clojure.test :as t :refer [deftest testing is]]
            [noon.utils.pseudo-random :as pr]
            [noon.utils.chance :as g]
            [noon.utils.misc :as u]
            [noon.test :as tu]))

(def event0 DEFAULT_EVENT)
(def S0 score0)

(defn e0> [& xs]
  (?reduce #(%2 %1) event0 xs))

(deftest helpers
  (testing "numeric-updates"

    (is (= ((sub 1) 1)
           0))
    (is (= ((add 1) 1)
           2))
    (is (= ((mul 2) 1)
           2))
    (is (= ((div 2) 1)
           1/2))

    (is ((eq 1) 1))
    (is (not ((eq 1) 0)))
    (is (not ((gt 1) 1)))
    (is ((gt 1) 2))
    (is ((gte 1) 1))
    (is ((gte 1) 2))
    (is (not ((gte 1) 0)))
    (is ((lt 1) 0))
    (is (not ((lt 1) 2)))
    (is ((lte 1) 1))
    (is (not ((lte 1) 2)))
    (is ((lte 1) 0))

    (is (= (?reduce + 0 (range 5))
           10))
    (is (nil? (?reduce (fn [a e] (if (> e 2) (+ a e)))
                       0 (range 5))))
    (is (= (?reduce (fn [a e] (if (> e 2) (+ a e)))
                    0 (range 3 6))
           12))

    (is (= (->7bits-natural 1/3)
           0))
    (is (= (->7bits-natural 1000)
           127))
    (is (= (->7bits-natural -1000)
           0))
    (is (= (->7bits-natural 60.2)
           (->7bits-natural 59.7)
           (->7bits-natural 60)
           60))

    (is (= (->4bits-natural 1/3)
           0))
    (is (= (->4bits-natural 1000)
           15))
    (is (= (->4bits-natural -1000)
           0))

    (is (= (->4bits-natural 10.1)
           (->4bits-natural 9.7)
           (->4bits-natural 10)
           10))))

(deftest midi-values
  (is (= 0
         (midi-val 0)
         (midi-val -1)
         (midi-val :min)
         (midi-val 0.0)
         (midi-val 0/1)
         (midi-val -1/2)))
  (is (= 127
         (midi-val 127)
         (midi-val 1270)
         (midi-val 1.0)
         (midi-val 1.5)
         (midi-val 3/2)
         (midi-val :max)))
  (is (= 64
         (midi-val 64)
         (midi-val 1/2)
         (midi-val 0.5)))

  (is (= (pr/with-rand 0 (take 100 (iterate
                                    (humanize :max-step 1/10 :bounds [20 80])
                                    60)))
         (list 60 63 59 61 62 64 61 59 65 70 76 72 67 62 56 57 63 58 60 58 62 68 67 70 73 77 79 76
               79 73 67 73 76 78 73 72 66 60 58 56 61 67 72 74 70 66 71 65 68 72 75 71 75 80 74 72 77 79 80
               79 74 80 78 77 79 76 74 79 76 80 79 75 69 63 57 56 62 68 72 73 70 67 72 66 60 58 53 58 52 49 50
               56 53 58 60 65 59 61 60 61))))

(deftest event-updates

  (testing "basics"

    (is (= ((efn e e)
            DEFAULT_EVENT)
           ((ef_ _)
            DEFAULT_EVENT)
           DEFAULT_EVENT))

    (is (event-update? (ef_ _)))
    (is (event-update? (efn e e)))

    (is (not (event-update? (fn [x] x))))

    (let [u (map->efn {:position inc})
          v (map->efn {:position 1})]
      (and (is (event-update? u))
           (is (event-update? v))
           (is (= (u DEFAULT_EVENT)
                  (v DEFAULT_EVENT)
                  (assoc DEFAULT_EVENT :position 1)))))

    (testing "->event-update"

      (let [a (->event-update {:position inc})
            b (->event-update (map->efn {:position inc}))
            c (->event-update {:duration (div 2)})
            d (->event-update [{:duration (div 2)} (map->efn {:position inc})])]
        (is (and a b c d))
        (is (= (a DEFAULT_EVENT)
               (b DEFAULT_EVENT)
               (assoc DEFAULT_EVENT :position 1)))
        (is (= (d DEFAULT_EVENT)
               (-> DEFAULT_EVENT (a) (c))
               (assoc DEFAULT_EVENT :position 1 :duration 1/2)))))

    (testing "event-matcher"

      (is (event-matcher? (event-matcher (fn [_] true))))

      (let [m1-1 (->event-matcher chan0)
            m1-2 (->event-matcher {:channel 0})
            m2-1 (->event-matcher chan2)
            m2-2 (->event-matcher {:channel 2})
            m3-1 (->event-matcher (fn [e] (zero? (:position e))))
            m3-2 (->event-matcher (fn [e] (pos? (:position e))))
            m4-1 (->event-matcher (fn [e] {:a (zero? (:position e))
                                           :b (pos? (:duration e))}))
            m4-2 (->event-matcher (fn [e] {:a (zero? (:position e))
                                           :b (zero? (:duration e))}))]

        (is (and (m1-1 DEFAULT_EVENT)
                 (m1-2 DEFAULT_EVENT)
                 (m3-1 DEFAULT_EVENT)
                 (m4-1 DEFAULT_EVENT)))
        (is (not (or (m2-1 DEFAULT_EVENT)
                     (m2-2 DEFAULT_EVENT)
                     (m3-2 DEFAULT_EVENT)
                     (m4-2 DEFAULT_EVENT)))))))

  (testing "simples"

    (is (= (:duration ((dur 2) event0))
           2))
    (is (= (:duration ((dur 1/2) event0))
           1/2))

    (is (= (:velocity ((vel 23) event0))
           23))
    (is (= (:velocity ((vel inc) event0))
           (:velocity ((vel+ 1) event0))
           81))
    (is (= (:velocity ((vel- 10) event0))
           70))
    (is (= (:velocity ((vel (mul 2)) event0))
           127))
    (is (zero? (:velocity ((vel (sub 100)) event0))))

    (is (zero? (:channel event0)))
    (is (= (:channel ((chan inc) event0))
           1))
    (is (= (:channel ((chan+ 10) event0))
           10))
    (is (= (:channel ((chan+ 100) event0))
           15))
    (is (= (:channel ((chan- 100) event0))
           0))

    (is (= (:track event0)
           0))
    (is (= (:track ((track+ 1) event0))
           1))
    (is (= (:track ((track+ 1000) event0))
           1000))
    (is (= (:track ((track+ 100000) event0))
           65535))

    (is (= (:voice event0)
           0))
    (is (= (:voice ((voice+ 1) event0))
           1))
    (is (= (:voice ((voice+ 1000) event0))
           15))
    (is (= (:voice ((voice -1) event0))
           0))

    (is (= (:cc ((cc :volume 10) event0))
           (:cc ((cc "volume" 10) event0))
           (:cc ((cc "Volume" 10) event0))
           (:cc ((cc 'volume 10) event0))
           (:cc ((cc 'Volume 10) event0))
           {7 10}))

    (is (= (:cc ((cc :volume [50 100]) event0))
           {7 [50 100]}))
    (is (= (:cc ((cc :volume [-50 80 150]) event0))
           {7 [0 80 127]}))

    (is (= (:cc ((cc :bank-select-1 10) event0))
           {0 10}))

    (is (= (:cc ((cc :bank-select-1 (mul 100))
                 ((cc :bank-select-1 10) event0)))
           {0 127}))

    (is (= (:cc ((cc :bank-select-1 (mul -100))
                 ((cc :bank-select-1 10) event0)))
           {0 0}))

    (is (thrown? Exception (cc :vlume 10)))

    (is (= (:patch ((patch :vibraphone) event0))
           (:patch ((patch :vibe) event0))
           [nil 11]))

    (is (= (:patch ((patch :chromaphone/smooth-carillon) event0))
           (:patch ((patch :chroma/smooth-carillon) event0))
           [8 1]))

    (is (= (pr/with-rand 0
             (:patch ((patch :chromaphone/short) event0)))
           [11 6]))

    (is (= (:patch ((patch :truc) event0))
           [nil 56])))

  (testing "aliases"

    (is (= (:velocity (vel2 event0))
           21))
    (is (= (:velocity (vel12 event0))
           127))
    (is (= (:velocity (vel0 event0))
           0))

    (is (= (:duration (dur2 event0))
           2))
    (is (= (:duration (dur:2 event0))
           1/2))
    (is (= (:duration (dur5:2 event0))
           5/2))

    (is (= (:channel (chan3 event0))
           3))

    (is (= (:track (track12 event0))
           12)))

  (testing "pitch"

    ;; see noon.harmony test for wrapped :pitch transformations testing

    (is (= (e0> t3 :pitch)
           (e0> s1 t3 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 3}}))

    (is (= (e0> t3- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t -3}}))

    (is (= (e0> s2 :pitch)
           (e0> d1 c1 s2 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2}}))

    (is (= (e0> s2- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s -2}}))

    (is (= (e0> d1 :pitch)
           (e0> c1 d1 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 1}}))

    (is (= (e0> d1- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d -1}}))

    (is (= (e0> c1 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 1}}))

    (is (= (e0> c1- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c -1}}))

    (is (= (e0> c1- o1 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 1, :s 0, :d 0, :c -1}}))

    (is (= (e0> s1- d2 o2- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t -2, :s -1, :d 2}}))))

(deftest score-test

  (testing "basic"

    (is (= (score score0)
           score0))

    (is (score? (score score0)))

    (is (= (score event0)
           #{event0}))

    (is (score? (score event0)))

    (is (= (score (g/one-of event0))
           score0))

    (is (not (score? 23)))
    (is (not (score? {:a 1}))))

  (testing "views"

    (is (= (score-duration S0)
           1))
    (is (= (score-duration (update-score S0 dur2))
           2))
    (is (= (score-duration (mk (lin s0 s2 s4)))
           3))

    (is (= (score-track-count S0)
           1))
    (is (= (score-track-count (mk (superpose track1)))
           2))

    (is (= (score-bounds S0 :position)
           [{:position 0, :channel 0, :track 0, :duration 1, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}, :velocity 80, :voice 0, :patch [0 4]}
            {:position 0, :channel 0, :track 0, :duration 1, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}, :velocity 80, :voice 0, :patch [0 4]}]))

    (is (= (score-bounds (mk (lin s0 s2 s4)) :position)
           [{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0}}, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}
            {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 4}}, :voice 0, :duration 1, :position 2, :velocity 80, :track 0}]))

    (is (= (score-origin S0)
           0))
    (is (= (score-origin (update-score S0 {:position (add 10)}))
           10))

    (is (= (pitch-value-bounds S0)
           [60 60]))
    (is (= (pitch-value-bounds (mk (lin s0 s2 s4)))
           [60 76])))

  (testing "transformations"

    (testing "map-event-update"

      (is (= (map-event-update (mk (lin d0 d1 d2)) d1)
             (mk (lin d1 d2 d3))))

      (is (= (map-event-update (mk (lin d0 d1 d2)) vel4)
             (mk vel4 (lin d0 d1 d2))))

      (testing "can remove events by returning nil"

        (is (= (count (map-event-update (mk (lin d0 d1 d2))
                                        (fn [e] (if (not (zero? (pitch-class-value e))) e))))
               2))))

    (testing "scale score, shift score"

      (is (= (score-duration
              (scale-score (mk (lin s0 s2 s4))
                           1/3))
             1))
      (is (= (score-duration
              (scale-score (shift-score S0 10)
                           1/3))
             11/3))

      (is (= (score-origin
              (scale-score (shift-score S0 10)
                           1/3))
             10/3))

      (is (= (shift-score S0 10)
             (update-score S0 {:position (add 10)}))))

    (testing "fit-score, normalise-score"

      (is (let [s0 (mk (lin s0 s2 s4))
                s1 (fit-score s0 event0)
                s2 (normalise-score s0)]
            (and (= s1 s2)
                 (= 1 (score-duration s1))
                 (= 0 (score-origin s2)))))

      (is (let [s (fit-score (mk (lin s0 s2 s4))
                             {:position 3 :duration 2})]
            (and (= 5 (score-duration s))
                 (= 3 (score-origin s))))))

    (is (= (reverse-score (mk (lin s0 s2 s4)))
           (mk (lin s4 s2 s0))))

    (testing "filter-score"

      (is (= (mk (lin d0 d1))
             (filter-score (mk (lin d0 d1)) map?)))

      (is (= (mk (lin d0 d1))
             (filter-score (mk (lin d0 d1)) {:channel 0})
             (filter-score (mk (lin d0 d1)) chan0)))

      (is (= (filter-score (mk (chans (lin d0 d1)
                                      (lin d3 d4)))
                           chan0)
             (mk (lin d0 d1)))))

    (testing "midi prepare helpers"

      (is (= (numerify-pitches (mk (lin s0 s2 s4)))
             #{{:patch [0 4], :channel 0, :pitch 60, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}
               {:patch [0 4], :channel 0, :pitch 67, :voice 0, :duration 1, :position 1, :velocity 80, :track 0}
               {:patch [0 4], :channel 0, :pitch 76, :voice 0, :duration 1, :position 2, :velocity 80, :track 0}}))

      (is (= (->> (dedupe-patches-and-control-changes (mk (lin s0 s2 s4)))
                  (sort-score)
                  (map (juxt :position :patch :cc)))
             (list [0 [0 4] nil] [1 nil nil] [2 nil nil])))

      (is (= (->> (mk (lin s0
                           [(patch :vibraphone)
                            (cc :volume 70)
                            (lin s2 s4)]))
                  (dedupe-patches-and-control-changes)
                  (sort-score)
                  (map (juxt :position :patch :cc)))
             (list [0 [0 4] nil] [1 [nil 11] {7 70}] [2 nil nil])))))

  (testing "composition"

    (is (let [s (concat-score S0 S0)]
          (and (= 2 (score-duration s))
               (= 0 (score-origin s)))))

    (is (= #{} (concat-scores [])))

    (is (= S0 (concat-scores [S0])))

    (is (let [s (concat-score S0 S0)
              s (concat-scores [s s s])]
          (and (= 6 (score-duration s))
               (= 0 (score-origin s)))))

    (is (= (merge-scores [(mk (lin d0 d1 d2))
                          (mk (lin d1 d2 d3))])
           (mk (lin (par d0 d1) (par d1 d2) (par d2 d3))))))

  (testing "updates"

    (testing "basics"

      (is (u/t? :score-update (sf_ _)))

      (is (and (score-update? (sfn s s))
               (score-update? (sf_ _))))

      (is (not (score-update? (fn [s] s)))))

    (testing "->score-update"

      (let [id1 (->score-update (ef_ _))
            id2 (->score-update (sf_ _))
            id3 (->score-update [])
            id4 (->score-update [(ef_ _)])
            id5 (->score-update [(sf_ _) (sf_ _)])
            id6 (->score-update (g/one-of (sf_ _) (ef_ _)))]
        (is (= score0
               (update-score score0 id1)
               (update-score score0 id2)
               (update-score score0 id3)
               (update-score score0 id4)
               (update-score score0 id5)
               (update-score score0 id6))))

      (is (not (->score-update 0)))
      (is (not (->score-update (fn [x] x))))

      (is (= ((->score-update {:channel 1})
              score0)
             #{(assoc DEFAULT_EVENT :channel 1)})))

    (testing "->score-checker"

      (is (= score0
             ((->score-checker (fn [s] (<= (score-duration s) 1)))
              score0)))

      (is (nil?
           ((->score-checker (fn [s] (< (score-duration s) 1)))
            score0)))

      (is (nil? ((->score-checker {:position (gt 0)})
                 score0)))

      (is (= score0
             ((->score-checker {:position (gte 0)})
              score0))))

    (testing "chain-score-updates"

      (is (= score0
             (update-score score0 (chain-score-updates [(sf_ _) (sf_ _)]))))

      (is (= #{(assoc DEFAULT_EVENT :channel 5)}
             (update-score score0 (chain-score-updates [(->score-update {:channel 3})
                                                        (->score-update {:channel inc})
                                                        (->score-update {:channel inc})]))))

      (is (thrown? Exception (chain-score-updates [1]))))

    (testing "map-score-update"

      (is (= (map-score-update (mk (lin d0 d1 d2))
                               same)
             (mk (lin d0 d1 d2))))

      (is (= (map-score-update (mk (lin d0 d1 d2))
                               (tup d0 d3))
             (mk (lin (tup d0 d3)
                      (tup d1 d4)
                      (tup d2 d5))))))

    (testing "update-score"

      (is (= (update-score S0 (sf_ _))
             S0))

      (is (= (update-score S0 dur2)
             (mk dur2)))

      (is (= (update-score S0 [vel10 dur2])
             (mk dur2 vel10)))

      (is (= (update-score S0 (g/one-of vel10))
             (mk vel10)))

      (is (= (update-score S0 (par vel5 d1))
             (mk (par vel5 d1))
             (into (update-score S0 vel5)
                   (update-score S0 d1))))

      (is (thrown? Exception (update-score :not-an-update score0))))

    (testing "partial update"

      (is (= score0 (partial-update score0 chan1 vel4)))

      (is (= (partial-update (mk (chans (tup d0 d1 d2)
                                        o1-))
                             chan1
                             (tup s0 s1 s2))
             (mk (chans (tup d0 d1 d2)
                        [o1- (tup s0 s1 s2)])))))

    (testing "map-update"

      (is (= (map-update score0 vel2)
             (mk vel2)))

      (is (= (map-update (mk (lin s0 s1)) vel2)
             (mk vel2 (lin s0 s1))))

      (is (= (map-update (mk (tup s0 s2))
                         (tup d0 d3))
             (mk (tup [s0 (tup d0 d3)]
                      [s2 (tup d0 d3)])))))

    (testing "same, _, k"

      (is (= (mk same)
             (mk _)
             S0))

      (is (= (mk (k _))
             S0))

      (is (= (mk (tup d0 d1 d2)
                 (k (lin s0 s1)))
             (mk (lin s0 s1))))

      (is (= (mk void)
             (mk (tup d0 d1 d2) void)
             #{})))

    (testing "chain"

      (is (= (mk (chain chan2 vel2))
             (mk (chain (chain chan2) (chain vel2)))
             (mk [chan2 vel2])
             (update-score (update-score S0 chan2)
                           vel2)))

      (is (= S0 (mk (chain)))))

    (testing "par, par>, lin, lin>"

      (is (= (mk (par chan2 chan3))
             (mk (par chan2 chan3 chan3))
             (into (mk chan2)
                   (mk chan3))))

      (is (= (mk (par> d1 d1))
             (mk (par d1 d2))))

      (is (= (mk (lin s1 [chan2 s3]))
             (concat-score (mk s1) (mk chan2 s3))
             #{{:patch [0 4], :channel 2, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 3}}, :voice 0, :duration 1, :position 1, :velocity 80, :track 0}
               {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1}}, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}}))

      (is (= (mk (lin> d1 d1 d1))
             (mk (lin d1 d2 d3)))))

    (testing "each"

      (is (= (mk (lin s0 s1 s2)
                 (each d1))
             (mk (lin [s0 d1] [s1 d1] [s2 d1]))))

      (is (= (mk (lin s0 s1 s2)
                 (each d1 c1))
             (mk (lin [s0 d1 c1]
                      [s1 d1 c1]
                      [s2 d1 c1]))
             #{{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 1, :c 1}}, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}
               {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2, :d 1, :c 1}}, :voice 0, :duration 1, :position 2, :velocity 80, :track 0}
               {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 1, :c 1}}, :voice 0, :duration 1, :position 1, :velocity 80, :track 0}}))

      (is (= (mk (tup d0 d1 d2)
                 (each (tup d0 d1 d2)))
             (mk (tup d0 d1 d2
                      d1 d2 d3
                      d2 d3 d4)))))

    (testing "fit tup tup>"

      (is (= (mk (fit dur2))
             S0))

      (is (= (mk (fit (lin d1 d2 d3)))
             (mk (tup d1 d2 d3))))

      (is (= (score-duration
              (mk (tup d0 d2 d3)))
             1))

      (is (= (mk (tup> d1 d1 d1))
             (mk (tup d1 d2 d3)))))

    (testing "append, superpose"

      (is (= (mk (append d1))
             (mk (lin same d1))))

      (is (= (mk (superpose d1))
             (mk (par same d1)))))

    (testing "rep rup"

      (is (= (mk (rep 3 d1))
             (mk (lin same d1 d2))))

      (is (= (mk (rep 3 d1 :skip-first))
             (mk (lin d1 d2 d3))))

      (is (= (mk (rup 3 d1))
             (mk (tup same d1 d2))))

      (is (= (mk (rup 3 d1 :skip-first))
             (mk (tup d1 d2 d3)))))

    (testing "dup dupt"

      (is (= (mk (dup 3))
             (mk (lin same same same))))

      (is (= (mk (dupt 3))
             (mk (tup same same same)))))

    (testing "nlin ntup"

      (is (= (mk (ntup 3 d1))
             (mk (tup d1 d1 d1))))

      (is (= (mk (nlin 3 d1))
             (mk (lin d1 d1 d1)))))

    (testing "parts"

      (is (= score0
             (mk (parts chan2 void))
             (mk (parts chan0 same))))

      (is (= #{}
             (mk (parts chan0 void))))

      (is (= (mk (par chan1 chan2)
                 (parts chan1 d1))
             (mk (par [chan1 d1] chan2))))

      (is (= (mk (par chan1 chan2)
                 (parts chan1 d1 chan2 d2))
             (mk (par [chan1 d1] [chan2 d2])))))

    (testing "repeat-while"

      (is (= (mk (repeat-while within-midi-pitch-bounds?
                               o1
                               o1-))
             (mk o5)))

      (testing "test can be a regular function"
        (is (= (mk (repeat-while (fn [s] (< (count s) 8))
                                 (dup 2)))
               (mk (dup 8)))))

      (testing "test empty set return is interpreted as failure"
        (is (= (mk (repeat-while (sf_ #{})
                                 (tup d0 d1)))
               (mk (tup d0 d1)))))

      (testing "throwing when update or after are not updates"
        (is (thrown? Exception (repeat-while :not-an-update same)))
        (is (thrown? Exception (repeat-while same same :not-an-update)))))

    (testing "fst fst-that"

      (is (= (mk (fst void
                      d2))
             (mk (fst void
                      void
                      d2
                      void))
             (mk (fst d2
                      void))
             (mk d2)))

      (is (= (mk (fst-that (within-pitch-bounds? :C0 :G0)
                           o1
                           d5
                           d2
                           d1))
             (mk d2)))

      (is (= (mk (fst-that (within-pitch-bounds? :C0 :G0)
                           o1
                           d1
                           d2))
             (mk d1)))

      (is (= (mk (fst-that (within-time-bounds? 0 1)
                           (dup 3)
                           (lin d0 d1)
                           (tup _ _)))
             (mk (dupt 2)))))

    (testing "shrink"

      (is (= (mk (chans (dupt 3)
                        (dup 3))
                 (shrink chan1))
             (mk [chan1 (dup 3)])))

      (is (= (mk (lin d0 d1 d2)
                 (shrink {:position (lt 2)}))
             (mk (lin d0 d1)))))

    (testing "adjust"

      (is (= (mk (dup 4)
                 (adjust {:duration 2}))
             (mk (dupt 2)
                 (dup 2))))

      (is (= (mk (dup 4)
                 (adjust {:duration 2 :position 2}))
             (mk dur:2
                 (dup 4)
                 (sf_ (shift-score _ 2))))))

    (testing "fork-with, voices, chans, tracks"

      (is (= (mk (fork-with (fn [i] (vel (* 10 i)))
                            s1
                            o1))
             (mk (par [s1 (vel 0)] [o1 (vel 10)]))))

      (is (= (mk (voices d0 d1 d2))
             (mk (par [(voice 0) d0]
                      [(voice 1) d1]
                      [(voice 2) d2]))))

      (is (= (mk (chans d0 d1 d2))
             (mk (par [chan0 d0]
                      [chan1 d1]
                      [chan2 d2]))))

      (is (= (mk (tracks d0 d1 d2))
             (mk (par [track0 d0]
                      [track1 d1]
                      [track2 d2])))))

    (testing "mirror rev"

      (is (= (mk (tup d0 d1 d2)
                 rev)
             (mk (tup d2 d1 d0))
             (mk (tup d2 d1 d0)
                 rev rev)))

      (is (= (numerify-pitches
              (mk (tup d0 d1 d2)
                  (mirror :C0)))
             (numerify-pitches
              (mk (tup d0 c2- c4-)))))

      (is (= (numerify-pitches
              (mk (tup d0 d1 d2)
                  (mirror :G0)))
             (numerify-pitches
              (mk (tup [o1 d1] o1 [o1 c2-]))))))

    (testing "event-scale"

      (is (= (mk (tup (vel 0) (vel 60) (vel 120))
                 (event-scale :velocity [30 60]))
             (mk (tup (vel 30) (vel 45) (vel 60))))))

    (testing "selection"

      (testing "min-by max-by"

        (is (= (mk (tup d0 d1 d2)
                   (min-by :position))
               (mk [dur:3 d0])))

        (is (= (mk (tup d0 [vel12 d1] d2)
                   (max-by :velocity))
               (mk [dur:3 d1 vel12]
                   (adjust {:position 1/3}))))

        (is (= (mk (lin d0 d1 d2)
                   min-pitch)
               (mk d0)))

        (is (= (mk (lin d0 d1 d2)
                   max-pitch)
               (mk d2
                   (adjust {:position 2})))))

      (testing "time"

        (is (= (mk (lin d0 d1 d2)
                   (from 1))
               (mk (lin d0 d1 d2)
                   (from 0.5))
               (mk (lin d1 d2)
                   (adjust {:position 1}))))

        (is (= (mk (lin d0 d1 d2)
                   (until 2))
               (mk (lin d0 d1 d2)
                   (until 1.5))
               (mk (lin d0 d1))))

        (is (= (mk (lin d0 d1 d2)
                   (between 1 2))
               (mk (lin d0 d1 d2)
                   (between 0.5 1.5))
               (mk d1 (adjust {:position 1}))))

        (is (= (mk (lin d0 d1 d2)
                   (start-from 1))
               (mk (lin d1 d2))))

        (is (= (mk (lin d0 d1 d2)
                   start-from-last)
               (mk (lin d0 d1 d2)
                   (start-from-nth-last 1))
               (mk d2)))

        (is (= (mk (lin d0 d1 d2)
                   (start-from-nth-last 2))
               (mk (lin d1 d2))))

        (is (= (mk (lin d0 d1 d2)
                   (start-from-nth-last 4))
               #{}))

        (testing "trim"

          (is (= (mk (lin d0 d1 d2)
                     (trim 1 2))
                 (mk d1
                     (adjust {:position 1}))))

          (is (= (mk (lin d0 d1 d2)
                     (trim 0 3))
                 (mk (lin d0 d1 d2))))

          (is (= #{}
                 (mk (lin d0 d1 d2)
                     (trim 0 0))))

          (is ((->event-matcher {:trimed-fw true
                                 :duration 1/2
                                 :position 0})
               (first (mk (trim 0 1/2)))))

          (is ((->event-matcher {:trimed-bw true
                                 :duration 1/2
                                 :position 1/2})
               (first (mk (trim 1/2 1))))))

        (testing "checks"

          (let [f (within-bounds? :velocity 30 60)]
            (is (and (not (mk vel1 f))
                     (mk vel3 f)
                     (mk (vel 60) f)
                     (mk (vel 30) f)
                     (mk vel5 f)
                     (not (mk vel7 f))
                     (not (mk vel9 f))
                     (not (mk vel11 f)))))

          (is (mk (lin d0 d1 d2)
                  (within-time-bounds? 0 3)))

          (is (not (mk (lin d0 d1 d2 d3)
                       (within-time-bounds? 0 3))))

          (is (mk (lin d0 d1 d2)
                  (within-pitch-bounds? :C0 :E0)))

          (is (not (mk (lin d0 d1 d2)
                       (within-pitch-bounds? :C0 :D0))))

          (is (not (mk o8 within-midi-pitch-bounds?)))
          (is (not (mk o8- within-midi-pitch-bounds?)))
          (is (mk within-midi-pitch-bounds?))
          (is (mk (lin o1 o2 o3)
                  within-midi-pitch-bounds?)))

        (testing "non determinism"

          (is (pr/with-rand 0
                (= (mk (one-of chan1 chan2 chan3))
                   (mk chan3))))

          (is (pr/with-rand -78
                (= (mk (one-of chan1 chan2 chan3))
                   (mk chan1))))

          (is (every? (->event-matcher {:channel (partial contains? #{0 1 2 3})})
                      (mk (nlin 100 (maybe chan1 chan2 chan3)))))

          (is (every? (->event-matcher {:channel (partial contains? #{0 1 2 3})})
                      (mk (nlin 100 (probs {chan1 1 chan2 2 chan3 7})))))

          (testing "mix and shuf"

            (is (= (sort (map pitch-value (mk (shuftup d0 d1 d2))))
                   (sort (map pitch-value (mk (mixtup d0 d1 d2))))
                   (sort (map pitch-value (mk (tup d0 d1 d2))))
                   (sort (map pitch-value (mk (shuflin d0 d1 d2))))
                   (sort (map pitch-value (mk (mixlin d0 d1 d2))))
                   (sort (map pitch-value (mk (lin d0 d1 d2))))))))

        (testing "incubating"

          (testing "fill fill>"

            (is (= (mk (fill 1/4 d3))
                   (mk (tup d3 d3 d3 d3))))

            (is (= (mk dur2 (fill 1/4 d3))
                   (mk dur2 (ntup 8 d3))))

            (is (= (mk (fill> 1/4 d3))
                   (mk (ntup> 4 d3))))

            (is (= (mk dur2 (fill> 2/5 d3))
                   (mk dur2 (ntup> 5 d3))))

            (is (thrown? Exception
                         (mk (fill 2/3 d1))))))))

    (is (tu/frozen :frozen-test
                   (lin d1 d2 d3)))))


'(deftest new-rep
  (testing "new rep"
    (play (newrep 5))
    (play (newrep 5 (one-of d1 d2 d3 d4) :fit))
    (play (newrep 5 (one-of s0 s1 s2 s3 s4) :par))

    (play (iter 7 d1 :fit true))
    (play (iter d1 :take 7 :fit true))
    (play (iter d1 :drop 1 :take 7 :fit true))
    (play (iter d1 :next true :take 7 :fit true))
    (play (iter 7 d1 :fit :append))
    (play (iter 4 d1 :next true))
    (play (iter 4 d3 :par true)
          (iter 4 (one-of d1- d2))
          (iter 4 (transpose c3-)))))
