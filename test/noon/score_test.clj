(ns noon.score-test
  (:use noon.score)
  (:require [clojure.test :refer [deftest testing is]]
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
           (update-score S0 {:position (add 10)})))
    (is (let [s0 (mk (lin s0 s2 s4))
              s1 (fit-score s0 event0)
              s2 (normalise-score s0)]
          (and (= s1 s2)
               (= 1 (score-duration s1))
               (= 0 (score-origin s2)))))
    (is (let [s (fit-score (mk (lin s0 s2 s4))
                           {:position 3 :duration 2})]
          (and (= 5 (score-duration s))
               (= 3 (score-origin s)))))

    (is (let [s (concat-score S0 S0)]
          (and (= 2 (score-duration s))
               (= 0 (score-origin s)))))
    (is (= #{} (concat-scores [])))
    (is (= S0 (concat-scores [S0])))
    (is (let [s (concat-score S0 S0)
              s (concat-scores [s s s])]
          (and (= 6 (score-duration s))
               (= 0 (score-origin s)))))

    (is (= (reverse-score (mk (lin s0 s2 s4)))
           (mk (lin s4 s2 s0))))

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
           (list [0 [0 4] nil] [1 [nil 11] {7 70}] [2 nil nil]))))

  (testing "updates"
    (is (u/t? :score-update (sf_ _)))
    (is (score-update? (sfn [s] s)))
    (is (= (update-score S0 (sf_ _))
           S0))
    (is (= (update-score S0 dur2)
           (mk dur2)))
    (is (= (update-score S0 [vel10 dur2])
           (mk dur2 vel10)))
    (is (= (update-score S0 (g/one-of vel10))
           (mk vel10)))
    (is (= (update-score S0 #{vel5 d1})
           (mk (par vel5 d1))
           (into (update-score S0 vel5)
                 (update-score S0 d1))))

    (is (= (mk same)
           (mk _)
           S0))

    (is (= (mk (k S0))
           S0))

    (is (= (mk void)
           #{}))

    (is (= (mk (chain chan2 vel2))
           (mk (chain (chain chan2) (chain vel2)))
           (mk [chan2 vel2])
           (update-score (update-score S0 chan2)
                         vel2)))

    (is (= S0 (mk (chain))))

    (is (= (mk (par chan2 chan3))
           (mk (par chan2 chan3 chan3))
           (into (mk chan2)
                 (mk chan3))))

    (is (= (mk (par> d1 d1))
           (mk (par d1 d2))))

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

    (is (= (mk (lin s1 [chan2 s3]))
           (concat-score (mk s1) (mk chan2 s3))
           #{{:patch [0 4], :channel 2, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 3}}, :voice 0, :duration 1, :position 1, :velocity 80, :track 0}
             {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1}}, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}}))

    (is (= (mk (lin> d1 d1 d1))
           (mk (lin d1 d2 d3))))

    (is (= (mk (fit dur2))
           S0))

    (is (= (mk (fit (lin d1 d2 d3)))
           (mk (tup d1 d2 d3))))

    (is (= (score-duration
            (mk (tup d0 d2 d3)))
           1))

    (is (= (mk (tup> d1 d1 d1))
           (mk (tup d1 d2 d3))))

    (is (= (mk (append d1))
           (mk (lin same d1))))

    (is (= (mk (superpose d1))
           (mk (par same d1))))

    (is (= (mk (rep 3 d1))
           (mk (lin same d1 d2))))

    (is (= (mk (rep 3 d1 :skip-first))
           (mk (lin d1 d2 d3))))

    (is (= (mk (rup 3 d1))
           (mk (tup same d1 d2))))

    (is (= (mk (rup 3 d1 :skip-first))
           (mk (tup d1 d2 d3))))

    (is (= (mk (dup 3))
           (mk (lin same same same))))

    (is (= (mk (dupt 3))
           (mk (tup same same same))))

    (is (= (mk (ntup 3 d1))
           (mk (tup d1 d1 d1))))

    (is (= (mk (nlin 3 d1))
           (mk (lin d1 d1 d1))))

    (is (= (mk (par chan1 chan2)
               (parts chan1 d1))
           (mk (par [chan1 d1] chan2))))

    (is (tu/frozen (lin d1 d2 d3)))))


(deftest new-rep
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
