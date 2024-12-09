(ns noon.events-test
  (:require [noon.events :as e]
            [clojure.test :as t :refer [testing deftest is]]
            [noon.numbers :as numbers]
            [noon.utils.pseudo-random :as pr]
            [noon.utils.misc :as u]))

(def event0 e/DEFAULT_EVENT)
(defn e0> [& xs]
  (u/?reduce #(%2 %1) event0 xs))

(deftest midi-values
  (is (= 0
         (e/midi-val 0)
         (e/midi-val -1)
         (e/midi-val :min)
         (e/midi-val 0.0)
         (e/midi-val (/ 0 1))
         (e/midi-val (/ -1 2))))
  (is (= 127
         (e/midi-val 127)
         (e/midi-val 1270)
         (e/midi-val 1.001)
         (e/midi-val 1.5)
         (e/midi-val (/ 3 2))
         (e/midi-val :max)))
  (is (= 64
         (e/midi-val 64)
         (e/midi-val (/ 1 2))
         (e/midi-val 0.5)))

  (is (= (pr/with-rand 0 (take 100 (iterate
                                    (e/humanize :max-step (/ 1 10) :bounds [20 80])
                                    60)))
         #?(:clj (list 60 63 59 61 62 64 61 59 65 70 76 72 67 62 56 57 63 58 60 58 62 68 67 70 73 77 79 76
                       79 73 67 73 76 78 73 72 66 60 58 56 61 67 72 74 70 66 71 65 68 72 75 71 75 80 74 72 77 79 80
                       79 74 80 78 77 79 76 74 79 76 80 79 75 69 63 57 56 62 68 72 73 70 67 72 66 60 58 53 58 52 49 50
                       56 53 58 60 65 59 61 60 61)
            :cljs (list 60 61 62 68 65 60 66 64 62 61 63 69 70 75 77 76 78 75 74 79 80 79 77 73 78 74 79 80 75
                        79 78 76 79 73 75 69 70 71 77 73 75 78 75 72 76 70 64 62 58 53 56 61 64 67 64 62 61 55
                        59 56 60 56 52 53 59 58 64 59 55 49 53 47 53 48 54 51 49 54 52 55 50 52 58 63 65 60 55
                        57 60 62 56 52 57 51 57 53 56 54 59 61)))))

(deftest event-updates

  (testing "basics"

    (is (= ((e/efn e e)
            e/DEFAULT_EVENT)
           ((e/ef_ _)
            e/DEFAULT_EVENT)
           e/DEFAULT_EVENT))

    (is (e/event-update? (e/ef_ _)))
    (is (e/event-update? (e/efn e e)))

    (is (not (e/event-update? (fn [x] x))))

    (let [u (e/map->efn {:position inc})
          v (e/map->efn {:position 1})]
      (and (is (e/event-update? u))
           (is (e/event-update? v))
           (is (= (u e/DEFAULT_EVENT)
                  (v e/DEFAULT_EVENT)
                  (assoc e/DEFAULT_EVENT :position 1)))))

    (testing "->event-update"

      (let [a (e/->event-update {:position inc})
            b (e/->event-update (e/map->efn {:position inc}))
            c (e/->event-update {:duration (numbers/div 2)})
            d (e/->event-update [{:duration (numbers/div 2)} (e/map->efn {:position inc})])]
        (is (and a b c d))
        (is (= (a e/DEFAULT_EVENT)
               (b e/DEFAULT_EVENT)
               (assoc e/DEFAULT_EVENT :position 1)))
        (is (= (d e/DEFAULT_EVENT)
               (-> e/DEFAULT_EVENT (a) (c))
               (assoc e/DEFAULT_EVENT :position 1 :duration (/ 1 2))))))

    (testing "event-matcher"

      (is (e/event-matcher? (e/event-matcher (fn [_] true))))

      (let [m1-1 (e/->event-matcher e/chan0)
            m1-2 (e/->event-matcher {:channel 0})
            m2-1 (e/->event-matcher e/chan2)
            m2-2 (e/->event-matcher {:channel 2})
            m3-1 (e/->event-matcher (fn [e] (zero? (:position e))))
            m3-2 (e/->event-matcher (fn [e] (pos? (:position e))))
            m4-1 (e/->event-matcher (fn [e] {:a (zero? (:position e))
                                           :b (pos? (:duration e))}))
            m4-2 (e/->event-matcher (fn [e] {:a (zero? (:position e))
                                           :b (zero? (:duration e))}))]

        (is (and (m1-1 e/DEFAULT_EVENT)
                 (m1-2 e/DEFAULT_EVENT)
                 (m3-1 e/DEFAULT_EVENT)
                 (m4-1 e/DEFAULT_EVENT)))
        (is (not (or (m2-1 e/DEFAULT_EVENT)
                     (m2-2 e/DEFAULT_EVENT)
                     (m3-2 e/DEFAULT_EVENT)
                     (m4-2 e/DEFAULT_EVENT)))))))

  (testing "simples"

    (is (= (:duration ((e/dur 2) event0))
           2))
    (is (= (:duration ((e/dur (/ 1 2)) event0))
           (/ 1 2)))

    (is (= (:velocity ((e/vel 23) event0))
           23))
    (is (= (:velocity ((e/vel inc) event0))
           (:velocity ((e/vel+ 1) event0))
           81))
    (is (= (:velocity ((e/vel- 10) event0))
           70))
    (is (= (:velocity ((e/vel (numbers/mul 2)) event0))
           127))
    (is (zero? (:velocity ((e/vel (numbers/sub 100)) event0))))

    (is (zero? (:channel event0)))
    (is (= (:channel ((e/chan inc) event0))
           1))
    (is (= (:channel ((e/chan+ 10) event0))
           10))
    (is (= (:channel ((e/chan+ 100) event0))
           15))
    (is (= (:channel ((e/chan- 100) event0))
           0))

    (is (= (:track event0)
           0))
    (is (= (:track ((e/track+ 1) event0))
           1))
    (is (= (:track ((e/track+ 1000) event0))
           1000))
    (is (= (:track ((e/track+ 100000) event0))
           65535))

    (is (= (:voice event0)
           0))
    (is (= (:voice ((e/voice+ 1) event0))
           1))
    (is (= (:voice ((e/voice+ 1000) event0))
           15))
    (is (= (:voice ((e/voice -1) event0))
           0))

    (is (= (:cc ((e/cc :volume 10) event0))
           (:cc ((e/cc "volume" 10) event0))
           (:cc ((e/cc "Volume" 10) event0))
           (:cc ((e/cc 'volume 10) event0))
           (:cc ((e/cc 'Volume 10) event0))
           {7 10}))

    (is (= (:cc ((e/cc :volume [50 100]) event0))
           {7 [50 100]}))
    (is (= (:cc ((e/cc :volume [-50 80 150]) event0))
           {7 [0 80 127]}))

    (is (= (:cc ((e/cc :bank-select-1 10) event0))
           {0 10}))

    (is (= (:cc ((e/cc :bank-select-1 (numbers/mul 100))
                 ((e/cc :bank-select-1 10) event0)))
           {0 127}))

    (is (= (:cc ((e/cc :bank-select-1 (numbers/mul -100))
                 ((e/cc :bank-select-1 10) event0)))
           {0 0}))

    (is (thrown? #?(:clj Exception
                    :cljs js/Error)
                 (e/cc :vlume 10)))

    (is (= (:patch ((e/patch :vibraphone) event0))
           (:patch ((e/patch :vibe) event0))
           [nil 11]))

    (is (= (:patch ((e/patch :chromaphone/smooth-carillon) event0))
           (:patch ((e/patch :chroma/smooth-carillon) event0))
           [8 1]))

    (is (= (pr/with-rand 0
             (:patch ((e/patch :chromaphone/short) event0)))
           #?(:clj [11 6]
              :cljs [11 13])))

    (is (= (:patch ((e/patch :truc) event0))
           [nil 56])))

  (testing "aliases"

    (is (= (:velocity (e/vel2 event0))
           21))
    (is (= (:velocity (e/vel12 event0))
           127))
    (is (= (:velocity (e/vel0 event0))
           0))

    (is (= (:duration (e/dur2 event0))
           2))
    (is (= (:duration (e/dur:2 event0))
           (/ 1 2)))
    (is (= (:duration (e/dur5:2 event0))
           (/ 5 2)))

    (is (= (:channel (e/chan3 event0))
           3))

    (is (= (:track (e/track12 event0))
           12)))

  (testing "pitch"

    ;; see noon.harmony test for wrapped :pitch transformations testing

    (is (= (e0> e/t3 :pitch)
           (e0> e/s1 e/t3 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 3}}))

    (is (= (e0> e/t3- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t -3}}))

    (is (= (e0> e/s2 :pitch)
           (e0> e/d1 e/c1 e/s2 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2}}))

    (is (= (e0> e/s2- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s -2}}))

    (is (= (e0> e/d1 :pitch)
           (e0> e/c1 e/d1 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 1}}))

    (is (= (e0> e/d1- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d -1}}))

    (is (= (e0> e/c1 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 1}}))

    (is (= (e0> e/c1- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c -1}}))

    (is (= (e0> e/c1- e/o1 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 1, :s 0, :d 0, :c -1}}))

    (is (= (e0> e/s1- e/d2 e/o2- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t -2, :s -1, :d 2}}))))
