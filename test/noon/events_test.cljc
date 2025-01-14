(ns noon.events-test
  (:require [noon.events :as e]
            [noon.updates :as u]
            [clojure.test :as t :refer [testing deftest is]]
            [noon.numbers :as numbers]
            [noon.utils.pseudo-random :as pr]
            [noon.utils.misc :as utils]))

(def event0 e/DEFAULT_EVENT)
(defn e0> [& xs]
  (utils/?reduce #(%2 %1) event0 xs))

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

      (let [m1-1 (e/->event-matcher u/chan0)
            m1-2 (e/->event-matcher {:channel 0})
            m2-1 (e/->event-matcher u/chan2)
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

    (is (= (:duration ((u/dur 2) event0))
           2))
    (is (= (:duration ((u/dur (/ 1 2)) event0))
           (/ 1 2)))

    (is (= (:velocity ((u/vel 23) event0))
           23))
    (is (= (:velocity ((u/vel inc) event0))
           (:velocity ((u/vel+ 1) event0))
           81))
    (is (= (:velocity ((u/vel- 10) event0))
           70))
    (is (= (:velocity ((u/vel (numbers/mul 2)) event0))
           127))
    (is (zero? (:velocity ((u/vel (numbers/sub 100)) event0))))

    (is (zero? (:channel event0)))
    (is (= (:channel ((u/chan inc) event0))
           1))
    (is (= (:channel ((u/chan+ 10) event0))
           10))
    (is (= (:channel ((u/chan+ 100) event0))
           15))
    (is (= (:channel ((u/chan- 100) event0))
           0))

    (is (= (:track event0)
           0))
    (is (= (:track ((u/track+ 1) event0))
           1))
    (is (= (:track ((u/track+ 1000) event0))
           1000))
    (is (= (:track ((u/track+ 100000) event0))
           65535))

    (is (= (:voice event0)
           0))
    (is (= (:voice ((u/voice+ 1) event0))
           1))
    (is (= (:voice ((u/voice+ 1000) event0))
           15))
    (is (= (:voice ((u/voice -1) event0))
           0))

    (is (= (:cc ((u/cc :volume 10) event0))
           (:cc ((u/cc "volume" 10) event0))
           (:cc ((u/cc "Volume" 10) event0))
           (:cc ((u/cc 'volume 10) event0))
           (:cc ((u/cc 'Volume 10) event0))
           {7 10}))

    (is (= (:cc ((u/cc :volume [50 100]) event0))
           {7 [50 100]}))
    (is (= (:cc ((u/cc :volume [-50 80 150]) event0))
           {7 [0 80 127]}))

    (is (= (:cc ((u/cc :bank-select-1 10) event0))
           {0 10}))

    (is (= (:cc ((u/cc :bank-select-1 (numbers/mul 100))
                 ((u/cc :bank-select-1 10) event0)))
           {0 127}))

    (is (= (:cc ((u/cc :bank-select-1 (numbers/mul -100))
                 ((u/cc :bank-select-1 10) event0)))
           {0 0}))

    (is (thrown? #?(:clj Exception
                    :cljs js/Error)
                 (u/cc :vlume 10)))

    (is (= (:patch ((u/patch :vibraphone) event0))
           (:patch ((u/patch :vibe) event0))
           [nil 11]))

    (is (= (:patch ((u/patch :chromaphone/smooth-carillon) event0))
           (:patch ((u/patch :chroma/smooth-carillon) event0))
           [8 1]))

    (is (= (pr/with-rand 0
             (:patch ((u/patch :chromaphone/short) event0)))
           #?(:clj [11 6]
              :cljs [11 13])))

    (is (= (:patch ((u/patch :truc) event0))
           [nil 56])))

  (testing "aliases"

    (is (= (:velocity (u/vel2 event0))
           21))
    (is (= (:velocity (u/vel12 event0))
           127))
    (is (= (:velocity (u/vel0 event0))
           0))

    (is (= (:duration (u/dur2 event0))
           2))
    (is (= (:duration (u/dur:2 event0))
           (/ 1 2)))
    (is (= (:duration (u/dur5:2 event0))
           (/ 5 2)))

    (is (= (:channel (u/chan3 event0))
           3))

    (is (= (:track (u/track12 event0))
           12)))

  (testing "pitch"

    ;; see noon.harmonic-context test for wrapped :pitch transformations testing

    (is (= (e0> u/t3 :pitch)
           (e0> u/s1 u/t3 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 3}}))

    (is (= (e0> u/t3- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t -3}}))

    (is (= (e0> u/s2 :pitch)
           (e0> u/d1 u/c1 u/s2 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2}}))

    (is (= (e0> u/s2- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s -2}}))

    (is (= (e0> u/d1 :pitch)
           (e0> u/c1 u/d1 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 1}}))

    (is (= (e0> u/d1- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d -1}}))

    (is (= (e0> u/c1 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 1}}))

    (is (= (e0> u/c1- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c -1}}))

    (is (= (e0> u/c1- u/o1 :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 1, :s 0, :d 0, :c -1}}))

    (is (= (e0> u/s1- u/d2 u/o2- :pitch)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t -2, :s -1, :d 2}}))))
