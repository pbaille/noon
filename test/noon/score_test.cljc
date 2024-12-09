(ns noon.score-test
  (:require [noon.score :as s]
            [noon.updates :as u]
            [noon.events :as e]
            [noon.utils.pseudo-random :as pr]
            [noon.utils.chance :as g]
            [noon.utils.misc :as utils]
            [noon.numbers :as numbers]
            [clojure.test :as t :refer [deftest testing is]]))

(def event0 e/DEFAULT_EVENT)

(def S0 s/score0)

(defn e0> [& xs]
  (utils/?reduce #(%2 %1) event0 xs))

(defn round-with-precision [number precision]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* number factor)) factor)))

(defn comparable-score [score]
  (into #{}
        (map (fn [e] (-> e
                         (update :position round-with-precision 5)
                         (update :duration round-with-precision 5)))
             score)))

(defn score= [& scores]
  (apply = (map comparable-score scores)))

(deftest score-test

  (testing "basic"

    (is (= (s/score s/score0)
           s/score0))

    (is (s/score? (s/score s/score0)))

    (is (= (s/score event0)
           #{event0}))

    (is (s/score? (s/score event0)))

    (is (= (s/score (g/one-of event0))
           s/score0))

    (is (not (s/score? 23)))
    (is (not (s/score? {:a 1}))))

  (testing "views"

    (is (= (s/score-duration S0)
           1))
    (is (= (s/score-duration (s/update-score S0 e/dur2))
           2))
    (is (= (s/score-duration (s/mk (u/lin e/s0 e/s2 e/s4)))
           3))

    (is (= (s/score-track-count S0)
           1))
    (is (= (s/score-track-count (s/mk (u/superpose e/track1)))
           2))

    (is (= (s/score-bounds S0 :position)
           [{:position 0, :channel 0, :track 0, :duration 1, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}, :velocity 80, :voice 0, :patch [0 4]}
            {:position 0, :channel 0, :track 0, :duration 1, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}, :velocity 80, :voice 0, :patch [0 4]}]))

    (is (= (s/score-bounds (s/mk (u/lin e/s0 e/s2 e/s4)) :position)
           [{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0}}, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}
            {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 4}}, :voice 0, :duration 1, :position 2, :velocity 80, :track 0}]))

    (is (= (s/score-origin S0)
           0))
    (is (= (s/score-origin (s/update-score S0 {:position (numbers/add 10)}))
           10))

    (is (= (s/pitch-value-bounds S0)
           [60 60]))
    (is (= (s/pitch-value-bounds (s/mk (u/lin e/s0 e/s2 e/s4)))
           [60 76])))

  (testing "transformations"

    (testing "map-event-update"

      (is (= (s/map-event-update (s/mk (u/lin e/d0 e/d1 e/d2)) e/d1)
             (s/mk (u/lin e/d1 e/d2 e/d3))))

      (is (= (s/map-event-update (s/mk (u/lin e/d0 e/d1 e/d2)) e/vel4)
             (s/mk e/vel4 (u/lin e/d0 e/d1 e/d2))))

      (testing "can remove events by returning nil"

        (is (= (count (s/map-event-update (s/mk (u/lin e/d0 e/d1 e/d2))
                                          (fn [e] (if (not (zero? (e/pitch-class-value e))) e))))
               2))))

    (testing "scale score, shift score"

      (is (= (s/score-duration
              (s/scale-score (s/mk (u/lin e/s0 e/s2 e/s4))
                             (/ 1 3)))
             1))
      (is (numbers/float-equal? (s/score-duration
                                 (s/scale-score (s/shift-score S0 10)
                                                (/ 1 3)))
                                (/ 11 3)))

      (is (numbers/float-equal? (s/score-origin
                                 (s/scale-score (s/shift-score S0 10)
                                                (/ 1 3)))
                                (/ 10 3)))

      (is (= (s/shift-score S0 10)
             (s/update-score S0 {:position (numbers/add 10)}))))

    (testing "fit-score, normalise-score"

      (is (let [s0 (s/mk (u/lin e/s0 e/s2 e/s4))
                s1 (s/fit-score s0 event0)
                s2 (s/normalise-score s0)]
            (and (= s1 s2)
                 (= 1 (s/score-duration s1))
                 (= 0 (s/score-origin s2)))))

      (is (let [s (s/fit-score (s/mk (u/lin e/s0 e/s2 e/s4))
                               {:position 3 :duration 2})]
            (and (= 5 (s/score-duration s))
                 (= 3 (s/score-origin s))))))

    (is (= (s/reverse-score (s/mk (u/lin e/s0 e/s2 e/s4)))
           (s/mk (u/lin e/s4 e/s2 e/s0))))

    (testing "filter-score"

      (is (= (s/mk (u/lin e/d0 e/d1))
             (s/filter-score (s/mk (u/lin e/d0 e/d1)) map?)))

      (is (= (s/mk (u/lin e/d0 e/d1))
             (s/filter-score (s/mk (u/lin e/d0 e/d1)) {:channel 0})
             (s/filter-score (s/mk (u/lin e/d0 e/d1)) e/chan0)))

      (is (= (s/filter-score (s/mk (u/chans (u/lin e/d0 e/d1)
                                            (u/lin e/d3 e/d4)))
                             e/chan0)
             (s/mk (u/lin e/d0 e/d1)))))

    (testing "midi prepare helpers"

      (is (= (s/numerify-pitches (s/mk (u/lin e/s0 e/s2 e/s4)))
             #{{:patch [0 4], :channel 0, :pitch 60, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}
               {:patch [0 4], :channel 0, :pitch 67, :voice 0, :duration 1, :position 1, :velocity 80, :track 0}
               {:patch [0 4], :channel 0, :pitch 76, :voice 0, :duration 1, :position 2, :velocity 80, :track 0}}))

      (is (= (->> (s/dedupe-patches-and-control-changes (s/mk (u/lin e/s0 e/s2 e/s4)))
                  (s/sort-score)
                  (map (juxt :position :patch :cc)))
             (list [0 [0 4] nil] [1 nil nil] [2 nil nil])))

      (is (= (->> (s/mk (u/lin e/s0
                               [(e/patch :vibraphone)
                                (e/cc :volume 70)
                                (u/lin e/s2 e/s4)]))
                  (s/dedupe-patches-and-control-changes)
                  (s/sort-score)
                  (map (juxt :position :patch :cc)))
             (list [0 [0 4] nil] [1 [nil 11] {7 70}] [2 nil nil])))))

  (testing "composition"

    (is (let [s (s/concat-score S0 S0)]
          (and (= 2 (s/score-duration s))
               (= 0 (s/score-origin s)))))

    (is (= #{} (s/concat-scores [])))

    (is (= S0 (s/concat-scores [S0])))

    (is (let [s (s/concat-score S0 S0)
              s (s/concat-scores [s s s])]
          (and (= 6 (s/score-duration s))
               (= 0 (s/score-origin s)))))

    (is (= (s/merge-scores [(s/mk (u/lin e/d0 e/d1 e/d2))
                            (s/mk (u/lin e/d1 e/d2 e/d3))])
           (s/mk (u/lin (u/par e/d0 e/d1) (u/par e/d1 e/d2) (u/par e/d2 e/d3))))))

  (testing "chunk-score"

    (is (= (s/chunk-score (s/mk (u/lin e/d0 (u/par e/d1 e/d2) e/d3))
                          :position)
           (list (s/mk e/d0)
                 (s/mk (u/par e/d1 e/d2) (u/adjust {:position 1}))
                 (s/mk e/d3 (u/adjust {:position 2}))))))

  (testing "updates"

    (testing "basics"

      (is (utils/t? :score-update (s/sf_ _)))

      (is (and (s/score-update? (s/sfn s s))
               (s/score-update? (s/sf_ _))))

      (is (not (s/score-update? (fn [s] s)))))

    (testing "->score-update"

      (let [id1 (s/->score-update (e/ef_ _))
            id2 (s/->score-update (s/sf_ _))
            id3 (s/->score-update [])
            id4 (s/->score-update [(e/ef_ _)])
            id5 (s/->score-update [(s/sf_ _) (s/sf_ _)])
            id6 (s/->score-update (g/one-of (s/sf_ _) (e/ef_ _)))]
        (is (= S0
               (s/update-score S0 id1)
               (s/update-score S0 id2)
               (s/update-score S0 id3)
               (s/update-score S0 id4)
               (s/update-score S0 id5)
               (s/update-score S0 id6))))

      (is (not (s/->score-update 0)))
      (is (not (s/->score-update (fn [x] x))))

      (is (= ((s/->score-update {:channel 1})
              S0)
             #{(assoc e/DEFAULT_EVENT :channel 1)})))

    (testing "->score-checker"

      (is (= S0
             ((s/->score-checker (fn [s] (<= (s/score-duration s) 1)))
              S0)))

      (is (nil?
           ((s/->score-checker (fn [s] (< (s/score-duration s) 1)))
            S0)))

      (is (nil? ((s/->score-checker {:position (numbers/gt 0)})
                 S0)))

      (is (= S0
             ((s/->score-checker {:position (numbers/gte 0)})
              S0))))

    (testing "chain-score-updates"

      (is (= S0
             (s/update-score S0 (s/chain-score-updates [(s/sf_ _) (s/sf_ _)]))))

      (is (= #{(assoc e/DEFAULT_EVENT :channel 5)}
             (s/update-score S0 (s/chain-score-updates [(s/->score-update {:channel 3})
                                                        (s/->score-update {:channel inc})
                                                        (s/->score-update {:channel inc})]))))

      (is (thrown? #?(:clj Exception
                      :cljs js/Error)
                   (s/chain-score-updates [1]))))

    (testing "map-score-update"

      (is (= (s/map-score-update (s/mk (u/lin e/d0 e/d1 e/d2))
                                 u/same)
             (s/mk (u/lin e/d0 e/d1 e/d2))))

      (is (= (s/map-score-update (s/mk (u/lin e/d0 e/d1 e/d2))
                                 (u/tup e/d0 e/d3))
             (s/mk (u/lin (u/tup e/d0 e/d3)
                          (u/tup e/d1 e/d4)
                          (u/tup e/d2 e/d5))))))

    (testing "update-score"

      (is (= (s/update-score S0 (s/sf_ _))
             S0))

      (is (= (s/update-score S0 e/dur2)
             (s/mk e/dur2)))

      (is (= (s/update-score S0 [e/vel10 e/dur2])
             (s/mk e/dur2 e/vel10)))

      (is (= (s/update-score S0 (g/one-of e/vel10))
             (s/mk e/vel10)))

      (is (= (s/update-score S0 (u/par e/vel5 e/d1))
             (s/mk (u/par e/vel5 e/d1))
             (into (s/update-score S0 e/vel5)
                   (s/update-score S0 e/d1))))

      (is (thrown? #?(:clj Exception
                      :cljs js/Error)
                   (s/update-score :not-an-update S0))))

    (testing "partial update"

      (is (= S0 (s/partial-update S0 e/chan1 e/vel4)))

      (is (= (s/partial-update (s/mk (u/chans (u/tup e/d0 e/d1 e/d2)
                                              e/o1-))
                               e/chan1
                               (u/tup e/s0 e/s1 e/s2))
             (s/mk (u/chans (u/tup e/d0 e/d1 e/d2)
                            [e/o1- (u/tup e/s0 e/s1 e/s2)])))))

    (testing "map-update"

      (is (= (s/map-update S0 e/vel2)
             (s/mk e/vel2)))

      (is (= (s/map-update (s/mk (u/lin e/s0 e/s1)) e/vel2)
             (s/mk e/vel2 (u/lin e/s0 e/s1))))

      (is (= (s/map-update (s/mk (u/tup e/s0 e/s2))
                           (u/tup e/d0 e/d3))
             (s/mk (u/tup [e/s0 (u/tup e/d0 e/d3)]
                          [e/s2 (u/tup e/d0 e/d3)])))))

    (testing "same, _, k"

      (is (= (s/mk u/same)
             (s/mk u/_)
             S0))

      (is (= (s/mk (u/k u/_))
             S0))

      (is (= (s/mk (u/tup e/d0 e/d1 e/d2)
                   (u/k (u/lin e/s0 e/s1)))
             (s/mk (u/lin e/s0 e/s1))))

      (is (= (s/mk u/void)
             (s/mk (u/tup e/d0 e/d1 e/d2) u/void)
             #{})))

    (testing "chain"

      (is (= (s/mk (u/chain e/chan2 e/vel2))
             (s/mk (u/chain (u/chain e/chan2) (u/chain e/vel2)))
             (s/mk [e/chan2 e/vel2])
             (s/update-score (s/update-score S0 e/chan2)
                             e/vel2)))

      (is (= S0 (s/mk (u/chain)))))

    (testing "u/par, u/par>, u/lin, u/lin>"

      (is (= (s/mk (u/par e/chan2 e/chan3))
             (s/mk (u/par e/chan2 e/chan3 e/chan3))
             (into (s/mk e/chan2)
                   (s/mk e/chan3))))

      (is (= (s/mk (u/par> e/d1 e/d1))
             (s/mk (u/par e/d1 e/d2))))

      (is (= (s/mk (u/lin e/s1 [e/chan2 e/s3]))
             (s/concat-score (s/mk e/s1) (s/mk e/chan2 e/s3))
             #{{:patch [0 4], :channel 2, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 3}}, :voice 0, :duration 1, :position 1, :velocity 80, :track 0}
               {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1}}, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}}))

      (is (= (s/mk (u/lin> e/d1 e/d1 e/d1))
             (s/mk (u/lin e/d1 e/d2 e/d3)))))

    (testing "each"

      (is (= (s/mk (u/lin e/s0 e/s1 e/s2)
                   (u/each e/d1))
             (s/mk (u/lin [e/s0 e/d1] [e/s1 e/d1] [e/s2 e/d1]))))

      (is (= (s/mk (u/lin e/s0 e/s1 e/s2)
                   (u/each e/d1 e/c1))
             (s/mk (u/lin [e/s0 e/d1 e/c1]
                          [e/s1 e/d1 e/c1]
                          [e/s2 e/d1 e/c1]))
             #{{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 1, :c 1}}, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}
               {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2, :d 1, :c 1}}, :voice 0, :duration 1, :position 2, :velocity 80, :track 0}
               {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 1, :c 1}}, :voice 0, :duration 1, :position 1, :velocity 80, :track 0}}))

      (is (= (s/mk (u/tup e/d0 e/d1 e/d2)
                   (u/each (u/tup e/d0 e/d1 e/d2)))
             (s/mk (u/tup e/d0 e/d1 e/d2
                          e/d1 e/d2 e/d3
                          e/d2 e/d3 e/d4)))))

    (testing "fit u/tup u/tup>"

      (is (= (s/mk (u/fit e/dur2))
             S0))

      (is (= (s/mk (u/fit (u/lin e/d1 e/d2 e/d3)))
             (s/mk (u/tup e/d1 e/d2 e/d3))))

      (is (= (s/score-duration
              (s/mk (u/tup e/d0 e/d2 e/d3)))
             1))

      (is (= (s/mk (u/tup> e/d1 e/d1 e/d1))
             (s/mk (u/tup e/d1 e/d2 e/d3)))))

    (testing "append, superpose"

      (is (= (s/mk (u/append e/d1))
             (s/mk (u/lin u/same e/d1))))

      (is (= (s/mk (u/superpose e/d1))
             (s/mk (u/par u/same e/d1)))))

    (testing "rep rup"

      (is (= (s/mk (u/rep 3 e/d1))
             (s/mk (u/lin u/same e/d1 e/d2))))

      (is (= (s/mk (u/rep 3 e/d1 :skip-first))
             (s/mk (u/lin e/d1 e/d2 e/d3))))

      (is (= (s/mk (u/rup 3 e/d1))
             (s/mk (u/tup u/same e/d1 e/d2))))

      (is (= (s/mk (u/rup 3 e/d1 :skip-first))
             (s/mk (u/tup e/d1 e/d2 e/d3)))))

    (testing "dup dupt"

      (is (= (s/mk (u/dup 3))
             (s/mk (u/lin u/same u/same u/same))))

      (is (= (s/mk (u/dupt 3))
             (s/mk (u/tup u/same u/same u/same)))))

    (testing "u/nlin u/ntup"

      (is (= (s/mk (u/ntup 3 e/d1))
             (s/mk (u/tup e/d1 e/d1 e/d1))))

      (is (= (s/mk (u/nlin 3 e/d1))
             (s/mk (u/lin e/d1 e/d1 e/d1)))))

    (testing "u/parts"

      (is (= S0
             (s/mk (u/parts e/chan2 u/void))
             (s/mk (u/parts e/chan0 u/same))))

      (is (= #{}
             (s/mk (u/parts e/chan0 u/void))))

      (is (= (s/mk (u/par e/chan1 e/chan2)
                   (u/parts e/chan1 e/d1))
             (s/mk (u/par [e/chan1 e/d1] e/chan2))))

      (is (= (s/mk (u/par e/chan1 e/chan2)
                   (u/parts e/chan1 e/d1 e/chan2 e/d2))
             (s/mk (u/par [e/chan1 e/d1] [e/chan2 e/d2])))))

    (testing "repeat-while"

      (is (= (s/mk (u/repeat-while u/within-midi-pitch-bounds?
                                   e/o1
                                   e/o1-))
             (s/mk e/o5)))

      (testing "test can be a regular function"
        (is (= (s/mk (u/repeat-while (fn [s] (< (count s) 8))
                                     (u/dup 2)))
               (s/mk (u/dup 8)))))

      (testing "test empty set return is interpreted as failure"
        (is (= (s/mk (u/repeat-while (s/sf_ #{})
                                     (u/tup e/d0 e/d1)))
               (s/mk (u/tup e/d0 e/d1)))))

      (testing "throwing when update or after are not updates"
        (is (thrown? #?(:clj Exception
                        :cljs js/Error) (u/repeat-while :not-an-update u/same)))
        (is (thrown? #?(:clj Exception
                        :cljs js/Error) (u/repeat-while u/same u/same :not-an-update)))))

    (testing "fst fst-that"

      (is (= (s/mk (u/fst u/void
                          e/d2))
             (s/mk (u/fst u/void
                          u/void
                          e/d2
                          u/void))
             (s/mk (u/fst e/d2
                          u/void))
             (s/mk e/d2)))

      (is (= (s/mk (u/fst-that (u/within-pitch-bounds? :C0 :G0)
                               e/o1
                               e/d5
                               e/d2
                               e/d1))
             (s/mk e/d2)))

      (is (= (s/mk (u/fst-that (u/within-pitch-bounds? :C0 :G0)
                               e/o1
                               e/d1
                               e/d2))
             (s/mk e/d1)))

      (is (= (s/mk (u/fst-that (u/within-time-bounds? 0 1)
                               (u/dup 3)
                               (u/lin e/d0 e/d1)
                               (u/tup u/_ u/_)))
             (s/mk (u/dupt 2)))))

    (testing "shrink"

      (is (= (s/mk (u/chans (u/dupt 3)
                            (u/dup 3))
                   (u/shrink e/chan1))
             (s/mk [e/chan1 (u/dup 3)])))

      (is (= (s/mk (u/lin e/d0 e/d1 e/d2)
                   (u/shrink {:position (numbers/lt 2)}))
             (s/mk (u/lin e/d0 e/d1)))))

    (testing "adjust"

      (is (= (s/mk (u/dup 4)
                   (u/adjust {:duration 2}))
             (s/mk (u/dupt 2)
                   (u/dup 2))))

      (is (= (s/mk (u/dup 4)
                   (u/adjust {:duration 2 :position 2}))
             (s/mk e/dur:2
                   (u/dup 4)
                   (s/sf_ (s/shift-score _ 2))))))

    (testing "in-place"

      (is (score= (s/mk (u/lin e/d0 e/d1 e/d2)
                        (u/adjust {:position 3 :duration 2})
                        (u/in-place (u/dup 3)))
                  (s/mk (u/lin e/d0 e/d1 e/d2)
                        (u/dup 3)
                        (u/adjust {:position 3 :duration 2})))))

    (testing "fork-with, voices, u/chans, tracks"

      (is (= (s/mk (u/fork-with (fn [i] (e/vel (* 10 i)))
                                e/s1
                                e/o1))
             (s/mk (u/par [e/s1 (e/vel 0)] [e/o1 (e/vel 10)]))))

      (is (= (s/mk (u/voices e/d0 e/d1 e/d2))
             (s/mk (u/par [(e/voice 0) e/d0]
                          [(e/voice 1) e/d1]
                          [(e/voice 2) e/d2]))))

      (is (= (s/mk (u/chans e/d0 e/d1 e/d2))
             (s/mk (u/par [e/chan0 e/d0]
                          [e/chan1 e/d1]
                          [e/chan2 e/d2]))))

      (is (= (s/mk (u/tracks e/d0 e/d1 e/d2))
             (s/mk (u/par [e/track0 e/d0]
                          [e/track1 e/d1]
                          [e/track2 e/d2])))))

    (testing "mirror rev"

      (is (score= (s/mk (u/tup e/d0 e/d1 e/d2)
                        u/rev)
                  (s/mk (u/tup e/d2 e/d1 e/d0))
                  (s/mk (u/tup e/d2 e/d1 e/d0)
                        u/rev u/rev)))

      (is (= (s/numerify-pitches
              (s/mk (u/tup e/d0 e/d1 e/d2)
                    (u/mirror :C0)))
             (s/numerify-pitches
              (s/mk (u/tup e/d0 e/c2- e/c4-)))))

      (is (= (s/numerify-pitches
              (s/mk (u/tup e/d0 e/d1 e/d2)
                    (u/mirror :G0)))
             (s/numerify-pitches
              (s/mk (u/tup [e/o1 e/d1] e/o1 [e/o1 e/c2-]))))))

    (testing "event-scale"

      (is (= (s/mk (u/tup (e/vel 0) (e/vel 60) (e/vel 120))
                   (u/event-scale :velocity [30 60]))
             (s/mk (u/tup (e/vel 30) (e/vel 45) (e/vel 60))))))

    (testing "selection"

      (testing "min-by max-by"

        (is (= (s/mk (u/tup e/d0 e/d1 e/d2)
                     (u/min-by :position))
               (s/mk [e/dur:3 e/d0])))

        (is (= (s/mk (u/tup e/d0 [e/vel12 e/d1] e/d2)
                     (u/max-by :velocity))
               (s/mk [e/dur:3 e/d1 e/vel12]
                     (u/adjust {:position (/ 1 3)}))))

        (is (= (s/mk (u/lin e/d0 e/d1 e/d2)
                     u/min-pitch)
               (s/mk e/d0)))

        (is (= (s/mk (u/lin e/d0 e/d1 e/d2)
                     u/max-pitch)
               (s/mk e/d2
                     (u/adjust {:position 2})))))

      (testing "time"

        (is (= (s/mk (u/lin e/d0 e/d1 e/d2)
                     (u/from 1))
               (s/mk (u/lin e/d0 e/d1 e/d2)
                     (u/from 0.5))
               (s/mk (u/lin e/d1 e/d2)
                     (u/adjust {:position 1}))))

        (is (= (s/mk (u/lin e/d0 e/d1 e/d2)
                     (u/until 2))
               (s/mk (u/lin e/d0 e/d1 e/d2)
                     (u/until 1.5))
               (s/mk (u/lin e/d0 e/d1))))

        (is (= (s/mk (u/lin e/d0 e/d1 e/d2)
                     (u/between 1 2))
               (s/mk (u/lin e/d0 e/d1 e/d2)
                     (u/between 0.5 1.5))
               (s/mk e/d1 (u/adjust {:position 1}))))

        (is (= (s/mk (u/lin e/d0 e/d1 e/d2)
                     (u/start-from 1))
               (s/mk (u/lin e/d1 e/d2))))

        (is (= (s/mk (u/lin e/d0 e/d1 e/d2)
                     u/start-from-last)
               (s/mk (u/lin e/d0 e/d1 e/d2)
                     (u/start-from-nth-last 1))
               (s/mk e/d2)))

        (is (= (s/mk (u/lin e/d0 e/d1 e/d2)
                     (u/start-from-nth-last 2))
               (s/mk (u/lin e/d1 e/d2))))

        (is (nil? (s/mk (u/lin e/d0 e/d1 e/d2)
                        (u/start-from-nth-last 4))))

        (testing "trim"

          (is (= (s/mk (u/lin e/d0 e/d1 e/d2)
                       (u/trim 1 2))
                 (s/mk e/d1
                       (u/adjust {:position 1}))))

          (is (= (s/mk (u/lin e/d0 e/d1 e/d2)
                       (u/trim 0 3))
                 (s/mk (u/lin e/d0 e/d1 e/d2))))

          (is (= #{}
                 (s/mk (u/lin e/d0 e/d1 e/d2)
                       (u/trim 0 0))))

          (is ((e/->event-matcher {:trimed-fw true
                                   :duration (/ 1 2)
                                   :position 0})
               (first (s/mk (u/trim 0 (/ 1 2))))))

          (is ((e/->event-matcher {:trimed-bw true
                                   :duration (/ 1 2)
                                   :position (/ 1 2)})
               (first (s/mk (u/trim (/ 1 2) 1))))))

        (testing "only-between"

          (is (score= (s/mk (u/lin e/d0 e/d1 e/d2 e/d3)
                            (u/only-between 1 3 (u/each (u/tup e/d0 e/d1 e/d2))))
                      (s/mk (u/lin e/d0 (u/tup e/d1 e/d2 e/d3) (u/tup e/d2 e/d3 e/d4) e/d3)))))

        (testing "checks"

          (let [f (u/within-bounds? :velocity 30 60)]
            (is (and (not (s/mk e/vel1 f))
                     (s/mk e/vel3 f)
                     (s/mk (e/vel 60) f)
                     (s/mk (e/vel 30) f)
                     (s/mk e/vel5 f)
                     (not (s/mk e/vel7 f))
                     (not (s/mk e/vel9 f))
                     (not (s/mk e/vel11 f)))))

          (is (s/mk (u/lin e/d0 e/d1 e/d2)
                    (u/within-time-bounds? 0 3)))

          (is (not (s/mk (u/lin e/d0 e/d1 e/d2 e/d3)
                         (u/within-time-bounds? 0 3))))

          (is (s/mk (u/lin e/d0 e/d1 e/d2)
                    (u/within-pitch-bounds? :C0 :E0)))

          (is (not (s/mk (u/lin e/d0 e/d1 e/d2)
                         (u/within-pitch-bounds? :C0 :E/D0))))

          (is (not (s/mk e/o8 u/within-midi-pitch-bounds?)))
          (is (not (s/mk e/o8- u/within-midi-pitch-bounds?)))
          (is (s/mk u/within-midi-pitch-bounds?))
          (is (s/mk (u/lin e/o1 e/o2 e/o3)
                    u/within-midi-pitch-bounds?)))

        (testing "non determinism"

          (is (pr/with-rand 0
                (= (s/mk (u/one-of e/chan1 e/chan2 e/chan3))
                   #?(:clj (s/mk e/chan3)
                      :cljs (s/mk e/chan2)))))

          (is (pr/with-rand -78
                (= (s/mk (u/one-of e/chan1 e/chan2 e/chan3))
                   #?(:clj (s/mk e/chan1)
                      :cljs (s/mk e/chan2)))))

          (is (every? (e/->event-matcher {:channel (partial contains? #{0 1 2 3})})
                      (s/mk (u/nlin 100 (u/maybe e/chan1 e/chan2 e/chan3)))))

          (is (every? (e/->event-matcher {:channel (partial contains? #{0 1 2 3})})
                      (s/mk (u/nlin 100 (u/probs {e/chan1 1 e/chan2 2 e/chan3 7})))))

          (testing "mix and shuf"

            (is (= (sort (map e/pitch-value (s/mk (u/shuftup e/d0 e/d1 e/d2))))
                   (sort (map e/pitch-value (s/mk (u/mixtup e/d0 e/d1 e/d2))))
                   (sort (map e/pitch-value (s/mk (u/tup e/d0 e/d1 e/d2))))
                   (sort (map e/pitch-value (s/mk (u/shuflin e/d0 e/d1 e/d2))))
                   (sort (map e/pitch-value (s/mk (u/mixlin e/d0 e/d1 e/d2))))
                   (sort (map e/pitch-value (s/mk (u/lin e/d0 e/d1 e/d2))))))))

        (testing "incubating"

          (testing "fill fill>"

            (is (score= (s/mk (u/fill (/ 1 4) e/d3))
                        (s/mk (u/tup e/d3 e/d3 e/d3 e/d3))))

            (is (score= (s/mk e/dur2 (u/fill (/ 1 4) e/d3))
                        (s/mk e/dur2 (u/ntup 8 e/d3))))

            (is (score= (s/mk (u/fill> (/ 1 4) e/d3))
                        (s/mk (u/ntup> 4 e/d3))))

            (is (score= (s/mk e/dur2 (u/fill> (/ 2 5) e/d3))
                        (s/mk e/dur2 (u/ntup> 5 e/d3))))

            (is (thrown? #?(:clj Exception
                            :cljs js/Error)
                         (s/mk (u/fill (/ 2 3) e/d1)))))

          (testing "connect-by scan"

            (is (= (s/mk (u/lin e/s0 e/s2 e/s4)
                         (u/scan :position 2 1 s/merge-scores))
                   (s/mk (u/lin e/s0 e/s2 e/s4))))

            (is (= (s/mk (u/lin e/s0 e/s2 e/s4)
                         (u/connect-by :position into))
                   (s/mk (u/lin e/s0 e/s2 e/s4))))

            (is (= (s/mk (u/lin e/s0 e/s2 e/s4)
                         (u/connect-by :position
                                       (fn [a b]
                                         (s/update-score a
                                                         (u/in-place (u/tup u/same [(e/ef_ (conj _ (find (first b) :pitch))) e/d1]))))))
                   (s/mk (u/lin (u/tup e/s0 [e/s2 e/d1]) (u/tup e/s2 [e/s4 e/d1]) e/s4))))))))))


'(deftest new-rep
  (testing "new rep"
    (play (newrep 5))
    (play (newrep 5 (one-of e/d1 e/d2 e/d3 d4) :fit))
    (play (newrep 5 (one-of e/s0 e/s1 e/s2 e/s3 s4) :u/par))

    (play (iter 7 e/d1 :fit true))
    (play (iter e/d1 :take 7 :fit true))
    (play (iter e/d1 :drop 1 :take 7 :fit true))
    (play (iter e/d1 :next true :take 7 :fit true))
    (play (iter 7 e/d1 :fit :append))
    (play (iter 4 e/d1 :next true))
    (play (iter 4 e/d3 :par true)
          (iter 4 (one-of e/d1- e/d2))
          (iter 4 (transpose c3-)))))
