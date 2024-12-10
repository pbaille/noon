(ns noon.score-test
  (:require [noon.score :as s]
            [noon.updates :as u]
            [noon.events :as e]
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
    (is (= (s/score-duration (s/update-score S0 u/dur2))
           2))
    (is (= (s/score-duration (s/mk (u/lin u/s0 u/s2 u/s4)))
           3))

    (is (= (s/score-track-count S0)
           1))
    (is (= (s/score-track-count (s/mk (u/superpose u/track1)))
           2))

    (is (= (s/score-bounds S0 :position)
           [{:position 0, :channel 0, :track 0, :duration 1, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}, :velocity 80, :voice 0, :patch [0 4]}
            {:position 0, :channel 0, :track 0, :duration 1, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}, :velocity 80, :voice 0, :patch [0 4]}]))

    (is (= (s/score-bounds (s/mk (u/lin u/s0 u/s2 u/s4)) :position)
           [{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0}}, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}
            {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 4}}, :voice 0, :duration 1, :position 2, :velocity 80, :track 0}]))

    (is (= (s/score-origin S0)
           0))
    (is (= (s/score-origin (s/update-score S0 {:position (numbers/add 10)}))
           10))

    (is (= (s/pitch-value-bounds S0)
           [60 60]))
    (is (= (s/pitch-value-bounds (s/mk (u/lin u/s0 u/s2 u/s4)))
           [60 76])))

  (testing "transformations"

    (testing "map-event-update"

      (is (= (s/map-event-update (s/mk (u/lin u/d0 u/d1 u/d2)) u/d1)
             (s/mk (u/lin u/d1 u/d2 u/d3))))

      (is (= (s/map-event-update (s/mk (u/lin u/d0 u/d1 u/d2)) u/vel4)
             (s/mk u/vel4 (u/lin u/d0 u/d1 u/d2))))

      (testing "can remove events by returning nil"

        (is (= (count (s/map-event-update (s/mk (u/lin u/d0 u/d1 u/d2))
                                          (fn [e] (if (not (zero? (e/pitch-class-value e))) e))))
               2))))

    (testing "scale score, shift score"

      (is (= (s/score-duration
              (s/scale-score (s/mk (u/lin u/s0 u/s2 u/s4))
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

      (is (let [s0 (s/mk (u/lin u/s0 u/s2 u/s4))
                s1 (s/fit-score s0 event0)
                s2 (s/normalise-score s0)]
            (and (= s1 s2)
                 (= 1 (s/score-duration s1))
                 (= 0 (s/score-origin s2)))))

      (is (let [s (s/fit-score (s/mk (u/lin u/s0 u/s2 u/s4))
                               {:position 3 :duration 2})]
            (and (= 5 (s/score-duration s))
                 (= 3 (s/score-origin s))))))

    (is (= (s/reverse-score (s/mk (u/lin u/s0 u/s2 u/s4)))
           (s/mk (u/lin u/s4 u/s2 u/s0))))

    (testing "filter-score"

      (is (= (s/mk (u/lin u/d0 u/d1))
             (s/filter-score (s/mk (u/lin u/d0 u/d1)) map?)))

      (is (= (s/mk (u/lin u/d0 u/d1))
             (s/filter-score (s/mk (u/lin u/d0 u/d1)) {:channel 0})
             (s/filter-score (s/mk (u/lin u/d0 u/d1)) u/chan0)))

      (is (= (s/filter-score (s/mk (u/chans (u/lin u/d0 u/d1)
                                            (u/lin u/d3 u/d4)))
                             u/chan0)
             (s/mk (u/lin u/d0 u/d1)))))

    (testing "midi prepare helpers"

      (is (= (s/numerify-pitches (s/mk (u/lin u/s0 u/s2 u/s4)))
             #{{:patch [0 4], :channel 0, :pitch 60, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}
               {:patch [0 4], :channel 0, :pitch 67, :voice 0, :duration 1, :position 1, :velocity 80, :track 0}
               {:patch [0 4], :channel 0, :pitch 76, :voice 0, :duration 1, :position 2, :velocity 80, :track 0}}))

      (is (= (->> (s/dedupe-patches-and-control-changes (s/mk (u/lin u/s0 u/s2 u/s4)))
                  (s/sort-score)
                  (map (juxt :position :patch :cc)))
             (list [0 [0 4] nil] [1 nil nil] [2 nil nil])))

      (is (= (->> (s/mk (u/lin u/s0
                               [(u/patch :vibraphone)
                                (u/cc :volume 70)
                                (u/lin u/s2 u/s4)]))
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

    (is (= (s/merge-scores [(s/mk (u/lin u/d0 u/d1 u/d2))
                            (s/mk (u/lin u/d1 u/d2 u/d3))])
           (s/mk (u/lin (u/par u/d0 u/d1) (u/par u/d1 u/d2) (u/par u/d2 u/d3))))))

  (testing "chunk-score"

    (is (= (s/chunk-score (s/mk (u/lin u/d0 (u/par u/d1 u/d2) u/d3))
                          :position)
           (list (s/mk u/d0)
                 (s/mk (u/par u/d1 u/d2) (u/adjust {:position 1}))
                 (s/mk u/d3 (u/adjust {:position 2}))))))

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

      (is (= (s/map-score-update (s/mk (u/lin u/d0 u/d1 u/d2))
                                 u/same)
             (s/mk (u/lin u/d0 u/d1 u/d2))))

      (is (= (s/map-score-update (s/mk (u/lin u/d0 u/d1 u/d2))
                                 (u/tup u/d0 u/d3))
             (s/mk (u/lin (u/tup u/d0 u/d3)
                          (u/tup u/d1 u/d4)
                          (u/tup u/d2 u/d5))))))

    (testing "update-score"

      (is (= (s/update-score S0 (s/sf_ _))
             S0))

      (is (= (s/update-score S0 u/dur2)
             (s/mk u/dur2)))

      (is (= (s/update-score S0 [u/vel10 u/dur2])
             (s/mk u/dur2 u/vel10)))

      (is (= (s/update-score S0 (g/one-of u/vel10))
             (s/mk u/vel10)))

      (is (= (s/update-score S0 (u/par u/vel5 u/d1))
             (s/mk (u/par u/vel5 u/d1))
             (into (s/update-score S0 u/vel5)
                   (s/update-score S0 u/d1))))

      (is (thrown? #?(:clj Exception
                      :cljs js/Error)
                   (s/update-score :not-an-update S0))))

    (testing "partial update"

      (is (= S0 (s/partial-update S0 u/chan1 u/vel4)))

      (is (= (s/partial-update (s/mk (u/chans (u/tup u/d0 u/d1 u/d2)
                                              u/o1-))
                               u/chan1
                               (u/tup u/s0 u/s1 u/s2))
             (s/mk (u/chans (u/tup u/d0 u/d1 u/d2)
                            [u/o1- (u/tup u/s0 u/s1 u/s2)])))))

    (testing "map-update"

      (is (= (s/map-update S0 u/vel2)
             (s/mk u/vel2)))

      (is (= (s/map-update (s/mk (u/lin u/s0 u/s1)) u/vel2)
             (s/mk u/vel2 (u/lin u/s0 u/s1))))

      (is (= (s/map-update (s/mk (u/tup u/s0 u/s2))
                           (u/tup u/d0 u/d3))
             (s/mk (u/tup [u/s0 (u/tup u/d0 u/d3)]
                          [u/s2 (u/tup u/d0 u/d3)])))))))


'(deftest new-rep
  (testing "new rep"
    (play (newrep 5))
    (play (newrep 5 (one-of u/d1 u/d2 u/d3 d4) :fit))
    (play (newrep 5 (one-of u/s0 u/s1 u/s2 u/s3 s4) :u/par))

    (play (iter 7 u/d1 :fit true))
    (play (iter u/d1 :take 7 :fit true))
    (play (iter u/d1 :drop 1 :take 7 :fit true))
    (play (iter u/d1 :next true :take 7 :fit true))
    (play (iter 7 u/d1 :fit :append))
    (play (iter 4 u/d1 :next true))
    (play (iter 4 u/d3 :par true)
          (iter 4 (one-of u/d1- u/d2))
          (iter 4 (transpose c3-)))))
