(ns noon.updates-test
  (:require [noon.score :as s]
            [noon.updates :as u]
            [noon.events :as e]
            [noon.utils.pseudo-random :as pr]
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

(deftest updates

  (testing "same, _, k"

    (is (= (s/mk u/same)
           (s/mk u/_)
           S0))

    (is (= (s/mk (u/k u/_))
           S0))

    (is (= (s/mk (u/tup u/d0 u/d1 u/d2)
                 (u/k (u/lin u/s0 u/s1)))
           (s/mk (u/lin u/s0 u/s1))))

    (is (= (s/mk u/void)
           (s/mk (u/tup u/d0 u/d1 u/d2) u/void)
           #{})))

  (testing "chain"

    (is (= (s/mk (u/chain u/chan2 u/vel2))
           (s/mk (u/chain (u/chain u/chan2) (u/chain u/vel2)))
           (s/mk [u/chan2 u/vel2])
           (s/update-score (s/update-score S0 u/chan2)
                           u/vel2)))

    (is (= S0 (s/mk (u/chain)))))

  (testing "u/par, u/par>, u/lin, u/lin>"

    (is (= (s/mk (u/par u/chan2 u/chan3))
           (s/mk (u/par u/chan2 u/chan3 u/chan3))
           (into (s/mk u/chan2)
                 (s/mk u/chan3))))

    (is (= (s/mk (u/par> u/d1 u/d1))
           (s/mk (u/par u/d1 u/d2))))

    (is (= (s/mk (u/lin u/s1 [u/chan2 u/s3]))
           (s/concat-score (s/mk u/s1) (s/mk u/chan2 u/s3))
           #{{:patch [0 4], :channel 2, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 3}}, :voice 0, :duration 1, :position 1, :velocity 80, :track 0}
             {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1}}, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}}))

    (is (= (s/mk (u/lin> u/d1 u/d1 u/d1))
           (s/mk (u/lin u/d1 u/d2 u/d3)))))

  (testing "each"

    (is (= (s/mk (u/lin u/s0 u/s1 u/s2)
                 (u/each u/d1))
           (s/mk (u/lin [u/s0 u/d1] [u/s1 u/d1] [u/s2 u/d1]))))

    (is (= (s/mk (u/lin u/s0 u/s1 u/s2)
                 (u/each u/d1 u/c1))
           (s/mk (u/lin [u/s0 u/d1 u/c1]
                        [u/s1 u/d1 u/c1]
                        [u/s2 u/d1 u/c1]))
           #{{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 1, :c 1}}, :voice 0, :duration 1, :position 0, :velocity 80, :track 0}
             {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2, :d 1, :c 1}}, :voice 0, :duration 1, :position 2, :velocity 80, :track 0}
             {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 1, :c 1}}, :voice 0, :duration 1, :position 1, :velocity 80, :track 0}}))

    (is (= (s/mk (u/tup u/d0 u/d1 u/d2)
                 (u/each (u/tup u/d0 u/d1 u/d2)))
           (s/mk (u/tup u/d0 u/d1 u/d2
                        u/d1 u/d2 u/d3
                        u/d2 u/d3 u/d4)))))

  (testing "fit u/tup u/tup>"

    (is (= (s/mk (u/fit u/dur2))
           S0))

    (is (= (s/mk (u/fit (u/lin u/d1 u/d2 u/d3)))
           (s/mk (u/tup u/d1 u/d2 u/d3))))

    (is (= (s/score-duration
            (s/mk (u/tup u/d0 u/d2 u/d3)))
           1))

    (is (= (s/mk (u/tup> u/d1 u/d1 u/d1))
           (s/mk (u/tup u/d1 u/d2 u/d3)))))

  (testing "append, superpose"

    (is (= (s/mk (u/append u/d1))
           (s/mk (u/lin u/same u/d1))))

    (is (= (s/mk (u/superpose u/d1))
           (s/mk (u/par u/same u/d1)))))

  (testing "rep rup"

    (is (= (s/mk (u/rep 3 u/d1))
           (s/mk (u/lin u/same u/d1 u/d2))))

    (is (= (s/mk (u/rep 3 u/d1 :skip-first))
           (s/mk (u/lin u/d1 u/d2 u/d3))))

    (is (= (s/mk (u/rup 3 u/d1))
           (s/mk (u/tup u/same u/d1 u/d2))))

    (is (= (s/mk (u/rup 3 u/d1 :skip-first))
           (s/mk (u/tup u/d1 u/d2 u/d3)))))

  (testing "dup dupt"

    (is (= (s/mk (u/dup 3))
           (s/mk (u/lin u/same u/same u/same))))

    (is (= (s/mk (u/dupt 3))
           (s/mk (u/tup u/same u/same u/same)))))

  (testing "u/nlin u/ntup"

    (is (= (s/mk (u/ntup 3 u/d1))
           (s/mk (u/tup u/d1 u/d1 u/d1))))

    (is (= (s/mk (u/nlin 3 u/d1))
           (s/mk (u/lin u/d1 u/d1 u/d1)))))

  (testing "u/parts"

    (is (= S0
           (s/mk (u/parts u/chan2 u/void))
           (s/mk (u/parts u/chan0 u/same))))

    (is (= #{}
           (s/mk (u/parts u/chan0 u/void))))

    (is (= (s/mk (u/par u/chan1 u/chan2)
                 (u/parts u/chan1 u/d1))
           (s/mk (u/par [u/chan1 u/d1] u/chan2))))

    (is (= (s/mk (u/par u/chan1 u/chan2)
                 (u/parts u/chan1 u/d1 u/chan2 u/d2))
           (s/mk (u/par [u/chan1 u/d1] [u/chan2 u/d2])))))

  (testing "repeat-while"

    (is (= (s/mk (u/repeat-while u/within-midi-pitch-bounds?
                                 u/o1
                                 u/o1-))
           (s/mk u/o5)))

    (testing "test can be a regular function"
      (is (= (s/mk (u/repeat-while (fn [s] (< (count s) 8))
                                   (u/dup 2)))
             (s/mk (u/dup 8)))))

    (testing "test empty set return is interpreted as failure"
      (is (= (s/mk (u/repeat-while (s/sf_ #{})
                                   (u/tup u/d0 u/d1)))
             (s/mk (u/tup u/d0 u/d1)))))

    (testing "throwing when update or after are not updates"
      (is (thrown? #?(:clj Exception
                      :cljs js/Error) (u/repeat-while :not-an-update u/same)))
      (is (thrown? #?(:clj Exception
                      :cljs js/Error) (u/repeat-while u/same u/same :not-an-update)))))

  (testing "fst fst-that"

    (is (= (s/mk (u/fst u/void
                        u/d2))
           (s/mk (u/fst u/void
                        u/void
                        u/d2
                        u/void))
           (s/mk (u/fst u/d2
                        u/void))
           (s/mk u/d2)))

    (is (= (s/mk (u/fst-that (u/within-pitch-bounds? :C0 :G0)
                             u/o1
                             u/d5
                             u/d2
                             u/d1))
           (s/mk u/d2)))

    (is (= (s/mk (u/fst-that (u/within-pitch-bounds? :C0 :G0)
                             u/o1
                             u/d1
                             u/d2))
           (s/mk u/d1)))

    (is (= (s/mk (u/fst-that (u/within-time-bounds? 0 1)
                             (u/dup 3)
                             (u/lin u/d0 u/d1)
                             (u/tup u/_ u/_)))
           (s/mk (u/dupt 2)))))

  (testing "shrink"

    (is (= (s/mk (u/chans (u/dupt 3)
                          (u/dup 3))
                 (u/shrink u/chan1))
           (s/mk [u/chan1 (u/dup 3)])))

    (is (= (s/mk (u/lin u/d0 u/d1 u/d2)
                 (u/shrink {:position (numbers/lt 2)}))
           (s/mk (u/lin u/d0 u/d1)))))

  (testing "adjust"

    (is (= (s/mk (u/dup 4)
                 (u/adjust {:duration 2}))
           (s/mk (u/dupt 2)
                 (u/dup 2))))

    (is (= (s/mk (u/dup 4)
                 (u/adjust {:duration 2 :position 2}))
           (s/mk u/dur:2
                 (u/dup 4)
                 (s/sf_ (s/shift-score _ 2))))))

  (testing "in-place"

    (is (score= (s/mk (u/lin u/d0 u/d1 u/d2)
                      (u/adjust {:position 3 :duration 2})
                      (u/in-place (u/dup 3)))
                (s/mk (u/lin u/d0 u/d1 u/d2)
                      (u/dup 3)
                      (u/adjust {:position 3 :duration 2})))))

  (testing "fork-with, voices, u/chans, tracks"

    (is (= (s/mk (u/fork-with (fn [i] (u/vel (* 10 i)))
                              u/s1
                              u/o1))
           (s/mk (u/par [u/s1 (u/vel 0)] [u/o1 (u/vel 10)]))))

    (is (= (s/mk (u/voices u/d0 u/d1 u/d2))
           (s/mk (u/par [(u/voice 0) u/d0]
                        [(u/voice 1) u/d1]
                        [(u/voice 2) u/d2]))))

    (is (= (s/mk (u/chans u/d0 u/d1 u/d2))
           (s/mk (u/par [u/chan0 u/d0]
                        [u/chan1 u/d1]
                        [u/chan2 u/d2]))))

    (is (= (s/mk (u/tracks u/d0 u/d1 u/d2))
           (s/mk (u/par [u/track0 u/d0]
                        [u/track1 u/d1]
                        [u/track2 u/d2])))))

  (testing "mirror rev"

    (is (score= (s/mk (u/tup u/d0 u/d1 u/d2)
                      u/rev)
                (s/mk (u/tup u/d2 u/d1 u/d0))
                (s/mk (u/tup u/d2 u/d1 u/d0)
                      u/rev u/rev)))

    (is (= (s/numerify-pitches
            (s/mk (u/tup u/d0 u/d1 u/d2)
                  (u/mirror :C0)))
           (s/numerify-pitches
            (s/mk (u/tup u/d0 u/c2- u/c4-)))))

    (is (= (s/numerify-pitches
            (s/mk (u/tup u/d0 u/d1 u/d2)
                  (u/mirror :G0)))
           (s/numerify-pitches
            (s/mk (u/tup [u/o1 u/d1] u/o1 [u/o1 u/c2-]))))))

  (testing "event-scale"

    (is (= (s/mk (u/tup (u/vel 0) (u/vel 60) (u/vel 120))
                 (u/event-scale :velocity [30 60]))
           (s/mk (u/tup (u/vel 30) (u/vel 45) (u/vel 60))))))

  (testing "selection"

    (testing "min-by max-by"

      (is (= (s/mk (u/tup u/d0 u/d1 u/d2)
                   (u/min-by :position))
             (s/mk [u/dur:3 u/d0])))

      (is (= (s/mk (u/tup u/d0 [u/vel12 u/d1] u/d2)
                   (u/max-by :velocity))
             (s/mk [u/dur:3 u/d1 u/vel12]
                   (u/adjust {:position (/ 1 3)}))))

      (is (= (s/mk (u/lin u/d0 u/d1 u/d2)
                   u/min-pitch)
             (s/mk u/d0)))

      (is (= (s/mk (u/lin u/d0 u/d1 u/d2)
                   u/max-pitch)
             (s/mk u/d2
                   (u/adjust {:position 2})))))

    (testing "time"

      (is (= (s/mk (u/lin u/d0 u/d1 u/d2)
                   (u/from 1))
             (s/mk (u/lin u/d0 u/d1 u/d2)
                   (u/from 0.5))
             (s/mk (u/lin u/d1 u/d2)
                   (u/adjust {:position 1}))))

      (is (= (s/mk (u/lin u/d0 u/d1 u/d2)
                   (u/until 2))
             (s/mk (u/lin u/d0 u/d1 u/d2)
                   (u/until 1.5))
             (s/mk (u/lin u/d0 u/d1))))

      (is (= (s/mk (u/lin u/d0 u/d1 u/d2)
                   (u/between 1 2))
             (s/mk (u/lin u/d0 u/d1 u/d2)
                   (u/between 0.5 1.5))
             (s/mk u/d1 (u/adjust {:position 1}))))

      (is (= (s/mk (u/lin u/d0 u/d1 u/d2)
                   (u/start-from 1))
             (s/mk (u/lin u/d1 u/d2))))

      (is (= (s/mk (u/lin u/d0 u/d1 u/d2)
                   u/start-from-last)
             (s/mk (u/lin u/d0 u/d1 u/d2)
                   (u/start-from-nth-last 1))
             (s/mk u/d2)))

      (is (= (s/mk (u/lin u/d0 u/d1 u/d2)
                   (u/start-from-nth-last 2))
             (s/mk (u/lin u/d1 u/d2))))

      (is (nil? (s/mk (u/lin u/d0 u/d1 u/d2)
                      (u/start-from-nth-last 4))))

      (testing "trim"

        (is (= (s/mk (u/lin u/d0 u/d1 u/d2)
                     (u/trim 1 2))
               (s/mk u/d1
                     (u/adjust {:position 1}))))

        (is (= (s/mk (u/lin u/d0 u/d1 u/d2)
                     (u/trim 0 3))
               (s/mk (u/lin u/d0 u/d1 u/d2))))

        (is (= #{}
               (s/mk (u/lin u/d0 u/d1 u/d2)
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

        (is (score= (s/mk (u/lin u/d0 u/d1 u/d2 u/d3)
                          (u/only-between 1 3 (u/each (u/tup u/d0 u/d1 u/d2))))
                    (s/mk (u/lin u/d0 (u/tup u/d1 u/d2 u/d3) (u/tup u/d2 u/d3 u/d4) u/d3)))))

      (testing "checks"

        (let [f (u/within-bounds? :velocity 30 60)]
          (is (and (not (s/mk u/vel1 f))
                   (s/mk u/vel3 f)
                   (s/mk (u/vel 60) f)
                   (s/mk (u/vel 30) f)
                   (s/mk u/vel5 f)
                   (not (s/mk u/vel7 f))
                   (not (s/mk u/vel9 f))
                   (not (s/mk u/vel11 f)))))

        (is (s/mk (u/lin u/d0 u/d1 u/d2)
                  (u/within-time-bounds? 0 3)))

        (is (not (s/mk (u/lin u/d0 u/d1 u/d2 u/d3)
                       (u/within-time-bounds? 0 3))))

        (is (s/mk (u/lin u/d0 u/d1 u/d2)
                  (u/within-pitch-bounds? :C0 :E0)))

        (is (not (s/mk (u/lin u/d0 u/d1 u/d2)
                       (u/within-pitch-bounds? :C0 :U/D0))))

        (is (not (s/mk u/o8 u/within-midi-pitch-bounds?)))
        (is (not (s/mk u/o8- u/within-midi-pitch-bounds?)))
        (is (s/mk u/within-midi-pitch-bounds?))
        (is (s/mk (u/lin u/o1 u/o2 u/o3)
                  u/within-midi-pitch-bounds?)))

      (testing "non determinism"

        (is (pr/with-rand 0
              (= (s/mk (u/one-of u/chan1 u/chan2 u/chan3))
                 #?(:clj (s/mk u/chan3)
                    :cljs (s/mk u/chan2)))))

        (is (pr/with-rand -78
              (= (s/mk (u/one-of u/chan1 u/chan2 u/chan3))
                 #?(:clj (s/mk u/chan1)
                    :cljs (s/mk u/chan2)))))

        (is (every? (e/->event-matcher {:channel (partial contains? #{0 1 2 3})})
                    (s/mk (u/nlin 100 (u/maybe u/chan1 u/chan2 u/chan3)))))

        (is (every? (e/->event-matcher {:channel (partial contains? #{0 1 2 3})})
                    (s/mk (u/nlin 100 (u/probs {u/chan1 1 u/chan2 2 u/chan3 7})))))

        (testing "mix and shuf"

          (is (= (sort (map e/pitch-value (s/mk (u/shuftup u/d0 u/d1 u/d2))))
                 (sort (map e/pitch-value (s/mk (u/mixtup u/d0 u/d1 u/d2))))
                 (sort (map e/pitch-value (s/mk (u/tup u/d0 u/d1 u/d2))))
                 (sort (map e/pitch-value (s/mk (u/shuflin u/d0 u/d1 u/d2))))
                 (sort (map e/pitch-value (s/mk (u/mixlin u/d0 u/d1 u/d2))))
                 (sort (map e/pitch-value (s/mk (u/lin u/d0 u/d1 u/d2))))))))

      (testing "incubating"

        (testing "fill fill>"

          (is (score= (s/mk (u/fill (/ 1 4) u/d3))
                      (s/mk (u/tup u/d3 u/d3 u/d3 u/d3))))

          (is (score= (s/mk u/dur2 (u/fill (/ 1 4) u/d3))
                      (s/mk u/dur2 (u/ntup 8 u/d3))))

          (is (score= (s/mk (u/fill> (/ 1 4) u/d3))
                      (s/mk (u/ntup> 4 u/d3))))

          (is (score= (s/mk u/dur2 (u/fill> (/ 2 5) u/d3))
                      (s/mk u/dur2 (u/ntup> 5 u/d3))))

          (is (thrown? #?(:clj Exception
                          :cljs js/Error)
                       (s/mk (u/fill (/ 2 3) u/d1)))))

        (testing "connect-by scan"

          (is (= (s/mk (u/lin u/s0 u/s2 u/s4)
                       (u/scan :position 2 1 s/merge-scores))
                 (s/mk (u/lin u/s0 u/s2 u/s4))))

          (is (= (s/mk (u/lin u/s0 u/s2 u/s4)
                       (u/connect-by :position into))
                 (s/mk (u/lin u/s0 u/s2 u/s4))))

          (is (= (s/mk (u/lin u/s0 u/s2 u/s4)
                       (u/connect-by :position
                                     (fn [a b]
                                       (s/update-score a
                                                       (u/in-place (u/tup u/same [(e/ef_ (conj _ (find (first b) :pitch))) u/d1]))))))
                 (s/mk (u/lin (u/tup u/s0 [u/s2 u/d1]) (u/tup u/s2 [u/s4 u/d1]) u/s4)))))))))
