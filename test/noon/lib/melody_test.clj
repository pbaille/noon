(ns noon.lib.melody-test
  (:require [noon.lib.melody :as m]
            [noon.test :as t]
            [clojure.test :refer [deftest testing is]]
            [noon.score :as n]
            [noon.utils.pseudo-random :as pr]))

(deftest main

  (testing "misc"
    (is (= (m/layer-split :s
                          (n/mk (n/tup n/s0 n/s1 n/s2)
                                (n/$ (n/tup n/d0 n/d1 n/d2))))
           (list {:position 2/3, :layer-idx 2, :score #{{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2, :d 1}}, :voice 0, :duration 1/9, :position 7/9, :velocity 80, :track 0} {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2, :d 0}}, :voice 0, :duration 1/9, :position 2/3, :velocity 80, :track 0} {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2, :d 2}}, :voice 0, :duration 1/9, :position 8/9, :velocity 80, :track 0}}}
                 {:position 1/3, :layer-idx 1, :score #{{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 0}}, :voice 0, :duration 1/9, :position 1/3, :velocity 80, :track 0} {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 2}}, :voice 0, :duration 1/9, :position 5/9, :velocity 80, :track 0} {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 1}}, :voice 0, :duration 1/9, :position 4/9, :velocity 80, :track 0}}}
                 {:position 0N, :layer-idx 0, :score #{{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0}}, :voice 0, :duration 1/9, :position 0N, :velocity 80, :track 0} {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 1}}, :voice 0, :duration 1/9, :position 1/9, :velocity 80, :track 0} {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 2}}, :voice 0, :duration 1/9, :position 2/9, :velocity 80, :track 0}}}))))

  (testing "permutations"
    (is (t/freeze :perm1
                  (pr/with-rand 0
                    (n/mk (n/rup 8 n/d1)
                          (m/permutation {:grade 5 :layer :d})))))

    (is (t/freeze :perm2
                  (pr/with-rand 0
                    (n/mk n/dur2
                          (n/tup n/s0 n/s1 n/s2 n/s3)
                          (n/$ (n/tup n/d1 n/d1- n/d0))
                          (m/permutation :rand {:layer :s})))))

    (is (t/freeze :perm3
                  (n/mk n/dur2
                        (n/tup n/s0 n/s1 n/s2)
                        (n/$ (n/rup 4 n/d1))
                        (m/permutation 0 {:layers [:s :d]})))))

  (testing "contour"
    (is (t/freeze :contour1
                  (pr/with-rand 0 (n/mk (n/tup n/d0 n/d1 n/d2)
                                        (m/contour :mirror {:layer :d})))))

    (is (t/freeze :contour2
                  (pr/with-rand 0
                    (n/mk n/dur:2
                          (n/shufcat n/s0 n/s1 n/s2 n/s4)
                          (n/$ (n/tup n/d0 n/c1- n/d1 n/d0))
                          (n/cat n/same
                                 [n/dur:4 n/vel0]
                                 (m/contour :mirror {:layer :s})
                                 [n/dur:4 n/vel0]
                                 (m/contour :similar {:extent [4 4] :layer :s})
                                 [n/dur:4 n/vel0]
                                 (m/contour :rotation {:layer :s})))))))

  (testing "gen"

    (is (t/freeze :simple-line1
                  (pr/with-rand 0
                    (n/mk (m/simple-line 32 (n/one-of n/s1 n/s1- n/d1 n/d1-))
                          (n/adjust 4)))))

    (is (t/freeze :simple-line2
                  (pr/with-rand 0
                    (n/mk (m/simple-line 32 (n/one-of n/s1 n/s1- (n/tup n/d1 n/d0 n/s2-) (n/tup n/d1- n/d0 n/s2)))
                          (n/adjust 8)))))

    (is (t/freeze :simple-line3
                  (pr/with-rand 0
                    (n/mk n/dur:4
                          (m/simple-line 64
                                         (n/one-of (n/catn> 4 (n/one-of n/d1- n/d1))
                                                   (n/tup n/d1 n/d1- n/s0)
                                                   (n/cat n/s2 n/s1 n/s1-)
                                                   (n/catn> 4 (n/one-of n/s1- n/s1))))
                          (n/chans (n/patch :electric-piano-1)
                                   [(n/patch :ocarina) n/o1 (n/$ n/d3)])))))

    (is (t/freeze :simple-line4
                  (pr/with-rand 0
                    (n/mk {:description "another way to build a melodic line from a bunch of randomly chosen updates"}
                          (n/patch :acoustic-guitar-nylon)
                          (n/while (n/within-time-bounds? 0 32)
                            (n/append [n/start-from-last
                                       (n/any-that (n/within-pitch-bounds? :C-1 :C2)
                                                   (n/rep 3 n/d3 :skip-first)
                                                   (n/rep 3 n/d3- :skip-first)
                                                   n/d1 n/d1-)]))
                          (n/adjust 3)))))

    (is (t/freeze :genline1
                  (pr/with-rand 0 (n/mk n/dur:2
                                        (n/patch :whistle)
                                        (m/gen-line {:contour [6 4] :grow 6 :layer :c})
                                        (n/append> n/c3 n/c4 n/c2 n/rev (n/$ (n/maybe n/o1 n/o1-)))))))

    (is (= (pr/with-rand 0
             (take 10 (m/step-seqs {:length 5
                                    :delta 0
                                    :bounds [-2 6]
                                    :step-range [-4 4]})))
           '([-1 1 2 -4 2] (-2 3 -2 3 -2) (-2 4 -2 -2 2) (-1 -1 4 -3 1) [-2 1 2 3 -4] (4 -2 -1 2 -3) (-2 3 -3 4 -2) (3 -3 3 -4 1) (2 -3 2 3 -4) [-1 3 -3 4 -3])))

    (is (t/freeze :gen-tup1
                  (pr/with-rand 0
                    (n/mk n/dur:2
                          (n/catn 100 (n/! (m/gen-tup :c 6 6 {:bounds [-12 12]
                                                              :step-range [-7 7]})))))))))
