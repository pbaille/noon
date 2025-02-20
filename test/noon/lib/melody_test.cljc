(ns noon.lib.melody-test
  (:require [noon.lib.melody :as m]
            [clojure.test :as t :refer [deftest testing]]
            [noon.eval :refer [score]]
            [noon.freeze :refer [freeze]]
            #?(:clj [noon.utils.pseudo-random :as pr])))

(deftest main

  (testing "misc"
    #?(:clj (t/is (= (m/layer-split :s
                                    (score (tup s0 s1 s2)
                                           (each (tup d0 d1 d2))))
                     (list {:position 2/3, :layer-idx 2, :score #{{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2, :d 0}}, :voice 0, :duration 1/9, :position 2/3, :velocity 80, :track 0} {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2, :d 2}}, :voice 0, :duration 1/9, :position 8/9, :velocity 80, :track 0} {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2, :d 1}}, :voice 0, :duration 1/9, :position 7/9, :velocity 80, :track 0}}}
                           {:position 1/3, :layer-idx 1, :score #{{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 0}}, :voice 0, :duration 1/9, :position 1/3, :velocity 80, :track 0} {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 2}}, :voice 0, :duration 1/9, :position 5/9, :velocity 80, :track 0} {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 1}}, :voice 0, :duration 1/9, :position 4/9, :velocity 80, :track 0}}}
                           {:position 0N, :layer-idx 0, :score #{{:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 2}}, :voice 0, :duration 1/9, :position 2/9, :velocity 80, :track 0} {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 1}}, :voice 0, :duration 1/9, :position 1/9, :velocity 80, :track 0} {:patch [0 4], :channel 0, :pitch {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0}}, :voice 0, :duration 1/9, :position 0N, :velocity 80, :track 0}}})))))

  (testing "permutations"
    (freeze (score (rup 8 d1)
                   (m/permutation {:grade 5 :layer :d})))

    (freeze (score dur2
                   (tup s0 s1 s2 s3)
                   (each (tup d1 d1- d0))
                   (m/permutation :rand {:layer :s})))

    (freeze (score dur2
                   (tup s0 s1 s2)
                   (each (rup 4 d1))
                   (m/permutation 0 {:layers [:s :d]}))))

  (testing "contour"
    (freeze (score (tup d0 d1 d2)
                   (m/contour :mirror {:layer :d})))

    (freeze (score dur:2
                   (shuflin s0 s1 s2 s4)
                   (each (tup d0 c1- d1 d0))
                   (lin same
                        [dur:4 vel0]
                        (m/contour :mirror {:layer :s})
                        [dur:4 vel0]
                        (m/contour :similar {:extent [4 4] :layer :s})
                        [dur:4 vel0]
                        (m/contour :rotation {:layer :s :pick 2}))))

    (freeze (score dur:2
                   (lin s0 s1 s2 s3)
                   (lin* (map (fn [i] (lin [dur:4 vel0]
                                           (m/contour :rotation {:extend [0 0] :layer :s :pick i})))
                              (range 4)))))

    (freeze (score dur:2
                   (lin s0 s1 s2 s4)
                   (each (tup d0 d3- d6-))
                   (lin* (map (fn [i] (lin [dur:4 vel0]
                                           (m/contour :rotation {:extend [0 0] :layer :s :pick i})))
                              (range 5))))))

  (testing "gen"

    (freeze (score (m/simple-line 32 (one-of s1 s1- d1 d1-))
                   (adjust 4)))

    (freeze (score (m/simple-line 32 (one-of s1 s1- (tup d1 d0 s2-) (tup d1- d0 s2)))
                   (adjust 8)))

    (freeze (score dur:4
                   (m/simple-line 64
                                  (one-of (nlin> 4 (one-of d1- d1))
                                          (tup d1 d1- s0)
                                          (lin s2 s1 s1-)
                                          (nlin> 4 (one-of s1- s1))))
                   (chans (patch :electric-piano-1)
                          [(patch :ocarina) o1 (each d3)])))

    (freeze (score {:description "another way to build a melodic line from a bunch of randomly chosen updates"}
                   (patch :acoustic-guitar-nylon)
                   (repeat-while (within-time-bounds? 0 32)
                                 (append [start-from-last
                                          (any-that (within-pitch-bounds? :C-1 :C2)
                                                    (rep 3 d3 :skip-first)
                                                    (rep 3 d3- :skip-first)
                                                    d1 d1-)]))
                   (adjust 3)))

    (freeze (score dur:2
                   (patch :whistle)
                   (m/gen-line {:contour [6 4] :grow 6 :layer :c})
                   (append> c3 c4 c2 rev (each (maybe o1 o1-)))))

    #?(:clj (t/is (= (pr/with-rand 0
                       (take 10 (m/step-seqs {:length 5
                                              :delta 0
                                              :bounds [-2 6]
                                              :step-range [-4 4]})))
                     '([-1 1 2 -4 2] (-2 3 -2 3 -2) (-2 4 -2 -2 2) (-1 -1 4 -3 1) [-2 1 2 3 -4] (4 -2 -1 2 -3) (-2 3 -3 4 -2) (3 -3 3 -4 1) (2 -3 2 3 -4) [-1 3 -3 4 -3]))))

    (freeze (score dur:2
                   (nlin 100 (! (m/gen-tup :c 6 6 {:bounds [-12 12]
                                                   :step-range [-7 7]})))))))
