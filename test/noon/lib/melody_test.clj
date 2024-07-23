(ns noon.lib.melody-test
  (:require [noon.lib.melody :as m]
            [noon.test :as t]
            [clojure.test :refer [deftest testing is]]
            [noon.score :as n]
            [noon.utils.pseudo-random :as pr]))

(deftest main

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
