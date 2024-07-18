(ns noon.utils.sequences-test
  (:require [noon.utils.sequences :as s]
            [clojure.test :refer [deftest is]]
            [noon.utils.pseudo-random :as pr]))

(deftest impl
  (is (s/rotate [1 2 3 4] -1)
      (list 4 1 2 3))
  (is (= (s/rotate [1 2 3 4] 1)
         (s/rotate [1 2 3 4] 5)
         (list 2 3 4 1)))
  (is (= (s/rotate [1 2 3 4] -6)
         (list 3 4 1 2)))
  (is (= (pr/with-rand 0
           (s/shuffle-no-rep [1 2 3 2 4 5 3 4]))
         (list 3 5 2 1 2 4 3 4)))
  (is (every? (fn [xs] (every? (partial apply not=) (partition 2 1 xs)))
              (repeatedly 10 #(s/shuffle-no-rep [1 2 3 2 4 5 3 4])))))

(deftest member
  (is (= 1 (s/member [1 2 3 4] 0)))
  (is (= 4 (s/member [1 2 3 4] -1)))
  (is (= 3 (s/member [1 2 3 4] -2)))
  (is (= 3 (pr/with-rand 0 (s/member [1 2 3 4] :rand))))
  (is (= 3 (pr/with-rand 0 (s/member [1 2 3 4] :random))))
  (is (= 2 (pr/with-rand 0 (s/member [1 2 3 4] [1 -2]))))
  (is (= 3 (s/member [1 2 3 4] 0.5)))
  (is (= 4 (s/member [1 2 3 4] 0.7)))
  (is (= 4 (s/member [1 2 3 4] -0.1))))

(deftest transformations
  (is (= (s/rotations [1 2 3 4])
         '((2 3 4 1) (3 4 1 2) (4 1 2 3))))
  (is (= (s/rotation [1 2 3 4] 2)
         (list 3 4 1 2)))
  (is (= (s/rotation [1 2 3 4] -2)
         (list 3 4 1 2)))
  (is (= (s/rotation [1 2 3 4] 1/4)
         (list 2 3 4 1)))
  (is (= (pr/with-rand 0 (s/rotation [1 2 3 4] :rand))
         (list 3 4 1 2)))
  (is (= (s/partitions [1 2 3 4 5 6])
         '(((1 2 3) (4 5 6))
           ((1 2) (3 4) (5 6))
           ((1) (2) (3) (4) (5) (6)))))
  (is (= (s/simple-subseqs [1 2 3 4 5 6])
         '((1 2 3) (4 5 6) (1 2) (3 4) (5 6))))
  (is (= (s/simple-permutations [1 2 3 4 5 6])
         '((1 2 3 4 5 6)
           (4 5 6 1 2 3)
           (1 2 5 6 3 4)
           (3 4 1 2 5 6)
           (3 4 5 6 1 2)
           (5 6 1 2 3 4)
           (5 6 3 4 1 2)))))

(deftest permutations

  (is (= (s/splits [1 2 3 4 5 6] 3)
         '([(1 2) (3 4) (5 6)]
           [(1) (2 3) (4 5 6)]
           [(1) (2 3 4) (5 6)]
           [(1 2) (3) (4 5 6)]
           [(1 2) (3 4 5) (6)]
           [(1 2 3) (4) (5 6)]
           [(1 2 3) (4 5) (6)]
           [(1) (2) (3 4 5 6)]
           [(1) (2 3 4 5) (6)]
           [(1 2 3 4) (5) (6)])))

  (is (= (s/splits [1 2 3 4 5 6] 2)
         '([(1 2 3) (4 5 6)]
           [(1 2) (3 4 5 6)]
           [(1 2 3 4) (5 6)]
           [(1) (2 3 4 5 6)]
           [(1 2 3 4 5) (6)])))

  (is (= (s/splits [1 2 3 4 5 6] 2 [2 3 4])
         '([(1 2 3) (4 5 6)]
           [(1 2) (3 4 5 6)]
           [(1 2 3 4) (5 6)])))

  (is (= (s/split-permutations 3)
         '([0 2 1] [1 0 2] [2 1 0])))

  (is (= (s/split-permutations 4)
         '([0 2 1 3] [0 3 2 1] [1 0 3 2] [1 3 0 2]
                     [1 3 2 0] [2 0 3 1] [2 1 0 3] [2 1 3 0]
                     [3 0 2 1] [3 1 0 2] [3 2 1 0])))

  (is (= (s/grade-permutations [1 2 3 4] 0)
         '((1 2 3 4))))

  (is (= (s/grade-permutations [1 2 3 4] 1)
         '((3 4 1 2) (2 3 4 1) (4 1 2 3))))

  (is (= (s/grade-permutations [1 2 3 4] 2)
         '((1 3 4 2) (2 1 3 4)
                     (3 4 2 1) (1 4 2 3)
                     (2 3 1 4) (4 2 3 1)
                     (1 2 4 3) (3 1 2 4) (4 3 1 2))))

  (is (= (s/gradual-permutations [1 2 3 4])
         '{0 ((1 2 3 4)),
           1 ((3 4 1 2) (2 3 4 1) (4 1 2 3)),
           2 ((1 3 4 2) (2 1 3 4) (3 4 2 1) (1 4 2 3) (2 3 1 4) (4 2 3 1) (1 2 4 3) (3 1 2 4) (4 3 1 2)),
           3 ((1 3 2 4) (1 4 3 2) (2 1 4 3) (2 4 1 3) (2 4 3 1) (3 1 4 2) (3 2 1 4) (3 2 4 1) (4 1 3 2) (4 2 1 3) (4 3 2 1))}))

  (is (= (s/permutation [1 2 3 4] 0)
         (list 1 2 3 4)))
  (is (= (s/permutation [1 2 3 4] 1)
         (list 3 4 1 2)))
  (is (= (pr/with-rand 2
           (s/permutation [1 2 3 4] :rand))
         [2 1 4 3]))
  (is (= (s/permutation [1 2 3 4] -1)
         [4 3 2 1]))
  (is (= (s/permutation [1 2 3 4] 0.5)
         [4 3 1 2]))
  (is (= (pr/with-rand 0
           (s/permutation [1 2 3 4] [0.25 0.5]))
         (list 1 2 4 3)))
  (is (= (s/permutation [1 2 3 4] -1 {:grade 1})
         [4 1 2 3]))
  (is (= (s/permutation [1 2 3 4 5 6] -1 {:split-sizes [2 3]})
         [5 6 3 4 1 2])))

(comment :tries

         (= 2 (mirror-idx (list 1 2 3 4) 1))

         (= 0 (mirror-idx (list 1 2 3 4) 3))
         (do :misc
             (idx-permutations 4)
             (take 10 (simple-permutations (range 8)))
             (partitions '(1 2 3 4 5 6))
             (simple-subseqs '(0 1 2 3 4 5))
             (splits (range 10) 3)
             (u/sums 10 3 (range 1 10)))

         (split-permutations 1)
         (grade-permutations [0 1 2 3 4 5] 3 [2 4])

         (count (c/permutations (range 6)))
         (count (gradual-permutations (range 6)))
         (member (range 6) 0)
         (default-split-sizes (range 5))
         (permutation (range 6) 0)
         (permutation (range 6) :rand {:grade 2}))
