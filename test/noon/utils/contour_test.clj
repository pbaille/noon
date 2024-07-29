(ns noon.utils.contour-test
  (:require [noon.utils.contour :as c]
            [clojure.test :refer [deftest is]]
            [noon.utils.pseudo-random :as pr]))

(deftest main
  (is (= (c/bounds (shuffle (range 0 10)))
         [0 9]))
  (is (= (c/size (shuffle (range 0 10)))
         9))
  (is (= (c/contour [1 3 2 4 2])
         (c/contour [1 5 4 9 4])
         [0 2 1 3 1]))
  (is (= (pr/with-rand 0
           (c/contour-inversions (pr/shuffle (range 4))))
         (list [3 1 0 2] [0 2 1 3] [1 3 2 0] [2 0 3 1])))
  (is (= (c/contour-inversions (range 4))
         (list [0 1 2 3] [1 2 3 0] [2 3 0 1] [3 0 1 2])))
  (is (= (c/contour-inversions [0 1 0 2 1 3])
         (list [0 1 0 2 1 3] [1 2 1 3 2 0] [2 3 2 0 3 1] [3 0 3 1 0 2])))
  (is (= (c/contour-mirror [0 3 2 4 2])
         [4 1 2 0 2]))
  (is (= (c/lines [0 3 1 4 2] 2)
         (list [0 3 1 4 2] [0 3 1 5 2]
               [0 4 1 5 2] [0 4 1 5 3]
               [0 4 2 5 3] [0 3 1 6 2]
               [0 5 1 6 2] [0 5 1 6 4]
               [0 5 3 6 4] [0 4 1 6 2]
               [0 4 1 6 3] [0 5 1 6 3]
               [0 4 2 6 3] [0 5 2 6 3] [0 5 2 6 4])))
  (is (= (c/similars [0 3 1 4 2] 2)
         (list [0 3 1 6 2] [0 5 1 6 2]
               [0 5 1 6 4] [0 5 3 6 4]
               [0 4 1 6 2] [0 4 1 6 3]
               [0 5 1 6 3] [0 4 2 6 3]
               [0 5 2 6 3] [0 5 2 6 4])))
  (is (= (c/similars [0 3 1 4 2] [2 4])
         (list [0 3 1 6 2] [0 5 1 6 2] [0 5 1 6 4] [0 5 3 6 4]
               [0 4 1 6 2] [0 4 1 6 3] [0 5 1 6 3] [0 4 2 6 3]
               [0 5 2 6 3] [0 5 2 6 4] [0 3 1 7 2] [0 6 1 7 2]
               [0 6 1 7 5] [0 6 4 7 5] [0 4 1 7 2] [0 5 1 7 2]
               [0 4 1 7 3] [0 6 1 7 3] [0 5 1 7 4] [0 6 1 7 4]
               [0 4 2 7 3] [0 6 2 7 3] [0 6 2 7 5] [0 5 3 7 4]
               [0 6 3 7 4] [0 6 3 7 5] [0 5 1 7 3] [0 5 2 7 3]
               [0 5 2 7 4] [0 6 2 7 4] [0 3 1 8 2] [0 7 1 8 2]
               [0 7 1 8 6] [0 7 5 8 6] [0 4 1 8 2] [0 6 1 8 2]
               [0 4 1 8 3] [0 7 1 8 3] [0 6 1 8 5] [0 7 1 8 5]
               [0 4 2 8 3] [0 7 2 8 3] [0 7 2 8 6] [0 6 4 8 5]
               [0 7 4 8 5] [0 7 4 8 6] [0 5 1 8 2] [0 5 1 8 4]
               [0 7 1 8 4] [0 5 3 8 4] [0 7 3 8 4] [0 7 3 8 6]
               [0 5 1 8 3] [0 6 1 8 3] [0 6 1 8 4] [0 5 2 8 3]
               [0 6 2 8 3] [0 5 2 8 4] [0 7 2 8 4] [0 6 2 8 5]
               [0 7 2 8 5] [0 6 3 8 4] [0 6 3 8 5] [0 7 3 8 5] [0 6 2 8 4])))
  (is (= (pr/with-rand 0
           (c/gen-contour 4 3))
         [0 1 2 1]))
  (is (= (pr/with-rand 0
           (c/gen-contour 5 4))
         [0 1 3 2 3]))
  (is (= (pr/with-rand 0
           (c/gen-line {:contour [5 3] :grow 3 :pick :rand}))
         [0 4 5 4 0]))
  (is (= (pr/with-rand 0
           (c/gen-line {:contour [5 3] :grow 3 :pick -0.75}))
         [0 2 3 2 0])))
