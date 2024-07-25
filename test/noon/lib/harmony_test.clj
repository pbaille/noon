(ns noon.lib.harmony-test
  (:use noon.score)
  (:require [noon.lib.harmony :as h]
            [clojure.test :refer [testing deftest is]]))

(deftest helpers
  (is (and (not (h/bounds-gte [0 1] [1 2]))
           (not (h/bounds-gte [0 1] [0 2]))))
  (is (and (h/bounds-gte [0 1] [0 0])
           (h/bounds-gte [0 2] [0 1])
           (h/bounds-gte [0 2] [0 2])
           (h/bounds-gte [0 3] [1 2])))

  (is (h/in-bounds [60 72]
                   (mk (tup s0 s1 s2))))
  (is (not (h/in-bounds [60 72]
                        (mk (tup s0 [o1 d1])))))
  (is (not (h/in-bounds [60 72]
                        (mk d1-)))))

(deftest voicings

  (testing "abstract drops"
    (is (= (h/abstract-drops 2)
           (list [[0 1]] [[1] [0]])))
    (is (= (h/abstract-drops 3)
           (list [[0 1 2]] [[1 2] [0]] [[0 2] [1]] [[2] [0 1]] [[1] [0 2]] [[2] [1] [0]])))
    (is (= (h/abstract-drops 4)
           (list [[0 1 2 3]] [[1 2 3] [0]] [[0 2 3] [1]] [[2 3] [0 1]] [[0 1 3] [2]] [[0 3] [1 2]] [[1 3] [0 2]] [[3] [0 1 2]]
                 [[0 2] [1 3]] [[1 2] [0 3]] [[1] [0 2 3]] [[2] [0 1 3]] [[1 3] [2] [0]] [[2 3] [1] [0]] [[2] [1 3] [0]]
                 [[3] [1 2] [0]] [[0 3] [2] [1]] [[2] [0 3] [1]] [[3] [0 2] [1]] [[3] [2] [0 1]] [[1] [0 3] [2]]
                 [[3] [1] [0 2]] [[2] [1] [0 3]] [[3] [2] [1] [0]])))
    (is (= (h/abstract-drops (list 0 1 1 2))
           (list [[0 1 2] [1]] [[1 2] [0 1]] [[0 1] [1 2]] [[1] [0 1 2]]
                 [[1 2] [1] [0]] [[1] [1 2] [0]] [[0 2] [1] [1]] [[1] [0 2] [1]]
                 [[2] [0 1] [1]] [[2] [1] [0 1]] [[1] [1] [0 2]] [[2] [1] [1] [0]]))))

  (testing "closed"

    (is (= (h/closed (mk (par s0 s2 s4)))
           (mk (par s0 s2 [o1- s4]))))
    (is (= (h/closed (mk (par s0 s2 s4 s6)))
           (mk (par s0 s2 [o1- s4] [o2- s6]))))
    (is (= (h/closed-no-unison (mk (par s0 s2 s4 s6)))
           (mk (par s0 s2 [o1- s4] [o1- s6])))))

  (testing "drops"
    (h/drops (mk (par s0 s1 s2 s3)))
    (h/drops (mk (par d0 d1 d2 d3)))
    #_(h/drop)))
