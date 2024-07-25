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
