(ns noon.utils.euclidean-sums-test
  (:require [noon.utils.euclidean-sums :as e]
            [clojure.test :refer [deftest testing is]]))

(deftest main
  (testing "euclidean-sum"
    (is (= (e/euclidean-sum 5 8)
           [2 1 2 1 2]))
    (is (= (e/euclidean-sum 5 9)
           [2 2 1 2 2]))
    (is (= (e/euclidean-sum 3 8)
           [3 2 3]))
    (is (= (e/euclidean-sums 12)
           (list [12] [6 6] [4 4 4]
                 [3 3 3 3] [2 3 2 3 2]
                 [2 2 2 2 2 2] [2 1 2 2 2 1 2]
                 [1 2 1 2 1 2 1 2] [1 2 1 1 2 1 1 2 1]
                 [1 1 2 1 1 1 1 2 1 1] [1 1 1 1 1 2 1 1 1 1 1])))))
