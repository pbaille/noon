(ns noon.numbers-test
  (:require [noon.numbers :as n]
            [clojure.test :as t :refer [is]]))

(t/deftest main
  (t/testing "numeric-updates"

    (is (= ((n/sub 1) 1)
           0))
    (is (= ((n/add 1) 1)
           2))
    (is (= ((n/mul 2) 1)
           2))
    (is (= ((n/div 2) 1)
           0.5))

    (is ((n/eq 1) 1))
    (is (not ((n/eq 1) 0)))
    (is (not ((n/gt 1) 1)))
    (is ((n/gt 1) 2))
    (is ((n/gte 1) 1))
    (is ((n/gte 1) 2))
    (is (not ((n/gte 1) 0)))
    (is ((n/lt 1) 0))
    (is (not ((n/lt 1) 2)))
    (is ((n/lte 1) 1))
    (is (not ((n/lte 1) 2)))
    (is ((n/lte 1) 0))

    (is (= (n/->7bits-natural 0.33)
           0))
    (is (= (n/->7bits-natural 1000)
           127))
    (is (= (n/->7bits-natural -1000)
           0))
    (is (= (n/->7bits-natural 60.2)
           (n/->7bits-natural 59.7)
           (n/->7bits-natural 60)
           60))

    (is (= (n/->4bits-natural 0.33)
           0))
    (is (= (n/->4bits-natural 1000)
           15))
    (is (= (n/->4bits-natural -1000)
           0))

    (is (= (n/->4bits-natural 10.1)
           (n/->4bits-natural 9.7)
           (n/->4bits-natural 10)
           10))))
