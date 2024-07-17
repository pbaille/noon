(ns noon.score-test
  (:require [noon.score :as s]
            [clojure.test :refer [deftest testing is]]))

(def E0 s/DEFAULT_EVENT)

(deftest helpers
  (testing "numeric-updates"

    (is (= ((s/sub 1) 1)
           0))
    (is (= ((s/add 1) 1)
           2))
    (is (= ((s/mul 2) 1)
           2))
    (is (= ((s/div 2) 1)
           1/2))

    (is ((s/eq 1) 1))
    (is (not ((s/eq 1) 0)))
    (is (not ((s/gt 1) 1)))
    (is ((s/gt 1) 2))
    (is ((s/gte 1) 1))
    (is ((s/gte 1) 2))
    (is (not ((s/gte 1) 0)))
    (is ((s/lt 1) 0))
    (is (not ((s/lt 1) 2)))
    (is ((s/lte 1) 1))
    (is (not ((s/lte 1) 2)))
    (is ((s/lte 1) 0))

    (is (= (s/hm* [:a 1 :b 2])
           {:b 2, :a 1}))

    (is (= (s/?reduce + 0 (range 5))
           10))
    (is (nil? (s/?reduce (fn [a e] (if (> e 2) (+ a e)))
                         0 (range 5))))
    (is (= (s/?reduce (fn [a e] (if (> e 2) (+ a e)))
                      0 (range 3 6))
           12))

    (is (= (s/->7bits-natural 1/3)
           0))
    (is (= (s/->7bits-natural 1000)
           127))
    (is (= (s/->7bits-natural -1000)
           0))
    (is (= (s/->7bits-natural 60.2)
           (s/->7bits-natural 59.7)
           (s/->7bits-natural 60)
           60))

    (is (= (s/->4bits-natural 1/3)
           0))
    (is (= (s/->4bits-natural 1000)
           15))
    (is (= (s/->4bits-natural -1000)
           0))

    (is (= (s/->4bits-natural 10.1)
           (s/->4bits-natural 9.7)
           (s/->4bits-natural 10)
           10))))

(deftest event-updates
  (testing "simples"

    (is (= (:velocity ((s/vel 23) E0))
           23))
    (is (= (:velocity ((s/vel inc) E0))
           81))
    (is (= (:velocity ((s/vel (s/mul 2)) E0))
           127))
    (is (zero? (:velocity ((s/vel (s/sub 100)) E0))))))
