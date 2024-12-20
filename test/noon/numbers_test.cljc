(ns noon.numbers-test
  (:require [noon.numbers :as n]
            [clojure.test :as t :refer [is]]
            [noon.utils.pseudo-random :as pr]))

(t/deftest main
  (t/testing "numeric-updates"

    (is (= ((n/sub 1) 1)
           0))
    (is (= ((n/add 1) 1)
           2))
    (is (= ((n/mul 2) 1)
           2))
    (is (n/float-equal? ((n/div 2) 1)
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

(t/deftest midi-values
  (is (= 0
         (n/midi-val 0)
         (n/midi-val -1)
         (n/midi-val :min)
         (n/midi-val 0.0)
         (n/midi-val (/ 0 1))
         (n/midi-val (/ -1 2))))
  (is (= 127
         (n/midi-val 127)
         (n/midi-val 1270)
         (n/midi-val 1.001)
         (n/midi-val 1.5)
         (n/midi-val (/ 3 2))
         (n/midi-val :max)))
  (is (= 64
         (n/midi-val 64)
         (n/midi-val (/ 1 2))
         (n/midi-val 0.5)))

  (is (= (pr/with-rand 0 (take 100 (iterate
                                    (n/humanize :max-step (/ 1 10) :bounds [20 80])
                                    60)))
         #?(:clj (list 60 63 59 61 62 64 61 59 65 70 76 72 67 62 56 57 63 58 60 58 62 68 67 70 73 77 79 76
                       79 73 67 73 76 78 73 72 66 60 58 56 61 67 72 74 70 66 71 65 68 72 75 71 75 80 74 72 77 79 80
                       79 74 80 78 77 79 76 74 79 76 80 79 75 69 63 57 56 62 68 72 73 70 67 72 66 60 58 53 58 52 49 50
                       56 53 58 60 65 59 61 60 61)
            :cljs (list 60 54 56 60 63 58 63 69 65 64 60 55 52 48 46 43 45 44 46 50 51 49 53 55 56 53 59 63 68
                        73 67 62 56 61 63 66 72 75 70 69 72 76 71 67 72 68 70 76 70 68 63 68 71 73 69 63 58 52
                        50 44 38 32 37 38 32 29 33 30 31 34 40 35 29 24 23 21 20 22 24 26 27 28 29 30 36 38 40
                        37 32 35 40 42 46 51 45 43 39 38 44 50)))))
