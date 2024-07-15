(ns noon.utils.pseudo-random-test
  (:require [noon.utils.pseudo-random :as pr]
            [clojure.test :refer [deftest is]]))

(defn sample [n f]
  (if-not (zero? n)
    (cons (f) (sample (dec n) f))))

(deftest one
  (is (= (pr/with-rand 0
           (list (pr/rand) (pr/rand) (pr/rand)))
         (list 0.7309677600860596 0.8314409852027893 0.2405363917350769)))
  (is (= (pr/with-rand 1
           (list (pr/rand) (pr/rand) (pr/rand)))
         (list 0.7308781743049622 0.10047316551208496 0.41008079051971436)))
  (is (every? (fn [n] (<= 0 n 10))
              (sample 50 #(pr/rand-int 10))))

  (is (every? (fn [n] (<= 10 n 30))
              (sample 50 #(pr/rand-int-between 10 30))))

  (is (= (pr/with-rand 234
           (sample 50 #(pr/rand-int-between 10 30)))
         (list 24 26 26 14 29 26 16 13 12 10 11 18 16 19 24 10 23 26 15 16 23 14 10 13 24 29 23 12 22 28
               14 24 10 23 27 11 22 10 27 20 29 16 18 26 19 10 12 24 14 27)))

  (is (= (pr/with-rand 234
           (sample 50 #(pr/rand-nth (range 0 10))))
         (list 7 8 8 2 9 8 3 1 1 0 0 4 3 4 7 0 6 8 2 3 6 2 0 1
               7 9 6 1 6 9 2 7 0 6 8 0 6 0 8 5 9 3 4 8 4 0 1 7 2 8)))

  (is (= (pr/with-rand 234
           (sample 10 #(pr/shuffle (range 10))))
         (list [3 2 0 8 4 5 1 6 9 7] [4 5 1 8 7 6 2 3 9 0] [4 6 3 8 7 0 1 5 9 2] [6 8 3 2 0 4 9 7 5 1]
               [9 8 2 4 1 5 3 7 0 6] [3 6 2 9 4 7 8 5 1 0] [8 2 7 9 4 3 1 6 0 5] [9 3 1 8 2 0 5 7 4 6]
               [9 2 0 7 8 6 4 1 5 3] [2 9 0 6 5 7 1 3 8 4])))

  (is (every? (fn [s] (= (range 10) (sort s)))
              (pr/with-rand 234
                (sample 10 #(pr/shuffle (range 10)))))))
