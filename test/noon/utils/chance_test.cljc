(ns noon.utils.chance-test
  (:require [noon.utils.chance :as c]
            [clojure.test :refer [deftest testing is]]
            [noon.utils.pseudo-random :as pr]))

(deftest base
  (testing "base"
    (is (c/gen? (c/fn->gen (constantly :ok))))
    (is (not (c/gen? (constantly :ok))))
    (is (= (c/realise (c/fn->gen (constantly :ok)))
           :ok))
    (is (= (c/realise :ok)
           :ok))
    (is (= (c/sample 3 (c/fn->gen (constantly :ok)))
           (repeat 3 :ok)))))

(c/defgen pouet
  "a pouet generator"
  {:meta :data
   :interesting false
   :deterministic false}
  ([]
   (pr/rand-nth [:pouet :pwet :puette]))
  ([strict?]
   (if strict? :pouet :pwet)))

(deftest syntax
  (testing "gen"
    (is (= 1 (c/realise (c/gen 1))))
    (is (= (pr/with-rand 0
             (c/realise (c/gen (pr/rand))))
           #?(:cljs 0.500062950304724
              :clj 0.7309677600860596)))
    (is (= (pr/with-rand 0
             (c/sample 3 (c/gen (pr/rand))))
           #?(:cljs (list 0.500062950304724 0.5157397988987481 0.9226656041393675)
              :clj
              (list 0.7309677600860596
                    0.8314409852027893
                    0.2405363917350769)))))
  (testing "defgen"
    (is (contains? #{:pouet :pwet :puette}
                   (c/realise (pouet))))
    (is (every? #{:pouet :pwet :puette}
                (c/sample 10 (pouet))))
    (is (= (pr/with-rand 0 (c/sample 10 (pouet)))
           #?(:cljs (list :pwet :pwet :puette :pouet :pouet :puette :pwet :pwet :pwet :pwet)
              :clj (list :puette :pouet :pwet :pwet :pwet :pouet :pwet :puette :puette :puette))))
    (is (every? (partial = :pouet)
                (c/sample 10 (pouet true))))))

(deftest simple
  (is (= (pr/with-rand 0
           (c/sample 4 c/coin))
         #?(:cljs (list false false false true)
            :clj (list false true false false))))
  (is (= (pr/with-rand 0
           (c/sample 10 c/cube))
         #?(:cljs (list 4 4 6 2 1 6 3 3 3 4)
            :clj (list 5 2 4 4 4 2 3 6 6 6))))
  (is (= (pr/with-rand 0
           (c/sample 10 (c/dice 6)))
         #?(:cljs (list 4 4 6 2 1 6 3 3 3 4)
            :clj (list 5 2 4 4 4 2 3 6 6 6))))
  (is (= (pr/with-rand 0
           (c/sample 10 (c/nat 0 12)))
         #?(:cljs (list 6 6 11 4 2 12 4 4 6 7)
            :clj (list 9 3 8 7 7 4 5 12 11 12))))
  (is (= (pr/with-rand 0
           (c/sample 4 (c/decimal 0 5)))
         #?(:cljs (list 2.50031475152362
                        2.5786989944937404
                        4.613328020696837
                        1.5775287446047945)
            :clj (list 3.654838800430298
                       4.1572049260139465
                       1.2026819586753845
                       3.0317258834838867))))
  (is (= (pr/with-rand 0
           (c/sample 4 (c/bag [:ok :ko])))
         #?(:cljs (list :ko :ko :ko :ok)
            :clj (list :ko :ok :ko :ko)))))

(deftest combinators

  (testing "data"
    (is (= (pr/with-rand 0
             (c/sample 6 (c/data [c/coin c/cube])))
           #?(:cljs (list [false 4] [false 2] [true 6] [true 3] [true 4] [false 4])
              :clj (list [false 2]
                         [false 4]
                         [false 2]
                         [true 6]
                         [false 6]
                         [true 1]))))
    (is (= (pr/with-rand 0
             (c/sample 6 {:valid c/coin
                          :coordinates [(c/decimal 0 10) (c/decimal 0 20)]}))
           #?(:cljs (list {:valid false, :coordinates [5.157397988987481 18.45331208278735]}
                          {:valid true, :coordinates [1.5515977008155541 18.973058210446744]}
                          {:valid true, :coordinates [3.723766031610725 9.7916347789093]}
                          {:valid false, :coordinates [9.732677405637846 11.416808956166918]}
                          {:valid false, :coordinates [6.5562498724451865 11.59576526647335]}
                          {:valid false, :coordinates [4.521097546564671 9.296317307580336]})
              :clj (list {:valid false, :coordinates [2.405363917350769 12.126903533935547]}
                         {:valid false, :coordinates [5.504369735717773 2.340131998062134]}
                         {:valid false, :coordinates [3.3321839570999146 5.055522918701172]}
                         {:valid true, :coordinates [9.84841525554657 19.656389951705933]}
                         {:valid false, :coordinates [9.41249132156372 3.5195350646972656]}
                         {:valid true, :coordinates [1.2889713048934937 7.359514236450195]})))))

  (testing "others"
    (is (= (pr/with-rand 0
             (c/sample 3 (c/$ c/cube -)))
           #?(:cljs (list -4 -4 -6)
              :clj (list -5 -2 -4))))
    (is (= (pr/with-rand 0
             (c/sample 10 (c/bind c/coin
                                  (fn [x] (if x (c/bag [:ok :yes]) (c/bag [:nop :flop]))))))
           #?(:cljs (list :flop :nop :yes :ok :yes :flop :flop :flop :ok :flop)
              :clj (list :nop :flop :nop :yes :flop :ok :ok :flop :yes :yes))))
    (is (= (pr/with-rand 0
             (c/sample 10 (c/keep (c/nat 1 10)
                                  even?)))
           #?(:cljs (list 6 6 10 4 2 10 4 4 10 6)
              :clj (list 8 6 6 4 4 10 10 2 2 6))))
    (is (= (pr/with-rand 0
             (c/sample 4 (c/one-of :yes :no)))
           (pr/with-rand 0
             (c/sample 4 (c/one-of* [:yes :no])))
           #?(:cljs (list :no :no :no :yes)
              :clj (list :no :yes :no :no))))
    (is (= (pr/with-rand 0
             (c/sample 10 (c/weighted {c/coin 1 (c/one-of :yes :no) 4})))
           #?(:cljs (list :no :yes false :yes :no :no :no :no :yes :no)
              :clj (list :no :no :yes :yes :no :no :yes :yes false false))))))

(deftest collections
  (is (= (pr/with-rand 0
           (c/sample 5 (c/tup c/coin (c/nat 1 10))))
         #?(:cljs (list [false 6] [false 4] [true 10] [true 4] [true 7])
            :clj (list [false 3] [false 6] [false 4] [true 10] [false 10]))))
  (is (= (pr/with-rand 0
           (c/sample 5 (c/tup c/coin
                              (c/tup (c/one-of :yes :no) (c/nat 1 10))
                              (c/dice 7))))
         #?(:cljs (list [false [:no 10] 3] [true [:no 4] 3] [true [:no 10] 4] [false [:no 6] 6] [true [:yes 9] 7])
            :clj (list [false [:yes 7] 4] [false [:yes 4] 7] [false [:no 3] 1] [true [:yes 6] 7] [true [:no 5] 6]))))
  (is (every? boolean?
              (concat (c/realise (c/seqof c/coin))
                      (c/realise (c/vecof c/coin))
                      (c/realise (c/setof c/coin)))))
  (is (= (pr/with-rand 0
           (c/sample 5 (c/seqof c/coin :size 4)))
         #?(:cljs '((true false false false)
                    (true true false true)
                    (false false false true)
                    (false false false false)
                    (false false true true))
            :clj '((false false true false)
                   (false true true false)
                   (true true false false)
                   (false false true true)
                   (false true false true)))))
  (is (= (pr/with-rand 0
           (c/sample 5 (c/vecof c/coin :min-size 2 :max-size 4)))
         #?(:cljs (list [false false true]
                        [false true]
                        [true false false]
                        [false false false]
                        [true true false false])
            :clj (list [true false false false]
                       [true false]
                       [false true true true]
                       [false false]
                       [false true]))))
  (is (= (pr/with-rand 0
           (c/sample 5 (c/mapof (c/bag (range 10)) c/coin :min-size 2 :max-size 4)))
         #?(:cljs (list {5 false, 3 true, 9 true} {4 false, 9 false, 8 false} {7 true, 4 false, 8 false} {3 false, 9 true, 8 false} {8 true, 6 true, 5 false})
            :clj (list {2 false, 5 false, 3 true, 9 false}
                       {2 true, 1 false, 5 false}
                       {7 false, 4 false}
                       {5 false, 1 true, 9 false, 7 true}
                       {0 true, 3 true, 8 false}))))
  (is (= (pr/with-rand 0
           (c/sample 5 (c/rep c/cube 5)))
         #?(:cljs (list [4 4 6 2 1]
                        [6 3 3 3 4]
                        [6 4 6 4 4]
                        [5 3 3 6 6]
                        [6 5 2 6 3])
            :clj (list [5 2 4 4 4]
                       [2 3 6 6 6]
                       [2 1 1 1 4]
                       [6 1 4 3 5]
                       [6 3 5 5 5])))))
