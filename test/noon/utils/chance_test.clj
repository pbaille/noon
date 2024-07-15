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

(deftest syntax
  (testing "gen"
    (is (= 1 (c/realise (c/gen 1))))
    (is (= (pr/with-rand 0
             (c/realise (c/gen (pr/rand))))
           0.7309677600860596))
    (is (= (pr/with-rand 0
             (c/sample 3 (c/gen (pr/rand))))
           (list 0.7309677600860596
                 0.8314409852027893
                 0.2405363917350769))))
  (c/defgen pouet
    "a pouet generator"
    {:meta :data
     :interesting false
     :deterministic false}
    ([]
     (pr/rand-nth [:pouet :pwet :puette]))
    ([strict?]
     (if strict? :pouet :pwet)))

  (testing "defgen"
    (is (contains? #{:pouet :pwet :puette}
                   (c/realise (pouet))))
    (is (every? #{:pouet :pwet :puette}
                (c/sample 10 (pouet))))
    (is (pr/with-rand 0
          (= (c/sample 10 (pouet))
             (list :puette :pouet :pwet :pwet :pwet :pouet :pwet :puette :puette :puette))))
    (is (every? (partial = :pouet)
                (c/sample 10 (pouet true))))))

(deftest simple
  (is (= (pr/with-rand 0
           (c/sample 4 c/coin))
         (list false true false false)))
  (is (= (pr/with-rand 0
           (c/sample 10 c/cube))
         (list 5 2 4 4 4 2 3 6 6 6)))
  (is (= (pr/with-rand 0
           (c/sample 10 (c/dice 6)))
         (list 5 2 4 4 4 2 3 6 6 6)))
  (is (= (pr/with-rand 0
           (c/sample 10 (c/nat 0 12)))
         (list 9 3 8 7 7 4 5 12 11 12)))
  (is (= (pr/with-rand 0
           (c/sample 4 (c/decimal 0 5)))
         (list 3.654838800430298
               4.1572049260139465
               1.2026819586753845
               3.0317258834838867)))
  (is (= (pr/with-rand 0
           (c/sample 4 (c/bag [:ok :ko])))
         (list :ko :ok :ko :ko))))

(deftest combinators

  (testing "data"
    (is (= (pr/with-rand 0
             (c/sample 6 (c/data [c/coin c/cube])))
           (list [false 2]
                 [false 4]
                 [false 2]
                 [true 6]
                 [false 6]
                 [true 1])))
    (is (= (pr/with-rand 0
             (c/sample 6 {:valid c/coin
                          :coordinates [(c/decimal 0 10) (c/decimal 0 20)]}))
           (list {:valid false, :coordinates [2.405363917350769 12.126903533935547]}
                 {:valid false, :coordinates [5.504369735717773 2.340131998062134]}
                 {:valid false, :coordinates [3.3321839570999146 5.055522918701172]}
                 {:valid true, :coordinates [9.84841525554657 19.656389951705933]}
                 {:valid false, :coordinates [9.41249132156372 3.5195350646972656]}
                 {:valid true, :coordinates [1.2889713048934937 7.359514236450195]}))))

  (testing "others"
    (is (= (pr/with-rand 0
             (c/sample 3 (c/$ c/cube -)))
           (list -5 -2 -4)))
    (is (= (pr/with-rand 0
             (c/sample 10 (c/bind c/coin
                                  (fn [x] (if x (c/bag [:ok :yes]) (c/bag [:nop :flop]))))))
           (list :nop :flop :nop :yes :flop :ok :ok :flop :yes :yes)))
    (is (= (pr/with-rand 0
             (c/sample 10 (c/keep (c/nat 1 10)
                                  even?)))
           (list 8 6 6 4 4 10 10 2 2 6)))
    (is (= (pr/with-rand 0
             (c/sample 4 (c/one-of :yes :no)))
           (pr/with-rand 0
             (c/sample 4 (c/one-of* [:yes :no])))
           (list :no :yes :no :no)))
    (is (= (pr/with-rand 0
             (c/sample 10 (c/weighted {c/coin 1 (c/one-of :yes :no) 4})))
           (list :no :no :yes :yes :no :no :yes :yes false false)))))

(deftest collections
  (is (= (pr/with-rand 0
           (c/sample 5 (c/tup c/coin (c/nat 1 10))))
         (list [false 3] [false 6] [false 4] [true 10] [false 10])))
  (is (= (pr/with-rand 0
           (c/sample 5 (c/tup c/coin
                              (c/tup (c/one-of :yes :no) (c/nat 1 10))
                              (c/dice 7))))
         (list [false [:yes 7] 4] [false [:yes 4] 7] [false [:no 3] 1] [true [:yes 6] 7] [true [:no 5] 6])))
  (is (every? boolean?
              (concat (c/realise (c/seqof c/coin))
                      (c/realise (c/vecof c/coin))
                      (c/realise (c/setof c/coin)))))
  (is (= (pr/with-rand 0
           (c/sample 5 (c/seqof c/coin :size 4)))
         '((false false true false)
           (false true true false)
           (true true false false)
           (false false true true)
           (false true false true))))
  (is (= (pr/with-rand 0
           (c/sample 5 (c/vecof c/coin :min-size 2 :max-size 4)))
         (list [true false false false]
               [true false]
               [false true true true]
               [false false]
               [false true])))
  (is (= (pr/with-rand 0
           (c/sample 5 (c/mapof (c/bag (range 10)) c/coin :min-size 2 :max-size 4)))
         (list {2 false, 5 false, 3 true, 9 false}
               {2 true, 1 false, 5 false}
               {7 false, 4 false}
               {5 false, 1 true, 9 false, 7 true}
               {0 true, 3 true, 8 false})))
  (is (= (pr/with-rand 0
           (c/sample 5 (c/rep c/cube 5)))
         (list [5 2 4 4 4]
               [2 3 6 6 6]
               [2 1 1 1 4]
               [6 1 4 3 5]
               [6 3 5 5 5]))))
