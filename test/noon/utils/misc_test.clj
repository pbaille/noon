(ns noon.utils.misc-test
  (:require [noon.utils.misc :as u]
            [clojure.test :as t :refer [deftest testing is]]))

(deftest numbers
  (testing "abs"
    (is (= 3 (u/abs -3) (u/abs 3)))
    (is (= 0 (u/abs -0) (u/abs 0))))

  (testing "divmod"
    (is (= (u/divmod 3 7) [2 1])))

  (testing "round"
    (is (= (u/round 5.75) 6.0))
    (is (= (u/round 5.25) 5.0)))

  (testing "rounded-div"
    (is (= (u/rounded-div 5 2) 2))
    (is (= (u/rounded-div 6 2) 3)))

  (testing "dist"
    (is (= (u/dist 6 2) 4))
    (is (= (u/dist 2 6) 4)))

  (testing "negate"
    (is (= (u/negate 6) -6))
    (is (= (u/negate -6) 6)))

  (testing "scale-range"
    (is (= (u/scale-range 4 [1 5] [2 6]) 5))
    (is (= (u/scale-range 4 [0 10] [0 30]) 12))
    (is (= (u/scale-range 0.2 [0 10] [0 2]) 0.04)))

  (testing "linear-interpolation"
    (is (= (u/linear-interpolation 0 100 5) [0 25 50 75 100])))

  (testing "sums"
    (is (= (u/sums 5 2 [1 2 3 4])
           '((1 4) (2 3))))
    (is (= (u/sums 5 3 [1 2 3 4])
           '((1 1 3) (1 2 2))))
    (is (= (u/sums 15 3 (range 15))
           '((0 1 14) (0 2 13) (0 3 12)
                      (0 4 11) (0 5 10) (0 6 9)
                      (0 7 8) (1 1 13) (1 2 12)
                      (1 3 11) (1 4 10) (1 5 9)
                      (1 6 8) (1 7 7) (2 2 11)
                      (2 3 10) (2 4 9) (2 5 8)
                      (2 6 7) (3 3 9) (3 4 8)
                      (3 5 7) (3 6 6) (4 4 7)
                      (4 5 6) (5 5 5))))
    (is (nil? (u/sums 5 2 [3 4 5 6]))))

  (testing "lazy-primes"
    (is (= (take 5 (u/lazy-primes)) '(2 3 5 7 11))))

  (testing "factorize"
    (is (= (u/factorize 11) [1 11]))
    (is (= (u/factorize 10) [1 2 5])))

  (testing "rand-int-between"
    (let [result (u/rand-int-between 5 10)]
      (is (and (>= result 5) (<= result 10))))))


(deftest metadata

  (testing "t"
    (is (= {} (u/t :pouet {})))
    (is (= :pouet (u/t (u/t :pouet {}))))
    (is (nil? (u/t 3)))
    (is (->> (with-meta {} {:foo 3}) (u/t :baz) meta (= {:foo 3, :type :baz}))))

  (testing "t?"
    (is (u/t? :foo (u/t :foo {})))
    (is ((u/t? :foo) (u/t :foo {})))
    (is (not (u/t? :foo {})))
    (is (not ((u/t? :foo) {}))))

  (testing "t="
    (is (u/t= (u/t :foo {}) (u/t :foo [])))
    (is (not (u/t= (u/t :foo []) (u/t :fu [])))))

  (testing "flagged"
    (is (= (u/flagged :foo :bar {}) {}))
    (is (u/flagged? :foo (u/flagged :foo :bar {})))
    (is (u/flagged? :bar (u/flagged :foo :bar {})))
    (is (not (u/flagged? :baz (u/flagged :foo :bar {}))))))

(u/defn* defn-star-test
  "just for tests"
  {:meta :data}
  [xs]
  (reduce + 0 xs))

(deftest macros
  (testing "parse-defn"
    (is (= (u/parse-defn '(plus [a b] (+ 1 2)))
           '{:name plus,
             :doc nil,
             :attrs nil,
             :arities (([a b] (+ 1 2)))}))
    (is (= (u/parse-defn '(plus "doc" [a b] (+ 1 2)))
           '{:name plus,
             :doc "doc",
             :attrs nil,
             :arities (([a b] (+ 1 2)))}))
    (is (= (u/parse-defn '(plus "doc" {:meta :data} [a b] (+ 1 2)))
           '{:name plus,
             :doc "doc",
             :attrs {:meta :data}  ,
             :arities (([a b] (+ 1 2)))}))
    (is (= (u/parse-defn '(plus {:meta :data} [a b] (+ 1 2)))
           '{:name plus,
             :doc nil,
             :attrs {:meta :data}  ,
             :arities (([a b] (+ 1 2)))}))
    (is (= (u/parse-defn '(plus {:meta :data} ([x y] (+ x y)) ([x y & more] (reduce plus (plus x y) more))))
           '{:name plus,
             :doc nil,
             :attrs {:meta :data},
             :arities (([x y] (+ x y)) ([x y & more] (reduce plus (plus x y) more)))})))

  (testing "defn*"
    (is (= 6
           (defn-star-test 1 2 3)
           (defn-star-test* [1 2 3])))
    (is (let [{:keys [doc meta]} (meta #'defn-star-test*)]
          (and (= meta :data)
               (= doc "just for tests")))))

  (testing ">_ and f_"
    (is (= (u/>_ 0 (inc _) (+ _ _ _))
           3))
    (is (= (u/>_ 0)
           0))
    (is (= ((u/f_ (inc _))
            0)
           1))
    (is (= ((u/f_ (inc _) (+ _ _ _))
            0)
           3))))

(deftest colls

  (testing "snoc"
    (is (= (u/snoc (list 1 2 3)
                   4)
           '(1 2 3 4)))
    (is (= (u/snoc [] 1)
           '(1)))
    (is (= (u/snoc nil 1)
           '(1))))

  (testing "$"
    (is (= (u/$ '(0 1 2) inc)
           '(1 2 3)))
    (is (= (u/$ [0 1 2] inc)
           [1 2 3]))
    (is (= (u/$ #{0 1 2} inc)
           #{1 2 3}))
    (is (= (u/$ {:a 1 :b 2}
                (fn [[k v]] [v k]))
           {1 :a, 2 :b})))

  (testing "deep-check"
    (is (u/deep-check {:a {:b {:c 1}}}))
    (is (not (u/deep-check {:a {:b {:c 1 :d false}}}))))

  (testing "deep-merge"
    (is (= (u/deep-merge {:a {:b {:c 1} :d 2}} {:a {:b {:f 9}}})
           {:a {:b {:c 1, :f 9}, :d 2}}))
    (is (= (u/deep-merge {:a {:b {:c 1} :d 2}} nil)
           (u/deep-merge nil {:a {:b {:c 1} :d 2}})
           {:a {:b {:c 1} :d 2}})))

  (testing "deep-find"
    (is (u/deep-find [0 [8 5 {:c [#{1}]}]] 1))
    (is (not (u/deep-find [0 [8 5 {:c [#{1}]}]]
                          9))))

  (testing "map-vals"
    (is (= (u/map-vals inc {:a 1 :b 2})
           {:a 2, :b 3})))

  (testing "hm-nodes"
    (is (= (u/hm-nodes {:a 1 :b 2 :c {:d 2 :e {:f 4 :g [3 {:h 23}]}}})
           {[] {:a 1, :b 2, :c {:d 2, :e {:f 4, :g [3 {:h 23}]}}},
            [:a] 1,
            [:b] 2,
            [:c] {:d 2, :e {:f 4, :g [3 {:h 23}]}},
            [:c :d] 2,
            [:c :e] {:f 4, :g [3 {:h 23}]},
            [:c :e :f] 4,
            [:c :e :g] [3 {:h 23}]}))
    (is (= (u/hm-nodes {})
           {[] {}})))

  (testing "hm-leaves"
    (is (= (u/hm-leaves {:a 1 :b 2 :c {:d 2 :e {:f 4 :g [3 {:h 23}]}}})
           {[:a] 1, [:b] 2, [:c :d] 2, [:c :e :f] 4, [:c :e :g] [3 {:h 23}]}))
    (is (= (u/hm-leaves {})
           {})))

  (testing "index-of"
    (is (= (u/index-of [1 2 :x 4] :x) 2))
    (is (= (u/index-of [:x  2 :x 4] :x) 0))
    (is (= (u/index-of [1 2 3 :x] :x) 3))
    (is (not (u/index-of [1 2 3 4] :x)))))

(u/defreduction my-plus [x y] (+ x y))

(defn cons-if-gte-first [acc x]
  (when (<= (first acc) x) (cons x acc)))

(u/defreduction my-redtest
  "Add elements to the head of the list if they are sorted"
  [x y]
  (cons-if-gte-first x y))

(deftest more-macros

  (testing "reductions"
    (is (= (my-plus 1 2 3 4)
           ((u/reduction +) 0 1 2 3 4)
           10))
    (is (= (my-redtest (list 0) 1 2 3 4)
           ((u/reduction cons-if-gte-first) (list 0) 1 2 3 4)
           (list 4 3 2 1 0)))
    (is (not ((u/reduction cons-if-gte-first)
              (list 0) 1 4 3 4)))
    (is (not (my-redtest (list 0) 1 4 3 4)))))
