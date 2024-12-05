(ns noon.utils.maps-test
  (:require [clojure.test :refer [deftest testing is]]
            [noon.utils.maps :refer [++ value-merge upd check match]]))

(deftest test-value-merge

  (testing "x or y are nil"
    (is (= (value-merge 1 nil) 1))
    (is (= (value-merge nil 1) 1)))

  (testing "y is a function"
    (is (= (value-merge 1 inc) 2))
    (is (= (value-merge nil (constantly 2)) 2)))

  (testing "y is a map"
    (is (= (value-merge {:a 1} {:a inc :b 2})
           {:b 2, :a 2}))
    (is (= (value-merge nil {:b 2})
           {:b 2}))
    (is (= (value-merge nil {:a (fnil inc 0) :b 2})
           {:b 2, :a 1}))))

(deftest test-++

  (testing "nested value merging"
    (is (= (++ {:a 1} {:a 2})
           {:a 2}))
    (is (= (++ {:a {:b 2} :c 3} {:a {:b 1}})
           {:c 3, :a {:b 1}}))
    (is (= (++ {:a {:b 2} :c 3} {:a 1})
           {:c 3, :a 1}))
    (is (= (++ {:a {:b 2} :c 3} {:a nil})
           {:a {:b 2} :c 3}))
    (is (= (++ {:a {:b 2} :c 3} {:a {:b nil}})
           {:a {:b 2} :c 3})))

  (testing "function updates"
    (is (= (++ {:a 1} {:a inc})
           {:a 2}))
    (is (= (++ {:a {:b 2} :c 3} {:a {:b dec}})
           {:c 3, :a {:b 1}}))
    (is (= (++ {:a {:b 2} :c 3} {:a count})
           {:c 3, :a 1}))
    (is (= (++ {:x {:a 1 :b 2}} {:x (fn [x] (zipmap (keys x) (map inc (vals x))))})
           {:x {:a 2, :b 3}})))

  (testing "variadic args"
    (is (= (++ {:a 1} {:b 2} {:b inc :a dec})
           {:b 3, :a 0}))
    (is (= (++ {:a 1} nil {:a dec})
           {:a 0}))))

(deftest test-upd

  (testing "various setups"
    (is (= (upd {:a 2} (fn [x] (assoc x :count (count x))))
           {:a 2, :count 1}))
    (is (= (upd {:a 2} {:a inc})
           {:a 3}))
    (is (= (upd {:a {:b 1 :c 3}} {:a {:b dec}})
           {:a {:c 3, :b 0}}))
    (is (= (upd {:a {:b 1 :c 3}} {:a count})
           {:a 2}))
    (is (= (upd {:a 1} {:p identity})
           {:a 1}))
    (is (thrown? #?(:clj Exception :cljs js/Error) (upd {} nil)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (upd {} 1)))))

(deftest test-check

  (testing "simple"
    (is (check {:a 1} map?))
    (is (check {:a 1} {:a pos?}))
    (is (not (check {:a 1} {:a neg?}))))

  (testing "nested"
    (is (check {:a 1 :b {:c 2}} {:b {:c even?}}))
    (is (check {:a 1 :b {:c 2}} {:b {:c 2}}))
    (is (not (check {:a 1 :b {:c 2}} {:b {:c odd?}})))))

(deftest test-match

  (testing "simple"
    (is (match {} map?))
    (is (match {:a 1} {:a pos?}))
    (is (not (match {:a 1} {:a 2})))
    (is (not (match {:a 1} {:a even?})))
    (is (not (match {:a 1} {:a even? :b identity}))))

  (testing "nested"
    (is (match {:a 1 :b {:c 2}} {:b {:c 2} :a pos?}))
    (is (not (match {:a 1 :b {:c 2}} {:b {:c 3}})))))

(comment :tries2

         (require '[noon.utils.chance :as c])

         (def sampler
           (c/setof (c/mapof (c/bag [:a :b :c :d :e :f])
                             (c/nat -10 10)
                             :size 3)
                    :size 20))

         (-> (sampler)
             ($ (f_ (assoc _ :sum (reduce + (vals _)))))
             (split {:sum pos?}))

         (split (sampler)
                {:a number?})

         (split (sampler)
                {:a 0})

         (-> (sampler)
             (split-upd (f_ (contains? _ :a))
                        ($_ (assoc _ :double-a (* (:a _) 2))))))
