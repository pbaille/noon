(ns noon.utils.mapsets-test
  (:require [noon.utils.mapsets :as m]
            [clojure.test :refer [deftest testing is]]))

(deftest mapsets
  (testing "++"
    (is (= (m/++ #{} nil)
           #{}))
    (is (= (m/++ #{} {:a 1})
           #{{:a 1}}))
    (is (= (m/++ #{} {:a 1} {:a 1})
           #{{:a 1}}))
    (is (= (m/++ #{} nil {:a 1} () {:a 1})
           #{{:a 1}}))
    (is (= (m/++ #{} (list #{{:a 1}} {:b 2} nil))
           (m/++ #{} #{{:a 1}} {:b 2} nil)
           #{{:a 1} {:b 2}})))

  (testing "mk"
    (is (= (m/mk {:a 1})
           #{{:a 1}}))
    (is (= (m/mk (list {:a 1} {:a 2}))
           #{{:a 1} {:a 2}}))
    (is (= (m/mk #{{:a 1} {:a 2}})
           #{{:a 1} {:a 2}}))
    (is (= (m/mk nil)
           #{})))

  (testing "$"
    (is (= (m/$ #{{:a 1} {:a 2}}
                (fn [x] (assoc x :z 3)))
           #{{:a 1, :z 3} {:a 2, :z 3}}))
    (testing "map transformation"
      (is (= (m/$ #{{:a 1} {:a 2}}
                  {:a inc})
             #{{:a 3} {:a 2}})))
    (testing "flattening"
      (is (= (m/$ #{{:a 1} {:a 2}}
                  hash-set)
             (m/$ #{{:a 1} {:a 2}}
                  list)
             #{{:a 1} {:a 2}}))))

  (testing "upd"

    (is (= (m/upd #{{:a 1}} identity)
           ((m/->upd identity)
            #{{:a 1}})
           #{{:a 1}}))

    (is (= (m/upd #{{:a 1} {:a 2 :b 3}}
                  {:a inc})
           ((m/->upd {:a inc})
            #{{:a 1} {:a 2 :b 3}})

           #{{:a 2} {:b 3, :a 3}}))

    (is (= (m/upd #{{:a 1} {:a 2 :b 3}}
                  {:a inc}
                  {:a inc})
           ((m/->upd {:a inc}
                     {:a inc})
            #{{:a 1} {:a 2 :b 3}})
           #{{:a 3} {:b 3, :a 4}}))

    (is (thrown? #?(:clj Exception :cljs js/Error) (m/->upd nil))))

  (testing "split split-upd and shrink"
    (is (= (m/split #{{:a 1} {:a 2} {:a 3} {:a 4}}
                    {:a even?})
           [#{{:a 4} {:a 2}}
            #{{:a 1} {:a 3}}]))
    (is (= (m/shrink #{{:a 1} {:a 2} {:a 3} {:a 4}}
                     {:a even?})
           #{{:a 4} {:a 2}}))
    (is (= (m/split-upd #{{:a 1} {:a 2} {:a 3} {:a 4}}
                        {:a even?}
                        {:a inc})
           #{{:a 1} {:a 3} {:a 5}}))))
