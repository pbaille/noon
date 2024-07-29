(ns noon.harmony-test
  (:require [noon.harmony :as h]
            [clojure.test :refer [deftest testing is]]))

(defn limit-bds [bds & [limit]]
  (-> bds
      (update :fw (fn [xs] (vec (take (or limit 10) xs))))
      (update :bw (fn [xs] (vec (take (or limit 10) xs))))))

(deftest bds

  (let [m6 (h/bds [0 3 7 9] 12)]

    (is (= (limit-bds m6)
           {:fw [0 3 7 9 12 15 19 21 24 27],
            :bw [0 -3 -5 -9 -12 -15 -17 -21 -24 -27]}))

    (is (= (h/bds-get m6 1)
           3))
    (is (= (h/bds-get m6 4)
           12))
    (is (= (h/bds-get m6 -1)
           -3))
    (is (= (h/bds-get m6 -3)
           -9))

    (is (= (limit-bds (h/bds-shift m6 -1))
           {:bw [-3 -5 -9 -12 -15 -17 -21 -24 -27 -29],
            :fw [-3 0 3 7 9 12 15 19 21 24]}))

    (is (= (limit-bds m6)
           (limit-bds (h/bds-shift (h/bds-shift m6 -1) 1))))

    (is (= (limit-bds (h/bds-shift m6 4))
           {:fw [12 15 19 21 24 27 31 33 36 39],
            :bw [12 9 7 3 0 -3 -5 -9 -12 -15]}))

    (is (= (limit-bds (h/bds-shift m6 -3))
           {:bw [-9 -12 -15 -17 -21 -24 -27 -29 -33 -36],
            :fw [-9 -5 -3 0 3 7 9 12 15 19]}))

    (is (= (h/bds-idx m6 9)
           [3 0]))
    (is (= (h/bds-idx m6 8)
           [2 1]))
    (is (= (h/bds-idx m6 6)
           [1 3]))
    (is (= (h/bds-idx m6 -8)
           [-2 -3]))

    (is (= (limit-bds (h/bds-go m6 9))
           (limit-bds (h/bds-shift m6 3))))

    (is (= (limit-bds (h/bds-go m6 9))
           (limit-bds (h/bds-go m6 10))
           (limit-bds (h/bds-shift m6 3))))))

(deftest harmonic-context

  (testing "build"

    (is (= (h/hc)
           {:scale [0 2 4 5 7 9 11],
            :struct [0 2 4],
            :origin {:d 35, :c 60},
            :position {:t 0, :s 0, :d 0, :c 0}})))

  (testing "position"

    (is (fn? (h/position 0 0 0 0)))

    (is (= (h/hc)
           ((h/position 0 0 0 0) (h/hc))))

    (is (= ((h/position 0 0 3)
            (h/hc))
           {:scale [0 2 4 5 7 9 11],
            :struct [0 2 4],
            :origin {:d 35, :c 60},
            :position {:t 0, :s 0, :d 3}}))
    (is (h/diatonic? ((h/position 0 0 3) (h/hc))))
    (is (not (h/structural? ((h/position 0 0 3) (h/hc)))))

    (is (= ((h/position 0 1)
            (h/hc))
           {:scale [0 2 4 5 7 9 11],
            :struct [0 2 4],
            :origin {:d 35, :c 60},
            :position {:t 0, :s 1}}))
    (is (h/structural? ((h/position 0 1) (h/hc))))
    (is (not (h/tonic? ((h/position 0 1) (h/hc))))))

  (testing "position converters"

    (testing "chromatic"
      (is (= (h/c->d ((h/position 0 0 0 3) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 1, :c 1}}))
      (is (= (h/c->d ((h/position 0 0 0 -3) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d -2, :c 0}}))
      (is (= (h/c->s ((h/position 0 0 0 7) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2, :d 0, :c 0}}))
      (is (= (h/c->s ((h/position 0 0 0 6) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 1, :c 1}}))
      (is (= (h/c->t ((h/position 0 0 0 13) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 1, :s 0, :d 0, :c 1}})))

    (testing "diatonic"
      (is (= (h/d->s ((h/position 0 0 3) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 1}}))
      (is (= (h/d->t ((h/position 0 0 8) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 1, :s 0, :d 1}}))
      (is (= (h/d->c ((h/position 0 0 -3) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :c -5}})))

    (testing "structural"
      (is (= (h/s->c ((h/position 0 3) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :c 12}}))
      (is (= (h/s->d ((h/position 0 3) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :d 7}}))
      (is (= (h/s->t ((h/position 0 3) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 1, :s 0}})))

    (testing "tonic"
      (is (= (h/t->s ((h/position 1) (h/hc)))
             (h/down-to-layer :s ((h/position 1) (h/hc)))
             (h/down-to-layer :structural ((h/position 1) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:s 3}}))
      (is (= (h/t->d ((h/position 1) (h/hc)))
             (h/down-to-layer :d ((h/position 1) (h/hc)))
             (h/down-to-layer :diatonic ((h/position 1) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:d 7}}))
      (is (= (h/t->c ((h/position 1) (h/hc)))
             (h/down-to-layer :c ((h/position 1) (h/hc)))
             (h/down-to-layer :chromatic ((h/position 1) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:c 12}})))

    (testing "layer idx"
      (is (= (h/layer-idx :s (h/upd (h/hc) (h/position 3 2 1 0)))
             11))
      (is (= (h/layer-idx :d (h/upd (h/hc) (h/position 3 2 1 0)))
             26))
      (is (= (h/layer-idx :d (h/upd (h/hc) (h/position 0 0 1 0)))
             1)))

    (testing "views"

      (is (= (h/hc->pitch (h/hc))
             {:d 35, :c 60}))
      (is (= (h/hc->pitch ((h/position 1 0 0 0) (h/hc)))
             {:d 42, :c 72}))
      (is (= (h/hc->pitch ((h/position 0 1 0 0) (h/hc)))
             {:d 37, :c 64}))
      (is (= (h/hc->pitch ((h/position 0 0 1 0) (h/hc)))
             {:d 36, :c 62}))
      (is (= (h/hc->pitch ((h/position 0 0 1 1) (h/hc)))
             {:d 36, :c 63}))

      (is (= (h/chromatic-distance ((h/position 0 1 0 0) (h/hc))
                                   ((h/position 0 2 0 0) (h/hc)))
             3))
      (is (= (h/chromatic-distance ((h/position 0 1 0 0) (h/hc))
                                   ((h/position 0 2 1 0) (h/hc)))
             5))
      (is (= (h/diatonic-distance ((h/position 1 1 0 0) (h/hc))
                                  ((h/position 0 2 1 0) (h/hc)))
             4))
      (is (= (h/diatonic-distance ((h/position 0 1 0 0) (h/hc))
                                  ((h/position 0 2 1 0) (h/hc)))
             3))

      (is (= (h/upd (h/hc)
                    (h/pitch->position (h/hc) (h/hc->pitch ((h/position 0 0 1 1) (h/hc)))))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 1, :c 1}})))

    (testing "intervals"
      (is (= (h/t-trim ((h/position 0 0 1 12) (h/hc)))
             (h/t-trim ((h/position 1 1 1 1) (h/hc)))
             (h/t-trim ((h/position 0 3 1 1) (h/hc)))
             (h/t-trim ((h/position 0 0 6 1) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 1}}))
      (is (= (h/s-trim ((h/position 0 0 1 7) (h/hc)))
             (h/s-trim ((h/position 0 0 2 6) (h/hc)))
             (h/s-trim ((h/position 0 1 1 3) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2}}))
      (is (= (h/d-trim ((h/position 0 0 0 7) (h/hc)))
             (h/d-trim ((h/position 0 0 1 6) (h/hc)))
             (h/d-trim ((h/position 0 0 2 3) (h/hc)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 4}}))
      (is (= (h/upd (h/hc)
                    (h/t-step 3))
             (h/upd ((h/position 0 2 1 1) (h/hc))
                    (h/t-step 3))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 3}}))
      (is (= (h/upd ((h/position 0 2 1 1) (h/hc))
                    (h/t-shift 3))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 3, :s 2, :d 1, :c 1}}))
      (is (= (h/upd ((h/position 0 2 1 1) (h/hc))
                    (h/t-shift 3))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 3, :s 2, :d 1, :c 1}}))
      (is (= (h/upd (h/hc)
                    (h/s-step 3))
             (h/upd ((h/position 0 0 1 1) (h/hc))
                    (h/s-step 3))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 3}}))
      (is (= (h/upd ((h/position 0 2 1 1) (h/hc))
                    (h/s-shift 3))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 5, :d 1, :c 1}}))
      (is (= (h/upd ((h/position 0) (h/hc))
                    (h/s-shift 3))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0}}))
      (is (= (h/upd ((h/position 0) (h/hc))
                    (h/s-shift 3 :forced))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 3}}))
      (is (= (h/upd (h/hc)
                    (h/d-step 3))
             (h/upd ((h/position 0 0 0 1) (h/hc))
                    (h/d-step 3))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 3}}))
      (is (= (h/upd ((h/position 0 2 1 1) (h/hc))
                    (h/d-shift 3))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 2, :d 4, :c 1}}))
      (is (= (h/upd ((h/position 0) (h/hc))
                    (h/d-shift 3))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0}}))
      (is (= (h/upd ((h/position 0) (h/hc))
                    (h/d-shift 3 :forced))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :d 3}})))

    (testing "position rounding"

      (is (= (h/t-round ((h/position 0) (h/hc)))
             ((h/position 0) (h/hc))))
      (is (= (h/t-round ((h/position 0 1) (h/hc)))
             ((h/position 0) (h/hc))))
      (is (= (h/t-round ((h/position 0 2) (h/hc)))
             ((h/position 1) (h/hc))))
      (is (= (h/t-round ((h/position 0 2) (h/hc)))
             ((h/position 1) (h/hc))))
      (is (= (h/t-round ((h/position 1 0 7 2) (h/hc)))
             ((h/position 2) (h/hc))))

      (is (= (h/s-round ((h/position 0) (h/hc)))
             ((h/position 0) (h/hc))))
      (is (= (h/s-round ((h/position 0 2) (h/hc)))
             ((h/position 0 2) (h/hc))))
      (is (= (h/s-round ((h/position 0 1 1 1) (h/hc)))
             ((h/position 0 2) (h/hc))))
      (is (= (h/s-round ((h/position 0 2 -1 1) (h/hc)))
             (h/s-round ((h/position 0 0 4 -1) (h/hc)))
             (h/s-round ((h/position 0 0 0 7) (h/hc)))
             (h/s-round ((h/position 0 0 0 8) (h/hc)))
             (h/s-round ((h/position 0 0 0 6) (h/hc)))
             ((h/position 0 2) (h/hc))))

      (is (= (h/d-round ((h/position 1) (h/hc)))
             ((h/position 1) (h/hc))))
      (is (= (h/d-round ((h/position 1 1) (h/hc)))
             ((h/position 1 1) (h/hc))))
      (is (= (h/d-round ((h/position 1 0 0 2) (h/hc)))
             ((h/position 1 0 1) (h/hc))))
      (is (= (h/d-round ((h/position 1 0 0 8) (h/hc)))
             ((h/position 1 0 4) (h/hc)))))

    (testing "normalise"

      (is (= (h/upd (h/hc)
                    (h/d-position 10)
                    h/normalise)
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 1, :s 1, :d 1, :c 0}}))

      (is (= (h/upd (h/hc)
                    (h/position 1 4 3 -2)
                    h/normalise)
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 3, :s -1, :d 0, :c 0}}))

      (is (= (h/hc->chromatic-value
              (h/upd (h/hc)
                     (h/position 1 4 3 -2)))
             91))

      (is (= (h/hc->chromatic-value
              (h/upd (h/hc)
                     (h/position 1 4 4 -2)
                     h/normalise))
             93)))

    (testing "update-constructors"
      (is (= (h/upd (h/hc)
                    (h/origin :E0))
             (h/upd (h/hc)
                    (h/origin "E0"))
             (h/upd (h/hc)
                    (h/origin 'E0))
             (h/upd (h/hc)
                    (h/origin 64))
             (h/upd (h/hc)
                    (h/origin {:c 64 :d 37}))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 37, :c 64}, :position {:t 0, :s 0, :d 0, :c 0}}))

      (is (thrown? Exception (h/upd (h/hc)
                                    (h/origin "pouet"))))
      (is (= (h/upd (h/hc)
                    (h/scale :dorian))
             (h/upd (h/hc)
                    (h/scale 'dorian))
             (h/upd (h/hc)
                    (h/scale "dorian"))
             {:scale [0 2 3 5 7 9 10], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}))

      (is (thrown? Exception (h/upd (h/hc)
                                    (h/scale "durian"))))

      (is (= (h/upd (h/hc)
                    (h/scale :dorian))
             (h/upd (h/hc)
                    (h/scale 'dorian))
             (h/upd (h/hc)
                    (h/scale "dorian"))
             {:scale [0 2 3 5 7 9 10], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}))

      (is (= (h/upd (h/hc)
                    (h/struct :tetrad))
             (h/upd (h/hc)
                    (h/struct "tetrad"))
             (h/upd (h/hc)
                    (h/struct 'tetrad))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4 6], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}))

      (is (= (h/upd (h/hc)
                    (h/repitch :E0))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 0, :c 0}}))

      (is (= (h/upd (h/hc)
                    (h/position 0 1)
                    (h/rebase (h/scale :dorian)))
             (h/upd (h/hc)
                    (h/position 0 1)
                    (h/rescale :dorian))
             {:scale [0 2 3 5 7 9 10], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 1, :d 0, :c 1}})))

    (testing "update"

      (is (= (h/upd (h/hc)
                    (fn [ctx] (assoc ctx :what :ever)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}, :what :ever}))

      (is (= (h/upd (h/hc)
                    (h/scale :phrygian))
             {:scale [0 1 3 5 7 8 10], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}))

      (is (= (h/upd ((h/position 0 2 1 0) (h/hc))
                    (h/hc))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 1, :s -1, :d 1, :c 0}}))

      (is (= (h/upd (h/hc) nil) (h/hc)))

      (is (thrown? Exception (h/upd (h/hc)
                                    :anything))))

    (testing "extras"

      (is (= (h/upd (h/hc)
                    (h/root :E))
             (h/upd (h/hc)
                    (h/root "E"))
             (h/upd (h/hc)
                    (h/root 'E))
             (h/upd (h/hc)
                    (h/root 4))
             (h/upd (h/hc)
                    (h/root {:c 4 :d 2}))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 37, :c 64}, :position {:t 0, :s 0, :d 0, :c 0}}))
      (is (= (h/upd (h/hc)
                    (h/degree 1))
             {:scale [0 2 3 5 7 9 10], :struct [0 2 4], :origin {:d 36, :c 62}, :position {:t 0, :s 0, :d 0, :c 0}}))
      (is (= (h/upd (h/hc)
                    (h/degree -1))
             {:scale [0 1 3 5 6 8 10], :struct [0 2 4], :origin {:d 34, :c 59}, :position {:t 0, :s 0, :d 0, :c 0}}))
      (is (= (h/upd (h/hc)
                    (h/inversion -1))
             {:scale [0 2 4 5 7 9 10], :struct [0 3 5], :origin {:d 32, :c 55}, :position {:t 0, :s 0, :d 0, :c 0}}))
      (is (= (h/upd (h/hc)
                    (h/inversion 1))
             {:scale [0 1 3 5 7 8 10], :struct [0 2 5], :origin {:d 37, :c 64}, :position {:t 0, :s 0, :d 0, :c 0}}))

      (is (= (h/upd (h/hc)
                    (h/redegree 2))
             {:scale [0 1 3 5 7 8 10], :struct [0 2 4], :origin {:d 37, :c 64}, :position {:t 0, :s -1, :d 1, :c 0}}))
      (is (= (h/hc->pitch (h/upd (h/hc)
                                 (h/redegree 2)))
             (h/hc->pitch (h/hc))))
      (is (let [hc (h/normalise ((h/position 1 -1 2 -3) (h/hc)))]
            (= (h/hc->pitch (h/upd hc
                                   (h/redegree 2)))
               (h/hc->pitch hc))))
      (is (= (h/upd (h/hc)
                    (h/transpose (h/inversion 1)))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 37, :c 64}, :position {:t 0, :s 0, :d 0, :c 0}}))
      (is (= (h/upd (h/hc)
                    (h/position 0 1 1 0)
                    (h/hc+ ((h/position 0 1 1 0) (h/hc))))
             (h/upd (h/hc)
                    (h/position 0 1 1 0)
                    (h/scale :dorian)
                    (h/hc+ ((h/position 0 1 1 0) (h/hc))))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 1, :s 0, :d -1, :c 0}}))

      (is (= (h/align :diatonic
                      (h/upd (h/hc) (h/scale :lydian) (h/root :E))
                      (h/upd (h/hc) (h/scale :dorian) (h/root :D)))
             {:scale [0 2 3 5 7 9 10], :struct [0 2 4], :origin {:d 36, :c 62}, :position {:t 0, :s 1, :d -1}}))

      (is (= (h/align :diatonic
                      (h/upd (h/hc) (h/scale :dorian) (h/root :D))
                      (h/upd (h/hc) (h/scale :lydian) (h/root :E)))
             {:scale [0 2 4 6 7 9 11], :struct [0 2 4], :origin {:d 37, :c 64}, :position {:t 0, :s 0, :d -1}}))

      (is (= (h/upd (h/hc)
                    (h/mirror :G0))
             {:scale [0 2 4 5 7 9 11], :struct [0 2 4], :origin {:d 35, :c 60}, :position {:t 1, :s 0, :d 1, :c 0}})))

    (deftest "passing TODO")

    (deftest "connections TODO")))
