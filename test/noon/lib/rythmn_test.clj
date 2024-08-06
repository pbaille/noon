(ns noon.lib.rythmn-test
  (:use noon.score)
  (:require [noon.test :as t]
            [noon.lib.rythmn :as r]
            [clojure.test :refer [deftest testing is]]))

(deftest main
  (testing "various"
    (is (t/frozen :example1
                  dur4
                  dorian
                  (chans [(patch :vibraphone) vel4 (par> d3 d3 d3)]
                         [(patch :taiko-drum) (r/gen-tup 9 3 :durations [1 2 3]) (each (one-of vel4 vel3) (maybe d3 d3-))]
                         [(patch :acoustic-bass) o1- (r/gen-bintup 9 5)
                          (parts {:bintup 0} _
                                 {:bintup 1} (each (one-of vel0 d3-)))]
                         [(patch :ocarina)
                          (r/gen-bintup 27 11  :shifted :euclidean)
                          (each (one-of vel0 vel3 vel5 vel7))
                          (parts {:bintup 0} (each (one-of d2 d4 d6))
                                 {:bintup 1} (each (one-of d3 d5 d7)))])
                  (append [rev lydian (transpose c4)] [lydian+2 (transpose c4-)] melodic-minor)
                  (append lydian)
                  (dup 2)))

    (is (t/frozen (patch :tinkle-bell)
                  (r/gen-tup 8 5 :durations [2 1/2 1])
                  (dup 4)))

    (is (t/frozen (patch :tinkle-bell)
                  dur2
                  (par [o1- (dupt 2)]
                       (r/gen-tup 12 5 :durations [2 1/2 1 3])
                       [o1 (r/gen-tup 12 7 :durations [2 1/2 1 3])])
                  (dup 4)))

    "shifted"
    (is (t/frozen (patch :tinkle-bell)
                  dur2
                  (par [o1- (dupt 2)]
                       (r/gen-tup 8 5 :shifted :durations [1/4 1/2 1 2 4]))
                  (dup 4)))

    (is (t/frozen (patch :tinkle-bell)
                  dur2
                  (par [o1- (dupt 2)]
                       (r/gen-tup 12 5 :shifted)
                       [o1 (r/gen-tup 12 5 :shifted)])
                  (dup 4)))

    (is (t/frozen (patch :tinkle-bell)
                  dur2
                  (par [o1- (dupt 2)]
                       (r/gen-tup 12 5 :shifted :durations [1 2 3])
                       [o1 (r/gen-tup 12 7 :shifted :durations [2 1 3])])
                  (dup 4)))

    (is (t/frozen (chans
                   [(patch :woodblock) o2-]
                   [(patch :woodblock) (tup dur2 dur3 dur3)
                    (r/rotation :rand-sub 5)])
                  (dup 4)))

    (is (t/frozen :rythmic-permutation-demo-re
                  (chans
                ;; beat
                   [(patch :taiko-drum) vel5 (dup 4)]
                ;; rythmic permutations
                   [(patch :woodblock)
                    (r/sum->tup [2 1 1 1/2 1/2])
                    (each (maybe o1 o1-))
                    (nlin 4 (r/permutation 5))]
                ;; chords
                   [(patch :electric-piano-1)
                    o1- vel4 lydian
                    (par> d0 d3 d3 d3 d3)
                    (lin (root :C) (root :Eb) (root :Ab) (root :Db))])
               ;; loop 4
                  (dup 4)))

    :euclidean

    (is (t/frozen (patch :tinkle-bell)
                  dur2
                  (chans o1-
                         (r/gen-tup 12 5 :euclidean)
                         [o1 (r/gen-tup 12 7 :euclidean :shifted)])
                  (dup 4)))

    (is (t/frozen (patch :tinkle-bell)
                  (let [rtup (! (r/gen-tup 16 5 :euclidean :shifted))]
                    (chans (ntup 2 o1-)
                           rtup
                           [o1 rtup]
                           [o2 rtup]
                           #_[oct3 rtup]))

                  (dup 4)
                  (adjust {:duration 8})))))
