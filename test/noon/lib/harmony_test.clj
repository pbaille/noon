(ns noon.lib.harmony-test
  (:use noon.score)
  (:refer-clojure :exclude [cat])
  (:require [noon.lib.harmony :as h]
            [clojure.test :refer [testing deftest is]]
            [noon.score :as n]
            [noon.test :as t]))

(defn pitch-values= [& xs]
    (apply = (map h/pitch-values xs)))

(deftest helpers
  (is (and (not (h/bounds-gte [0 1] [1 2]))
           (not (h/bounds-gte [0 1] [0 2]))))
  (is (and (h/bounds-gte [0 1] [0 0])
           (h/bounds-gte [0 2] [0 1])
           (h/bounds-gte [0 2] [0 2])
           (h/bounds-gte [0 3] [1 2])))

  (is (h/in-bounds [60 72]
                   (mk (tup s0 s1 s2))))
  (is (not (h/in-bounds [60 72]
                        (mk (tup s0 [o1 d1])))))
  (is (not (h/in-bounds [60 72]
                        (mk d1-)))))

(deftest voicings

  (testing "abstract drops"
    (is (= (h/abstract-drops 2 true)
           (list [[0 1]] [[1] [0]])))
    (is (= (h/abstract-drops 3 true)
           (list [[0 1 2]] [[1 2] [0]] [[0 2] [1]] [[2] [0 1]] [[1] [0 2]] [[2] [1] [0]])))
    (is (= (h/abstract-drops 4 true)
           (list [[0 1 2 3]] [[1 2 3] [0]] [[0 2 3] [1]] [[2 3] [0 1]] [[0 1 3] [2]] [[0 3] [1 2]] [[1 3] [0 2]] [[3] [0 1 2]]
                 [[0 2] [1 3]] [[1 2] [0 3]] [[1] [0 2 3]] [[2] [0 1 3]] [[1 3] [2] [0]] [[2 3] [1] [0]] [[2] [1 3] [0]]
                 [[3] [1 2] [0]] [[0 3] [2] [1]] [[2] [0 3] [1]] [[3] [0 2] [1]] [[3] [2] [0 1]] [[1] [0 3] [2]]
                 [[3] [1] [0 2]] [[2] [1] [0 3]] [[3] [2] [1] [0]])))
    (is (= (h/abstract-drops (list 0 1 1 2) true)
           (list [[0 1 2] [1]] [[1 2] [0 1]] [[0 1] [1 2]] [[1] [0 1 2]]
                 [[1 2] [1] [0]] [[1] [1 2] [0]] [[0 2] [1] [1]] [[1] [0 2] [1]]
                 [[2] [0 1] [1]] [[2] [1] [0 1]] [[1] [1] [0 2]] [[2] [1] [1] [0]]))))

  (testing "closed"

    (is (= (h/closed (mk (par s0 s2 s4)))
           (mk (par s0 s2 [o1- s4]))))
    (is (= (h/closed (mk (par s0 s2 s4 s6)))
           (mk (par s0 s2 [o1- s4] [o2- s6]))))
    (is (= (h/closed-no-unison (mk (par s0 s2 s4 s6)))
           (mk (par s0 s2 [o1- s4] [o1- s6])))))

  (testing "drops"
    (testing "simple-checks"
      (is (= (mk (par s0 s1 s2)
                 (h/drop 1))
             (mk (par s0 [o1 s1] s2))))
      (is (= (mk tetrad
                 (par s0 s1 s2 s3)
                 (h/drop 1))
             (mk tetrad
                 (par s0 [o1 s1] s2 s3))))
      (is (= (mk tetrad
                 (par s0 s1 s2 s3)
                 (h/drop 2))
             (mk tetrad
                 (par s0 s1 [o1 s2] s3))))
      (is (= (mk tetrad
                 (par s0 s1 s2 s3)
                 (h/drop -1))
             (mk tetrad
                 (par s0 [o2 s1] [o1 s2] s3))))

      (is (pitch-values= (mk (par s0 s1 s2 s3)
                             (h/drop 1))
                         (mk (par s0 [o1 s1] s2 s3)))))
    (testing "diatonic no duplicates"
      (is (t/frozen :diatonic-drops
            (par d0 d1 d2 d3)
            (sf_ (concat-scores (h/drops _ :inversions true)))))
      (is (t/frozen :diatonic-drops-no-inversions
            (par d0 d1 d2 d3)
            (sf_ (concat-scores (h/drops _))))))
    (testing "with two tonic"
      (is (t/frozen :drops-2-tonics
                    (par s0 s1 s2 s3)
                    (sf_ (concat-scores (h/drops _ :inversions true)))))
      (is (t/frozen :drops-2-tonics-no-inversions
                    (par s0 s1 s2 s3)
                    (sf_ (concat-scores (h/drops _))))))
    (testing "with duplicates"
      (is (t/frozen :diatonic-drops-with-duplicates
                    (par d0 d4 d8 d11)
                    (sf_ (concat-scores (h/drops _ :inversions true)))))
      (is (t/frozen :diatonic-drops-with-duplicates-no-inversions
                    (par d0 d1 d4 d8)
                    (sf_ (concat-scores (h/drops _)))))))

  (testing "inversions"

    (is (pitch-values= (mk (par s0 s1 s2)
                           (h/inversion 1))
                       (mk (par s1 s2 s3))))
    (is (pitch-values= (mk (par s0 s1 s2)
                           (h/inversion -1))
                       (mk (par s1- s0 s1))))
    (is (pitch-values= (mk (par s0 s1 s2)
                           (h/inversion 0))
                       (mk (par s0 s1 s2))))
    (is (pitch-values= (mk (par d0 d2 d4 d6 d8)
                           (h/inversion 1))
                       (mk (par d1 d4 d6 d7 d9))))
    (is (pitch-values= (mk (par d0 d2 d4 d6 d8)
                           (h/inversion -1))
                       (mk (par d1- d1 d2 d4 d7))))

    (testing "with duplicates"
      (is (pitch-values= (mk (par s0 s1 s2 s3)
                             (h/inversion 1))
                         (mk (par s1 s2 s3 s4))))
      (is (pitch-values= (mk (par s0 s1 s2 s3)
                             (h/inversion -1))
                         (mk (par s1- s0 s1 s2))))))

  (testing "voicings"
    (is (t/frozen :voicings1
                  tetrad
                  (par s0 s1 s2 s3)
                  (sf_ (concat-scores (h/voicings _ {:bounds [48 84]})))))
    (testing "with duplicates"
      (is (t/frozen :voicings-with-duplicates
                    (par s0 s1 s2 s3)
                    (sf_ (concat-scores (h/voicings _ {:bounds [48 84]})))))))

  (testing "voice-led"
    (is (t/frozen :voice-leading1
                  (cat I VI II V)
                  ($ tetrad h/simple-chord)
                  h/voice-led)))

  (testing "align-contexts"
    (is (t/frozen :align-contexts-diatonic-incremental
                  (catn> 4 (transpose c4))
                  (h/align-contexts :diatonic :incremental)
                  ($ h/simple-chord)))
    (is (t/frozen :align-contexts-diatonic-static
                  (catn> 4 (transpose c4))
                  (h/align-contexts :diatonic :static)
                  ($ h/simple-chord)))
    (is (t/frozen :align-contexts-structural-static
                  (catn> 12 (transpose c1))
                  (h/align-contexts :structural :static)
                  ($ tetrad h/simple-chord)))
    (is (t/frozen :align-contexts-structural-incremental
                  (catn> 12 (transpose c1))
                  (h/align-contexts :structural :incremental)
                  ($ tetrad h/simple-chord)))))


(comment

  (defmacro log [x & xs]
    `(noon.score/sfn ~x (noon.score/pp ~@xs) ~x))

  (def spy
    (log _ [:spy (map (juxt pitch-value
                            (comp :position :pitch))
                      (sort-by :position _))]))

  ;; harmonic-zip do not work well on complex scores
  ;; it can be used to pimp the base event but not really much more
  ;; double shifts can occur when the seed is not in zero position
  ;; TODO investigate the real value of harmonic-zip
  (noon {:play true :midi true
         :filename "test/data/hgrid"}
        (mk (h/harmonic-zip
             [tetrad
              (fit (append> (transpose c3) (transpose c6)))
              ($ C0 s-round (tup* (map redegree [0 5 1 4])))]

             (chans [(patch :acoustic-bass) C-2 t-round]
                    [(patch :electric-piano-1) vel5 (par s0 s1 s2 s3)]
                    [(patch :ocarina) o1 (tupn 14 (tup s0 s1 s2 s3))]))
            (adjust 16)))

  (stop)

  (noon {:play false
         :midi true
         :filename "test/data/aligned-contexts"}
        (mk dur4
            (append (transpose c3) (transpose c6))
            ($ [tetrad (tup I VI II V)])
            (h/align-contexts :s :static)
            ($ (par s0 s1 s2 s3))))

  (noon {:filename "test/data/drops"
         :midi true}
        (mk tetrad
            (par s0 s1 s2 s3 s4)
            (sf_ (concat-scores (h/drops _ :inversions true)))))

  (noon {:filename "test/data/inversions"
         :midi true}
        (mk tetrad
            (par s0 s1 s2 s3 s4)
            (sf_ (let [{:keys [upward downward self]} (h/inversions _ [40 80])]
                   (concat-scores (concat (reverse (next downward)) [self] (next upward)))))))

  (noon {:filename "test/data/voicings"
         :midi true}
        (mk tetrad
            (par s0 s1 s2 s3)
            (sf_ (concat-scores (h/voicings _ {:bounds [40 100]})))))

  (h/shiftings (mk tetrad (par s0 s1 s2 s3))
               [40 100])
  (h/drops (mk tetrad (par s0 s1 s2 s3))
           :inversions true)
  (h/voicings (mk tetrad (par s0 s1 s2 s3))
              {:bounds [40 100]}))

(comment :scratch-abstract-drop-remap
         (let [contour (list 0 0 1 2)
               notes (list :a :b :c :d)
               idx->notes (reduce (fn [ret [c n]]
                                    (update ret c (fnil conj []) n))
                                  {}
                                  (map vector contour notes))
               drop [[0 2] [1] [0]]]
           (loop [ret #{} drop drop octave 0 idx->notes idx->notes]
             (if-let [[current-octave & upper-octaves] (seq drop)]
               (if-let [[idx & current-octave] (seq current-octave)]
                 (recur (conj ret [octave (first (get idx->notes idx))])
                        (cons current-octave upper-octaves)
                        octave
                        (update idx->notes idx rest))
                 (recur ret upper-octaves (inc octave) idx->notes))
               ret)))

         (defn abstract-drop->score
           [abstract-drop idx->notes]
           (loop [ret #{} drop abstract-drop octave 0 idx->notes idx->notes]
             (if-let [[current-octave & upper-octaves] (seq drop)]
               (if-let [[idx & current-octave] (seq current-octave)]
                 (recur (conj ret ((n/t-shift octave) (first (get idx->notes idx))))
                        (cons current-octave upper-octaves)
                        octave
                        (update idx->notes idx rest))
                 (recur ret upper-octaves (inc octave) idx->notes))
               ret)))

         (let [contour (list 0 0 1 2)
               notes (list :a :b :c :d)
               idx->notes (reduce (fn [ret [c n]]
                                    (update ret c (fnil conj []) n))
                                  {}
                                  (map vector contour notes))
               drop [[0 2] [1] [0]]]
           (abstract-drop->score drop idx->notes)))
