(ns noon.lib.harmony-test
  (:require [noon.lib.harmony :as h]
            [clojure.test :refer [testing deftest is]]
            [noon.eval :refer [score]]
            [noon.freeze :refer [freeze]]))

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
                   (score (tup s0 s1 s2))))
  (is (not (h/in-bounds [60 72]
                        (score (tup s0 [o1 d1])))))
  (is (not (h/in-bounds [60 72]
                        (score d1-)))))

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

    (is (= (h/closed (score (par s0 s2 s4)))
           (score (par s0 s2 [o1- s4]))))
    (is (= (h/closed (score (par s0 s2 s4 s6)))
           (score (par s0 s2 [o1- s4] [o2- s6]))))
    (is (= (h/closed-no-unison (score (par s0 s2 s4 s6)))
           (score (par s0 s2 [o1- s4] [o1- s6])))))

  (testing "drops"
    (testing "simple-checks"
      (is (= (score (par s0 s1 s2)
                    (h/drop 1))
             (score (par s0 [o1 s1] s2))))
      (is (= (score tetrad
                    (par s0 s1 s2 s3)
                    (h/drop 1))
             (score tetrad
                    (par s0 [o1 s1] s2 s3))))
      (is (= (score tetrad
                    (par s0 s1 s2 s3)
                    (h/drop 2))
             (score tetrad
                    (par s0 s1 [o1 s2] s3))))
      (is (= (score tetrad
                    (par s0 s1 s2 s3)
                    (h/drop -1))
             (score tetrad
                    (par s0 [o2 s1] [o1 s2] s3))))

      (is (pitch-values= (score (par s0 s1 s2 s3)
                                (h/drop 1))
                         (score (par s0 [o1 s1] s2 s3)))))
    (testing "diatonic no duplicates"
      (freeze (score (par d0 d1 d2 d3)
                     (sf_ (score/concat-scores (h/drops _ :inversions true)))))
      (freeze (score (par d0 d1 d2 d3)
                     (sf_ (score/concat-scores (h/drops _))))))
    (testing "with two tonic"
      (freeze (score (par s0 s1 s2 s3)
                     (sf_ (score/concat-scores (h/drops _ :inversions true)))))
      (freeze (score (par s0 s1 s2 s3)
                     (sf_ (score/concat-scores (h/drops _))))))
    (testing "with duplicates"
      (freeze (score
               (par d0 d4 d8 d11)
               (sf_ (score/concat-scores (h/drops _ :inversions true)))))
      (freeze (score
               (par d0 d1 d4 d8)
               (sf_ (score/concat-scores (h/drops _)))))))

  (testing "inversions"

    (is (pitch-values= (score (par s0 s1 s2)
                              (h/inversion 1))
                       (score (par s1 s2 s3))))
    (is (pitch-values= (score (par s0 s1 s2)
                              (h/inversion -1))
                       (score (par s1- s0 s1))))
    (is (pitch-values= (score (par s0 s1 s2)
                              (h/inversion 0))
                       (score (par s0 s1 s2))))
    (is (pitch-values= (score (par d0 d2 d4 d6 d8)
                              (h/inversion 1))
                       (score (par d1 d4 d6 d7 d9))))
    (is (pitch-values= (score (par d0 d2 d4 d6 d8)
                              (h/inversion -1))
                       (score (par d1- d1 d2 d4 d7))))

    (testing "with duplicates"
      (is (pitch-values= (score (par s0 s1 s2 s3)
                                (h/inversion 1))
                         (score (par s1 s2 s3 s4))))
      (is (pitch-values= (score (par s0 s1 s2 s3)
                                (h/inversion -1))
                         (score (par s1- s0 s1 s2))))))

  (testing "voicings"
    (freeze (score tetrad
                   (par s0 s1 s2 s3)
                   (sf_ (score/concat-scores (h/voicings _ {:bounds [48 84]})))))
    (testing "with duplicates"
      (freeze (score (par s0 s1 s2 s3)
                     (sf_ (score/concat-scores (h/voicings _ {:bounds [48 84]})))))))

  (testing "voice-led"
    (freeze (score (lin I VI II V)
                   (each tetrad h/simple-chord)
                   h/voice-led)))

  (testing "align-contexts"
    (freeze (score (nlin> 4 (transpose c4))
                   (h/align-contexts :diatonic :incremental)
                   (each h/simple-chord)))
    (freeze (score (nlin> 4 (transpose c4))
                   (h/align-contexts :diatonic :static)
                   (each h/simple-chord)))
    (freeze (score (nlin> 12 (transpose c1))
                   (h/align-contexts :structural :static)
                   (each tetrad h/simple-chord)))
    (freeze (score (nlin> 12 (transpose c1))
                   (h/align-contexts :structural :incremental)
                   (each tetrad h/simple-chord)))))

(deftest parsed-updates
  (testing "lin"
    (freeze (score (h/lin :IM7 :VIIm7b5 :III7 :VI7 :IIm7 :II7 :V7sus4 :V7b9omit1)
                   (chans (each (par s0 s1 s2 s3))
                          [(patch :ocarina) o1
                           (each [(mixtup s0 s1 s2 s3)
                                  (shuftup s0 (one-of s3- s2- s1- s1 s2 s3))])])
                   (dup 2)))

    (freeze (score (h/tup :IM7 :V/III7 :IIIm7 :bIIIM7 :V/V7sus4 :bVI7#11omit5 :V7sus4 :bIIM7)
                   (chans (each [(par s0 s2 s3) (mixtup s0 s1 s2)])
                          [(patch :acoustic-bass) C-2 t-round])
                   (dup 2)
                   (adjust 16))))

  (is (= (score (h/upd :EM7) (par s0 s1 s2 s3))
         (score (root :E) tetrad C0 (par s0 s1 s2 s3))))

  (testing "mixed parsed, unparsed, event-update, score-update"
    (is (= (score (h/upd (dup 2) :E7sus4 vel2))
           #{{:patch [0 4], :channel 0,
              :pitch {:scale [0 2 4 5 7 9 10], :structure [0 3 4 6], :origin {:d 37, :c 64}, :position {:t 0, :s -2, :d 0, :c 1}},
              :voice 0, :duration 1, :position 1, :velocity 21, :track 0}
             {:position 0, :channel 0, :track 0, :duration 1,
              :pitch {:scale [0 2 4 5 7 9 10], :structure [0 3 4 6], :origin {:d 37, :c 64}, :position {:t 0, :s -2, :d 0, :c 1}},
              :velocity 21, :voice 0, :patch [0 4]}}))))

(comment

  (play (h/upd :Emix :7sus413 :omit1)
        (chans [(patch :acoustic-bass) C-2 t-round (dupt 4)]
               [(shuftup s0 s1 s2 s3)
                (each (shuftup (par _ s3) (par s1 s2)))
                (tup _ s1- s2-)
                (each (vel-humanize 10))])
        (adjust 8))

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
        (score (h/harmonic-zip
             [tetrad
              (fit (append> (transpose c3) (transpose c6)))
              (each C0 s-round (tup* (map redegree [0 5 1 4])))]

             (chans [(patch :acoustic-bass) C-2 t-round]
                    [(patch :electric-piano-1) vel5 (par s0 s1 s2 s3)]
                    [(patch :ocarina) o1 (ntup 14 (tup s0 s1 s2 s3))]))
            (adjust 16)))

  (stop)

  (noon {:play false
         :midi true
         :filename "test/data/aligned-contexts"}
        (score dur4
            (append (transpose c3) (transpose c6))
            (each [tetrad (tup I VI II V)])
            (h/align-contexts :s :static)
            (each (par s0 s1 s2 s3))))

  (noon {:filename "test/data/drops"
         :midi true}
        (score tetrad
            (par s0 s1 s2 s3 s4)
            (sf_ (score/concat-scores (h/drops _ :inversions true)))))

  (noon {:filename "test/data/inversions"
         :midi true}
        (score tetrad
            (par s0 s1 s2 s3 s4)
            (sf_ (let [{:keys [upward downward self]} (h/inversions _ [40 80])]
                   (score/concat-scores (concat (reverse (next downward)) [self] (next upward)))))))

  (noon {:filename "test/data/voicings"
         :midi true}
        (score tetrad
            (par s0 s1 s2 s3)
            (sf_ (score/concat-scores (h/voicings _ {:bounds [40 100]})))))

  (h/shiftings (score tetrad (par s0 s1 s2 s3))
               [40 100])
  (h/drops (score tetrad (par s0 s1 s2 s3))
           :inversions true)
  (h/voicings (score tetrad (par s0 s1 s2 s3))
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
