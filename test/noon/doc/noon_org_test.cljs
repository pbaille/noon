(ns noon.doc.noon-org-test
  (:require [cljs.test :as t]
            [noon.eval :refer [play noon score]]
            [noon.freeze]))
(t/deftest noon-tests
  (t/testing "Noon"
    (t/testing "Elements"
      (t/testing "Building blocks"
        (t/testing "Events"
          (t/is (noon.freeze/freeze noon.events/DEFAULT_EVENT)))
        (t/testing "Score" (t/is (noon.freeze/freeze (score)))))
      (t/testing "Top form"
        (t/is (noon.freeze/freeze (quote (noon <option-map> <score>))))
        (t/is (noon.freeze/freeze (noon {:play true} (score))))
        (t/testing "Options"
          (t/is (noon.freeze/freeze (noon {:play true, :tracks {0 :chorium}}
                                          (score dur2
                                                 (rup 8 d1)
                                                 (lin (patch :clarinet)
                                                      (patch :electric-piano-1)
                                                      (patch :trumpet)
                                                      (patch :ocarina))))))
          (t/is (noon.freeze/freeze (require (quote noon.midi))))
          (t/is (noon.freeze/freeze (def bus1
                                      (noon.midi/get-output-device "Bus 1"))))
          (t/is (noon.freeze/freeze (def bus1-sequencer
                                      (noon.midi/init-device-sequencer bus1))))
          (t/is (noon.freeze/freeze (noon {:play true,
                                           :tracks {0 bus1-sequencer}}
                                          (score (par s0 s1 s2)))))
          (t/testing "mp3 export"
            (t/is (noon.freeze/freeze (noon {:mp3 true}
                                            (score (tup s0 s1 s2)))))))
        (t/testing "score"
          (t/is (noon.freeze/freeze (score)))
          (t/is (noon.freeze/freeze #{{:position 0,
                                       :duration 1,
                                       :channel 0,
                                       :track 0,
                                       :velocity 80,
                                       :pitch {:scale [0 2 4 5 7 9 11],
                                               :structure [0 2 4],
                                               :origin {:d 35, :c 60},
                                               :position
                                                 {:t 0, :s 0, :d 0, :c 0}},
                                       :voice 0,
                                       :patch [0 4]}}))
          (t/is (noon.freeze/freeze
                  (quote (score transformation1 transformation2 ...)))))
        (t/testing "=noon.eval/play="
          (t/is (noon.freeze/freeze
                  (quote (play transformation1 transformation2 ...))))
          (t/is (noon.freeze/freeze
                  (quote (noon {:play true}
                               (mk transformation1 transformation2 ...)))))
          (t/is (noon.freeze/freeze (play dur2 (tup s0 s1 s2 s3))))))
      (t/testing "Transformations 1"
        (t/testing "Pitches"
          (t/is (noon.freeze/freeze (play Eb0)))
          (t/is (noon.freeze/freeze (play F#-1)))
          (t/is (noon.freeze/freeze (play Gb2))))
        (t/testing "Durations"
          (t/is (noon.freeze/freeze (play dur2)))
          (t/is (noon.freeze/freeze (play dur:3)))
          (t/is (noon.freeze/freeze (dur 2)))
          (t/is (noon.freeze/freeze (dur (/ 1 4))))
          (t/is (noon.freeze/freeze (dur (fn [x] (* x 2)))))
          (t/is (noon.freeze/freeze (play (dur (/ 1 4))))))
        (t/testing "Velocities"
          (t/is (noon.freeze/freeze (play vel0)))
          (t/is (noon.freeze/freeze (play vel3)))
          (t/is (noon.freeze/freeze (play vel8)))
          (t/is (noon.freeze/freeze (play vel12)))
          (t/is (noon.freeze/freeze (play (vel 100))))
          (t/is (noon.freeze/freeze (play (vel (fn [x] (/ x 2)))))))
        (t/testing "Composition"
          (t/is (noon.freeze/freeze (play [Eb0 dur:2])))
          (t/is (noon.freeze/freeze (play [F#-1 dur4 (vel 127)])))
          (t/is (noon.freeze/freeze (play [(vel 127) dur4 F#-1])))
          (t/is (noon.freeze/freeze (play F#-1 dur4))))
        (t/testing "Concatenation"
          (t/is (noon.freeze/freeze (play (lin C0 E0 G0 B0))))
          (t/is (noon.freeze/freeze
                  (play (lin [C0 dur:2] [Eb0 dur:4] [G0 dur:4] C1)))))
        (t/testing "Superposition"
          (t/is (noon.freeze/freeze (play (par C0 Eb0 G0))))
          (t/is (noon.freeze/freeze (play vel2 dur2 (par C0 F0 G0)))))
        (t/testing "Sounds"
          (t/is (noon.freeze/freeze (play (patch :clarinet)
                                          (lin C0 E0 G#0 B0))))
          (t/is (noon.freeze/freeze (play (patch :vibraphone)
                                          [dur:4 (lin C0 E0 G0 (par D1 B0))])))
          (t/is (noon.freeze/freeze noon.vst.general-midi/summary)))
        (t/testing "Channels"
          (t/is (noon.freeze/freeze
                  (play (chans [(patch :ocarina) dur:2
                                (lin G0 Eb0 C0 G-1 F0 D0 A-1 F-1)]
                               [(patch :vibraphone) dur2 vel3
                                (lin (par C0 Eb0 G0) (par A-1 F0 D0))]
                               [(patch :acoustic-bass) (lin [dur3 C-2] G-2)])
                        (dup 4))))))
      (t/testing "Transformations 2"
        (t/testing "Intervals 1"
          (t/testing "Steps"
            (t/testing "Chromatic"
              (t/is (noon.freeze/freeze (c-step 3)))
              (t/is (noon.freeze/freeze (c-step -1)))
              (t/is (noon.freeze/freeze c1))
              (t/is (noon.freeze/freeze c2-))
              (t/is (noon.freeze/freeze (play c3)))
              (t/is (noon.freeze/freeze (play (c-step -3))))
              (t/is (noon.freeze/freeze (play c12-))))
            (t/testing "Diatonic"
              (t/is (noon.freeze/freeze (d-step 1)))
              (t/is (noon.freeze/freeze (d-step -1)))
              (t/is (noon.freeze/freeze (d-step 4)))
              (t/is (noon.freeze/freeze d1))
              (t/is (noon.freeze/freeze d2-))
              (t/testing "Example"
                (t/is (noon.freeze/freeze (play dur:4
                                                (lin d0 d1 d2 d3 d4 d5 d6 d7))))
                (t/is (noon.freeze/freeze
                        (play dur:4 (lin d0 d2 d1 d3 d2 d4 d3 d5 d4))))
                (t/is (noon.freeze/freeze
                        (play dur:4 (lin d0 d2- d1- d3- d2- d4- d3- d5- d4-))))
                (t/is (noon.freeze/freeze (play
                                            dur:4
                                            (root :Eb)
                                            (scale :hungarian)
                                            (lin d0 d1 d2 d3 d4 d5 d6 d7)))))))
          (t/testing "Octaves"
            (t/is (noon.freeze/freeze (play (t-shift 1))))
            (t/is (noon.freeze/freeze (play (t-shift -1))))
            (t/is (noon.freeze/freeze (play o2-)))))
        (t/testing "lin"
          (t/is (noon.freeze/freeze (play (lin C0 E0 G0 B0))))
          (t/is (noon.freeze/freeze
                  (play dur:8 (lin c0 c3 c6) (lin c0 c2 c3 c5)))))
        (t/testing "tup"
          (t/is (noon.freeze/freeze (play (tup c1 c2 c3 c4 c5 c6 c7 c8))))
          (t/is (noon.freeze/freeze
                  (play (tup c0 c2 c4 c7) (tup c0 c3) (rep 3 c4-)))))
        (t/testing "dup"
          (t/is (noon.freeze/freeze (play (tup c0 c3 c6 c9) (dup 3)))))
        (t/testing "rep"
          (t/is (noon.freeze/freeze (play dur:4 (rep 8 c4))))
          (t/is (noon.freeze/freeze (play (rep 6 (tup c5 c10)))))
          (t/is (noon.freeze/freeze (play (rep 3 o1 :skip-first)))))
        (t/testing "fit"
          (t/is (noon.freeze/freeze (play (tup c0 c4) (fit (rep 4 c2)))))
          (t/is (noon.freeze/freeze (= (score (tup c0 c3 c8))
                                       (score (fit (lin c0 c3 c8))))))
          (t/is (noon.freeze/freeze (play (rup 15 d1))))
          (t/is (noon.freeze/freeze (play (tup d0 d3 d6 d7) (dupt 3)))))
        (t/testing "nlin"
          (t/is (noon.freeze/freeze (play (nlin 4 (tup d0 d1 d2 d3)))))
          (t/is (noon.freeze/freeze (play (tup d0 d1 d2 d3) (dup 4)))))
        (t/testing "ntup"
          (t/is (noon.freeze/freeze (play (ntup 4 (tup d0 d1 d2 d3))))))
        (t/testing "lin>"
          (t/is (noon.freeze/freeze (play (lin> c0 c2 c2 c2 c2 c2 c2)))))
        (t/testing "tup>"
          (t/is (noon.freeze/freeze (play (tup> d0 d1 d1 d1 d1 d1 d1 d1))))))
      (t/testing "Polyphony"
        (t/is (noon.freeze/freeze (play (par c0 c3 c7 c9 c14))))
        (t/is (noon.freeze/freeze (play (par c10 c0 c16 c5))))
        (t/is (noon.freeze/freeze (play (patch :electric-piano-1)
                                        (par (tup d0 d2 d4 o1)
                                             [vel3 (par> o1 d4)
                                              (fit (rep 8 d1))]
                                             o1-))))
        (t/is (noon.freeze/freeze
                (play o1 (tup c0 (par c15 c10) c9 (par c6 c4)) (rep 3 c3))))
        (t/is (noon.freeze/freeze (play (par (rep 12 c1) (rep 12 c1-)))))
        (t/is (noon.freeze/freeze (play (par> d0 d2 d2 d2 d2))))
        (t/is (noon.freeze/freeze
                (play (patch :string-ensemble-1) o2- (par> c0 c7 c7 c7 c7 c7))))
        (t/testing "Channels"
          (t/is (noon.freeze/freeze (play (chans c0 c3 c7))))
          (t/is (noon.freeze/freeze (chan 1)))
          (t/is (noon.freeze/freeze (chan 3)))
          (t/is (noon.freeze/freeze (chan inc)))
          (t/is (noon.freeze/freeze
                  (play (par [(chan 0) c0] [(chan 1) c3] [(chan 2) c7])))))
        (t/testing "Tracks"
          (t/is (noon.freeze/freeze (play (patch :flute)
                                          (tracks (tup> c0 c5 c5 c5- c2- c7-)
                                                  (tup> c0 c2- c5 c5))
                                          (dup 4))))
          (t/is (noon.freeze/freeze (track 1)))
          (t/is (noon.freeze/freeze (track 12)))
          (t/is (noon.freeze/freeze (track (fn [x] (+ x 3)))))))
      (t/testing "Mapping"
        (t/is (noon.freeze/freeze (play (lin c0 c1 c2 c3) (tup c0 o1))))
        (t/is (noon.freeze/freeze (play (lin c0 c1 c2 c3) (each (tup c0 o1)))))
        (t/is (noon.freeze/freeze (play (lin c0 o1)
                                        (each [dur:4 (rep 8 c1-)])))))
      (t/testing "Dynamism"
        (t/testing "Star functions"
          (t/is (noon.freeze/freeze (tup c1 c2 c3)))
          (t/is (noon.freeze/freeze (tup* [c1 c2 c3])))
          (t/is (noon.freeze/freeze (tup* (list c1 c2 c3)))))
        (t/testing "Map functions"
          (t/is (noon.freeze/freeze (play {:velocity (fn [x] (/ x 2)),
                                           :duration (fn [x] (* x 2))}))))
        (t/testing "Examples"
          (t/is (noon.freeze/freeze (play (tup* (shuffle [c0 c3 c7 c9])))))
          (t/is (noon.freeze/freeze (play (patch :electric-piano-1)
                                          (tup* (map (fn [v] {:velocity v})
                                                  (range 0 127 15))))))))
      (t/testing "Non determinism"
        (t/is (noon.freeze/freeze (play (rand-nth [(tup c0 c4 c7)
                                                   (tup c0 c3 c7)])
                                        (rep 4 (rand-nth [c3 c4 c3- c4-])))))
        (t/testing "one-of"
          (t/is (noon.freeze/freeze (play (one-of o1- o1))))
          (t/is (noon.freeze/freeze (play dur:8 (rep 50 (one-of c1 c1-))))))
        (t/testing "maybe"
          (t/is (noon.freeze/freeze (play (maybe o1 o2))))
          (t/is (noon.freeze/freeze (play (one-of same o1 o2))))
          (t/is (noon.freeze/freeze (play dur:8 (rep 50 (maybe c1 c1-))))))
        (t/testing "probs"
          (t/is (noon.freeze/freeze (play (probs {o1 4, o1- 1}))))
          (t/is (noon.freeze/freeze
                  (play dur:4 (rep 24 (probs {c1 6, c6- 1, (par c0 o1-) 1}))))))
        (t/testing "any-that"
          (t/is (noon.freeze/freeze (play dur:8
                                          (rep 60
                                               (any-that
                                                 (within-pitch-bounds? :C-1 :C1)
                                                 c2
                                                 c5
                                                 c7
                                                 c2-
                                                 c5-
                                                 c7-))))))
        (t/testing "!"
          (t/is (noon.freeze/freeze
                  (play (nlin 4 (! (tup* (shuffle [d0 d2 d4 d6])))))))
          (t/is (noon.freeze/freeze
                  (play (nlin 4 (tup* (shuffle [d0 d2 d4 d6])))))))
        (t/testing "Shuffling"
          (t/is (noon.freeze/freeze (play (shuftup d0 d2 d4 d6))))
          (t/is (noon.freeze/freeze (play (shuflin d0 d2 d4 d6))))))
      (t/testing "Harmony"
        (t/testing "Intervals 2"
          (t/testing "Steps"
            (t/testing "Structural"
              (t/is (noon.freeze/freeze (play (s-step 1))))
              (t/is (noon.freeze/freeze (play (s-step 2))))
              (t/is (noon.freeze/freeze (play s1)))
              (t/is (noon.freeze/freeze (play s2)))
              (t/is (noon.freeze/freeze (play s1-)))
              (t/testing "Examples"
                (t/testing "Arpegios"
                  (t/is (noon.freeze/freeze (play (tup s0 s1 s2 s3))))
                  (t/is (noon.freeze/freeze (play (rup 6 s1))))
                  (t/is (noon.freeze/freeze
                          (play (rep 4 s1-)
                                (each (tup> s2 s2 s2 s1- s2- s1-))))))
                (t/testing "Passing tones"
                  (t/is (noon.freeze/freeze (play (scale :eolian)
                                                  dur:2
                                                  o2
                                                  (rep 12 s1-)
                                                  (each
                                                    (tup s0 c1- d1 s0))))))))
            (t/testing "Tonic"
              (t/is (noon.freeze/freeze (play (t-step 1))))
              (t/is (noon.freeze/freeze (play (t-step -1))))
              (t/is (noon.freeze/freeze (play t1)))
              (t/is (noon.freeze/freeze (play t2)))
              (t/is (noon.freeze/freeze (play t1-)))
              (t/testing "Examples"
                (t/is (noon.freeze/freeze (play (rup 4 t1))))
                (t/is (noon.freeze/freeze
                        (play (rep 3 t1) (each (tup> s0 s1 s1 d1-)))))))))
        (t/testing "Implementation"
          (t/is (noon.freeze/freeze (= (:pitch noon.events/DEFAULT_EVENT)
                                       {:scale [0 2 4 5 7 9 11],
                                        :structure [0 2 4],
                                        :origin {:d 35, :c 60},
                                        :position {:t 0, :s 0, :d 0, :c 0}}))))
        (t/testing "Shifts"
          (t/is (noon.freeze/freeze (play (t-shift 1))))
          (t/is (noon.freeze/freeze (play (t-step 1))))
          (t/is (noon.freeze/freeze (play s1 (t-shift 1))))
          (t/is (noon.freeze/freeze (play s1 (t-step 1)))))
        (t/testing "Tonality"
          (t/testing "scale"
            (t/is (noon.freeze/freeze noon.constants/modes))
            (t/is (noon.freeze/freeze (play (scale :dorian) dur:4 (rep 8 d1))))
            (t/is (noon.freeze/freeze (mk harmonic-minor))))
          (t/testing "structure"
            (t/is (noon.freeze/freeze noon.constants/structures))
            (t/is (noon.freeze/freeze (score (structure :tetrad))))
            (t/is (noon.freeze/freeze (score sus47))))
          (t/testing "origin"
            (t/is (noon.freeze/freeze (score (origin :Eb0))))
            (t/testing "Examples"
              (t/is (noon.freeze/freeze
                      (play (lin (origin :C0) (origin :E0) (origin :G#0))
                            (each (rup 6 s1)))))))
          (t/testing "root"
            (t/is (noon.freeze/freeze (score (root :D))))
            (t/is (noon.freeze/freeze (score (root :B))))
            (t/testing "Examples"
              (t/is (noon.freeze/freeze (play (lin* (map root [:C :E :G#]))
                                              (each (chans (par d0 d3 d6 d9)
                                                           [(rup 4 d3)
                                                            (rup 3 d2)]))
                                              (rep 4 s1))))))
          (t/testing "transpose"
            (t/is (noon.freeze/freeze (play (scale :lydianb7)
                                            (rup 6 d2)
                                            (rep 4 (transpose c3-))))))
          (t/testing "rebase"
            (t/is (noon.freeze/freeze (score (rebase (root :E)))))
            (t/is (noon.freeze/freeze (= (get-in (score (rebase (root :E)))
                                                 [:pitch :position])
                                         {:t 0, :s -1, :d 0, :c 1})))
            (t/is (noon.freeze/freeze (score (rebase (root :E)
                                                     (scale :mixolydianb6))))))
          (t/testing "degree"
            (t/is (noon.freeze/freeze (score (degree 2))))
            (t/is (noon.freeze/freeze (score (scale :melodic-minor)
                                             (degree -1))))
            (t/is (noon.freeze/freeze (play (patch :trumpet)
                                            (lin I IV V I)
                                            (each (tup s0 s1 s2)))))))))
    (t/testing "Composing"
      (t/is (noon.freeze/freeze (require (quote [noon.lib.harmony :as h])
                                         (quote [noon.lib.melody :as m])
                                         (quote [noon.lib.rythmn :as r])
                                         (quote [noon.utils.sequences :as
                                                 seqs]))))
      (t/testing "Melody"
        (t/testing "Bounding"
          (t/testing "within-pitch-bounds?"
            (t/is (noon.freeze/freeze
                    (= (score Eb0 (within-pitch-bounds? :C-1 :C0)) nil)))
            (t/is (noon.freeze/freeze
                    (= (score Eb0 (within-pitch-bounds? :C0 :C1)) (score Eb0))))
            (t/is
              (noon.freeze/freeze
                (play
                  (patch :electric-piano-1)
                  dur:8
                  (rep
                    60
                    (any-that (within-pitch-bounds? :C0 :C1) c1 c1- c5 c5-)))))
            (t/is (noon.freeze/freeze
                    (play dur:8
                          (rep 60
                               (fst-that (within-pitch-bounds? :C0 :C1)
                                         (one-of c5 c5-)
                                         c2
                                         c2-)))))))
        (t/testing "Rotations"
          (t/testing "Example"
            (t/is (noon.freeze/freeze (play (fit (rep 8 d1)) (m/rotation 3)))))
          (t/testing "Forms"
            (t/is (noon.freeze/freeze (m/rotation 2)))
            (t/is (noon.freeze/freeze (m/rotation -3)))
            (t/is (noon.freeze/freeze (m/rotation (/ 1 2))))
            (t/is (noon.freeze/freeze (m/rotation - (/ 1 3))))
            (t/is (noon.freeze/freeze (m/rotation :rand)))
            (t/is (noon.freeze/freeze (m/rotation [0 (/ 1 2)]))))
          (t/testing "Chords"
            (t/is (noon.freeze/freeze (play (fit (rep 8 d1))
                                            (each (par d0 d3 d6))
                                            (m/rotation (/ 1 4)))))))
        (t/testing "Permutations"
          (t/testing "Forms"
            (t/is (noon.freeze/freeze (m/permutation 2)))
            (t/is (noon.freeze/freeze (m/permutation -1)))
            (t/is (noon.freeze/freeze (m/permutation (/ 1 2))))
            (t/is (noon.freeze/freeze (m/permutation - (/ 1 4))))
            (t/is (noon.freeze/freeze (m/permutation :rand)))
            (t/is (noon.freeze/freeze (m/permutation [(/ 1 4) - (/ 1 4)]))))
          (t/testing "Example"
            (t/is (noon.freeze/freeze (let [space [vel0 dur:8]]
                                        (play (patch :electric-piano-1)
                                              (tup d0 d2 d1 d3 d2 d4 d3 d5)
                                              (lin same
                                                   space
                                                   (m/permutation 1)
                                                   space
                                                   (m/permutation 2)
                                                   space
                                                   (m/permutation -
                                                                  (/ 1 4))))))))
          (t/testing "Options"
            (t/testing "Grade"
              (t/is (noon.freeze/freeze (require (quote [noon.utils.sequences
                                                         :as seqs]))))
              (t/is (noon.freeze/freeze (= (seqs/grade-permutations [0 1 2 3] 1)
                                           (quote
                                             ((2 3 0 1) (1 2 3 0) (3 0 1 2))))))
              (t/is (noon.freeze/freeze (m/permutation 0 {:grade 1})))
              (t/is (noon.freeze/freeze (m/permutation -1 {:grade [1 3]}))))
            (t/testing "Layers"
              (t/is (noon.freeze/freeze
                      (play dur2 (tup s0 s1 s2 s3) (each (tup d1 d1- d0)))))
              (t/is (noon.freeze/freeze (play dur2
                                              (tup s0 s1 s2 s3)
                                              (each (tup d1 d1- d0))
                                              (m/permutation 1
                                                             {:layer :s})))))))
        (t/testing "Mixed example"
          (t/is
            (noon.freeze/freeze
              (play
                {:description
                   "rand harmonic seq using IV II and VI degrees on vibraphone,\n   ocarina melody derives using transposition, rotation and permutation."}
                (chans [(patch :vibraphone) vel3
                        (ntup 4
                              [(one-of IV II VI) tetrad
                               (par [t2- vel5] s0 s1 s2 s3)])]
                       [(patch :ocarina) vel5 (shuftup d1 d2 d3 d4 d5)
                        (each (maybe (par d0 d3)))
                        (rup 16
                             (probs {(m/permutation :rand) 1,
                                     (m/rotation :rand) 3,
                                     (one-of* (map d-step (range -3 4))) 5}))])
                (adjust 10)
                (append [d2- (transpose c3)] [d2 (transpose c3-)] same)))))
        (t/testing "Contour"
          (t/is (noon.freeze/freeze (play (tup s0 s2 s1 s2))))
          (t/is (noon.freeze/freeze (play (tup s0 s3 s2 s3))))
          (t/is (noon.freeze/freeze (play (tup d0 d2 d1 d2))))
          (t/is (noon.freeze/freeze (play (tup d1 d5 d2 d5))))
          (t/is (noon.freeze/freeze (play (tup s2 s4 d8 s4))))
          (t/testing "Demo"
            (t/is (noon.freeze/freeze (play (tup s0 s1 s2 s3 s1 s2))))
            (t/is (noon.freeze/freeze (play (tup s0 s1 s2 s3 s1 s2)
                                            (m/contour :mirror))))
            (t/is (noon.freeze/freeze (play (tup s0 s1 s2 s3 s1 s2)
                                            (m/contour :rotation {:nth 1}))))
            (t/is (noon.freeze/freeze (play (tup s0 s1 s2 s3 s1 s2)
                                            (m/contour :rotation {:pick -1}))))
            (t/is (noon.freeze/freeze (play (tup s0 s1 s2 s3 s1 s2)
                                            (m/contour :rotation))))
            (t/is (noon.freeze/freeze (play (tup s0 s1 s2 s3 s1 s2)
                                            (m/contour :similar {:delta 1}))))
            (t/is (noon.freeze/freeze (play (tup s0 s1 s2 s3 s1 s2)
                                            (m/contour :mirror))))
            (t/is (noon.freeze/freeze (play (tup s0 s1 s2 s3 s1 s2)
                                            (m/contour :mirror {:layer :d}))))
            (t/is (noon.freeze/freeze (play (tup s0 s1 s2 s3 s1 s2)
                                            (m/contour :mirror {:layer :c}))))
            (t/is (noon.freeze/freeze
                    (play (tup s0 s1 s2 s3 s1 s2)
                          (m/contour :similar {:extent [-2 3], :layer :d}))))))
        (t/testing "Line"
          (t/is
            (noon.freeze/freeze
              (play
                {:description
                   "building a melodic line of 32 notes by chaining fragments of differerent lengths."}
                (patch :ocarina)
                dur:4
                (m/simple-line 32
                               (one-of (nlin> 4 (one-of d1- d1))
                                       (tup d1 d1- s0)
                                       (lin s2 s1 s1-)
                                       (nlin> 4 (one-of s1- s1)))))))
          (t/is
            (noon.freeze/freeze
              (play
                {:description
                   "another way to build a melodic line from a bunch of randomly chosen transformations."}
                (patch :acoustic-guitar-nylon)
                (repeat-while (within-time-bounds? 0 24)
                              (append [start-from-last
                                       (any-that (within-pitch-bounds? :C-1 :C2)
                                                 (rep 3 d3)
                                                 (rep 3 d3-)
                                                 d1
                                                 d1-)]))
                (adjust 3))))))
      (t/testing "Rythmn"
        (t/testing "Simple"
          (t/is (noon.freeze/freeze
                  (play (patch :woodblock)
                        dur:4
                        (lin same dur:2 dur:2 same dur2 same same))))
          (t/is (noon.freeze/freeze
                  (play (patch :woodblock) dur:4 (lin _ (tup _ _) _ dur2 _ _))))
          (t/is (noon.freeze/freeze
                  (play (patch :woodblock) dur:4 (lin _ (dupt 2) _ dur2 _ _))))
          (t/is (noon.freeze/freeze
                  (play (patch :woodblock)
                        dur2
                        (tup* (map dur [1 (/ 1 2) (/ 1 2) 1 2 1 1])))))
          (t/is (noon.freeze/freeze
                  (play dur2 (r/durtup 1 (/ 1 2) (/ 1 2) 1 2 1 1))))
          (t/is (noon.freeze/freeze
                  (play dur2 (r/durtup* [1 (/ 1 2) (/ 1 2) 1 2 1 1])))))
        (t/testing "Generation"
          (t/testing "Examples"
            (t/is (noon.freeze/freeze
                    (play (patch :woodblock) (r/gen-tup 8 5) (dup 4))))
            (t/is (noon.freeze/freeze (play (chans [(patch :tinkle-bell) o1-]
                                                   [(patch :woodblock)
                                                    (r/gen-tup 8 5)])
                                            (dup 4))))
            (t/is (noon.freeze/freeze
                    (play dur2
                          (chans [(patch :tinkle-bell) (tup o1- o1)]
                                 [(patch :woodblock) (r/gen-tup 16 8)])
                          (dup 4))))
            (t/is (noon.freeze/freeze
                    (play dur2
                          (chans [(patch :tinkle-bell) (tup o1- o1)]
                                 [(patch :woodblock) (r/gen-tup 12 6)
                                  (each (maybe o1 o1-))])
                          (dup 4))))
            (t/is (noon.freeze/freeze
                    (play dur2
                          (chans [(patch :tinkle-bell) (tup o1- o1)]
                                 [(patch :woodblock) (r/gen-tup 16 7 :shifted)
                                  (each (maybe o1 o1-))])
                          (dup 4))))
            (t/is (noon.freeze/freeze
                    (play dur2
                          (chans [(patch :tinkle-bell) (tup o1- o1)]
                                 [(patch :woodblock)
                                  (r/gen-tup 12 5 :durations [2 3])])
                          (dup 4))))
            (t/is (noon.freeze/freeze
                    (play (patch :tinkle-bell)
                          dur2
                          (par [o1- (dupt 2)]
                               (r/gen-tup 12 5 :shifted :durations [1 2 3])
                               [o1
                                (r/gen-tup 12 7 :shifted :durations [2 1 3])])
                          (dup 4))))
            (t/is (noon.freeze/freeze (play {:description "~trésillo"}
                                            (chans (patch :tinkle-bell)
                                                   [(patch :woodblock)
                                                    (r/gen-tup 8 3 :euclidean)])
                                            (dup 4))))
            (t/is (noon.freeze/freeze (play {:description "~bembé"}
                                            dur2
                                            (chans
                                              [(patch :tinkle-bell) (tup o1- _)]
                                              [(patch :woodblock)
                                               (r/gen-tup 12 7 :euclidean)])
                                            (dup 4))))
            (t/is (noon.freeze/freeze (play {:description "~bossa"}
                                            dur2
                                            (chans
                                              [(patch :tinkle-bell) (tup o1- _)]
                                              [(patch :woodblock)
                                               (r/gen-tup 16 5 :euclidean)])
                                            (dup 4))))
            (t/is (noon.freeze/freeze
                    (let [rtup (! (r/gen-tup 16 5 :euclidean :shifted))]
                      (play
                        (patch :tinkle-bell)
                        (chans (ntup 2 o1-) rtup [o1 rtup] [o2 rtup] [o3 rtup])
                        (dup 4)
                        (adjust {:duration 8})))))
            (t/is
              (noon.freeze/freeze
                (let [rtup (! [(r/gen-tup 16 5 :euclidean :shifted)
                               (each [(maybe o1 o2) (one-of vel4 vel6 vel8)])])]
                  (play mixolydian
                        (patch :vibraphone)
                        (lin same (transpose c4-))
                        (h/align-contexts)
                        (each (chans [(patch :tinkle-bell) o1-]
                                     [(patch :acoustic-bass) t1- (tup same s1-)]
                                     rtup
                                     [d4 rtup]
                                     [d6 rtup]
                                     [d10 rtup]))
                        (dup 8)
                        (adjust {:duration 32})))))))
        (t/testing "Transformation"
          (t/testing "noon.lib.melody"
            (t/is (noon.freeze/freeze
                    (play dur2
                          (chans [(patch :tinkle-bell) o1- (tup same [vel5 o1])
                                  (dup 8)]
                                 [(patch :woodblock) (r/gen-tup 12 5 :euclidean)
                                  (rep 8
                                       (probs {(m/permutation :rand) 1,
                                               (m/rotation :rand) 3}))])))))
          (t/testing "r/rotation"
            (t/testing "Example"
              (t/is (noon.freeze/freeze
                      (play (chans [(patch :tinkle-bell) o1- (dup 4)]
                                   [(patch :woodblock) (r/durtup 2 1 1 4)
                                    (lin _
                                         (r/rotation (/ 1 2))
                                         (r/rotation (/ 1 4))
                                         (r/rotation - (/ 1 4)))]))))
              (t/is (noon.freeze/freeze (play (chans [(patch :tinkle-bell) o1-]
                                                     [(patch :woodblock)
                                                      (r/durtup 2 1 1 4)
                                                      (r/rotation - (/ 1 5))])
                                              (dup 4))))
              (t/is (noon.freeze/freeze
                      (play dur2
                            (chans [(patch :tinkle-bell) o1-]
                                   [(patch :woodblock) (r/durtup 2 1 1 4)
                                    (r/rotation :relative - (/ 1 4))])
                            (dup 4))))
              (t/is (noon.freeze/freeze
                      (play dur2
                            (chans [(patch :tinkle-bell) o1-]
                                   [(patch :woodblock) (r/durtup 2 1 1 4)
                                    (r/rotation :rand-by (/ 1 2))])
                            (dup 4))))
              (t/is (noon.freeze/freeze (play dur2
                                              (chans [(patch :tinkle-bell) o1-]
                                                     [(patch :woodblock)
                                                      (r/durtup 2 1 1 4)
                                                      (r/rotation :rand-sub 4)])
                                              (dup 4))))))
          (t/testing "r/permutation"
            (t/testing "Example"
              (t/is (noon.freeze/freeze
                      (play (patch :woodblock) (r/durtup 2 1 1 4) (dup 4))))
              (t/is (noon.freeze/freeze (play (chans [(patch :tinkle-bell) o1-]
                                                     [(patch :woodblock)
                                                      (r/durtup 2 1 1 4)
                                                      (r/permutation 4)])
                                              (dup 4))))
              (t/is (noon.freeze/freeze (r/permutation 4 1)))
              (t/is (noon.freeze/freeze (r/permutation 4 -1)))
              (t/is (noon.freeze/freeze (r/permutation 8 [0 (/ 1 2)])))
              (t/is (noon.freeze/freeze (r/permutation 8 :rand)))
              (t/is (noon.freeze/freeze
                      (play
                        {:description "rythmic permutation demo"}
                        (chans
                          [(patch :taiko-drum) vel5 (dup 4)]
                          [(patch :woodblock) (r/durtup 2 1 1 (/ 1 2) (/ 1 2))
                           (each (maybe o1 o1-)) (nlin 4 (r/permutation 5))]
                          [(patch :electric-piano-1) o1- vel4 lydian
                           (par> d0 d3 d3 d3 d3)
                           (lin (root :C) (root :Eb) (root :Ab) (root :Db))])
                        (dup 4))))))))
      (t/testing "Harmony"
        (t/testing "Voicings"
          (t/is (noon.freeze/freeze
                  (play (patch :electric-piano-1) V tetrad (par s0 s1 s2 s3))))
          (t/is
            (noon.freeze/freeze
              (play (patch :electric-piano-1) V tetrad (par [o1 s0] s1 s2 s3))))
          (t/is
            (noon.freeze/freeze
              (play (patch :electric-piano-1) V tetrad (par s1 [o1 s2] s3 s4))))
          (t/testing "Inversions"
            (t/is (noon.freeze/freeze (play (patch :vibraphone)
                                            (par s0 s1 s2)
                                            (rep 4 (h/inversion 1)))))
            (t/is (noon.freeze/freeze (play (patch :vibraphone)
                                            o1
                                            (par s0 s1 s2)
                                            (rep 4 (h/inversion -2)))))
            (t/is (noon.freeze/freeze
                    (play (patch :vibraphone) (par s0 s1 s2) (rep 4 s1))))
            (t/is (noon.freeze/freeze
                    (play
                      {:description
                         "4 successive double inversions upward on a Cmaj79 "}
                      (patch :vibraphone)
                      o1-
                      (par d0 d2 d4 d6 d8)
                      (rep 4 (h/inversion 2))))))
          (t/testing "Drops"
            (t/is (noon.freeze/freeze (let [closed (par s0 s1 s2 s3)
                                            drop2 (par s0 [o1 s1] s2 s3)
                                            drop3 (par s0 s1 [o1 s2] s3)
                                            drop23 (par s0 [o1 s1] [o1 s2] s3)]
                                        (play (patch :vibraphone)
                                              tetrad
                                              (lin closed drop2 drop3 drop23)
                                              (each dur:2)))))
            (t/testing "drop"
              (t/testing "Examples"
                (t/is (noon.freeze/freeze (play (patch :vibraphone)
                                                tetrad
                                                (par s0 s1 s2 s3)
                                                (h/drop :rand))))
                (t/is (noon.freeze/freeze (play (patch :vibraphone)
                                                tetrad
                                                (par s0 s1 s2 s3)
                                                (h/drop 1))))
                (t/is (noon.freeze/freeze (play (patch :vibraphone)
                                                tetrad
                                                (par s0 s1 s2 s3)
                                                (h/drop -1))))
                (t/is (noon.freeze/freeze (play (patch :vibraphone)
                                                tetrad
                                                (par s0 s1 s2 s3)
                                                (h/drop [0 (/ 1 2)]))))))))
        (t/testing "Chord progressions"
          (t/testing "Voice leading"
            (t/is (noon.freeze/freeze (play (patch :electric-piano-1)
                                            (lin I VI IV V)
                                            (each (par s0 s1 s2))
                                            (dup 2))))
            (t/is (noon.freeze/freeze (play (patch :electric-piano-1)
                                            (lin I VI II V)
                                            (each [(par s0 s1 s2) (h/drop -1)])
                                            h/voice-led
                                            (dup 2))))
            (t/is (noon.freeze/freeze
                    (play (lin I VI II V)
                          (chans [(patch :acoustic-bass) C-2 (each t-round)]
                                 [(patch :electric-piano-1)
                                  (each (par s0 s1 s2)) h/voice-led])
                          (dup 2))))
            (t/is (noon.freeze/freeze
                    (play (structure :tetrad)
                          (lin I VI II V)
                          (chans [(patch :acoustic-bass) C-2
                                  (each [t-round (tup _ s2-)])]
                                 [(patch :electric-piano-1)
                                  (each [(par s0 s1 s2 s3) (h/inversion -3)
                                         (h/drop (/ 1 2))]) h/voice-led])
                          (dup 2)))))
          (t/testing "Melodies"
            (t/testing "align-contexts"
              (t/is (noon.freeze/freeze (play (patch :clarinet)
                                              (scale :harmonic-minor)
                                              (lin I IV VII I)
                                              (each (tup s0 s1 s2)))))
              (t/is (noon.freeze/freeze (play (patch :clarinet)
                                              (scale :harmonic-minor)
                                              (lin I IV VII I)
                                              (h/align-contexts :s)
                                              (each (tup s0 s1 s2)))))
              (t/is
                (noon.freeze/freeze
                  (play
                    dur2
                    (scale :harmonic-minor)
                    (lin I IV VII I)
                    (h/align-contexts :s)
                    (lin same (transpose c3) same)
                    (chans
                      [(patch :choir-aahs) vel4
                       (each [(par s0 s1 s2) (maybe (tup s0 s1-) (tup s0 s1))])]
                      [(patch :ocarina) vel6
                       (each [(shuftup s0 s1 s2)
                              (each (one-of
                                      (tup s0 (shuflin (one-of c1- s-) s+) s0)
                                      (tup s0 c1- s0 (one-of s2- s2))))])]
                      [(patch :acoustic-bass) vel3 o2-])))))
            (t/testing "harmonic-zip"
              (t/is (noon.freeze/freeze (play (patch :ocarina)
                                              (tup s0 s1 [s2 (lin d1 d1- _)] s1)
                                              (dupt 4)
                                              (adjust {:duration 4}))))
              (t/is (noon.freeze/freeze
                      (play (h/harmonic-zip
                              [(scale :harmonic-minor) (tup I IV VII I)
                               (h/align-contexts :s)]
                              [(patch :ocarina)
                               (tup s0 s1 [s2 (lin d1 d1- _)] s1) (dupt 4)])
                            (dup 2)
                            (adjust {:duration 6}))))
              (t/is (noon.freeze/freeze
                      (play (h/harmonic-zip
                              [(scale :harmonic-minor) (tup I IV VII I)
                               (h/align-contexts :s)]
                              (chans [(patch :ocarina)
                                      (tup s0 s1 [s2 (lin d1 d1- _)] s1)
                                      (dupt 4)]
                                     [(patch :acoustic-bass) t2-]
                                     [(patch :choir-aahs) vel4 (par s0 s2 s4)]))
                            (dup 2)
                            (adjust {:duration 12})))))))))
    (t/testing "Experiments"
      (t/is (noon.freeze/freeze
              (swap! out/options* assoc :tracks {0 :chorium})))
      (t/testing "Harmonic experiments"
        (t/testing "simple I IV VII I"
          (t/is (noon.freeze/freeze (play (scale :harmonic-minor)
                                          (lin I IV VII I)
                                          (h/align-contexts :s)
                                          (each (tup s0 s1 s2)))))
          (t/is (noon.freeze/freeze (play (scale :harmonic-minor)
                                          (lin I IV VII I)
                                          (h/align-contexts :s)
                                          (lin s0 s1 s2-)
                                          (each [(tup s0 s2)
                                                 (each (tup s0 c1- s+ s0))])
                                          (append rev))))
          (t/is
            (noon.freeze/freeze
              (play
                dur2
                (scale :harmonic-minor)
                (lin I IV VII I)
                (h/align-contexts :s)
                (lin same (transpose c3) same)
                (chans
                  [(patch :choir-aahs) vel4
                   (each [(par s0 s1 s2) (maybe (tup s0 s1-) (tup s0 s1))])]
                  [(patch :ocarina) vel6
                   (each [(shuftup s0 s1 s2)
                          (each (one-of (tup s0 (shuflin (one-of c1- s-) s+) s0)
                                        (tup s0 c1- s0 (one-of s2- s2))))])]
                  [(patch :kalimba) vel4 o2
                   (each [(shuftup s0 s1 s2)
                          (each
                            (one-of vel0 (par s0 s2-) (shuftup s0 s1 s2)))])]
                  [(patch :acoustic-bass) vel3 o2-])))))
        (t/testing "simple I IV I V"
          (t/is
            (noon.freeze/freeze
              (play dur2
                    (lin I IV I V)
                    (h/align-contexts :s)
                    (each (chans
                            [(patch :woodblock) C0 (dupt 4)]
                            [(patch :tinkle-bell) C0
                             (r/gen-tup 12 5 {:durations [1 2 3]})]
                            [(patch :marimba) o1- (r/gen-tup 12 5 :euclidean)
                             (each (par s0 s2)) (each (one-of s0 s1 s1-))]
                            [(patch :acoustic-bass) t2- vel10
                             (r/gen-tup 12 5 :euclidean :shifted)]
                            [vel12 (patch :music-box) o1 (one-of s0 s1 s1-)
                             (shuftup s0 s1 s3)
                             (each (probs {[(par s0 s2) (maybe (tup s0 s1))] 3,
                                           [(tup s3 s1 (par s2 s0) s1-)] 2,
                                           [(tup d1- s0 d1 s0)
                                            (maybe (m/rotation 2))]
                                             1}))]))
                    (dup 2)))))
        (t/testing "epic lydian"
          (t/is
            (noon.freeze/freeze
              (play {:description "epic lydian sequence by minor thirds"}
                    (h/harmonic-zip
                      [lydian sus47 (tup* (map root [:C :Eb :F# :A])) (dupt 2)
                       (h/align-contexts :s)]
                      (par [(chan 1) (patch :choir-aahs) vel3
                            (ntup 8 (par s0 s1 s2))]
                           [vel4
                            (let [s? (one-of s2- s1- s1 s2)]
                              (m/simple-tupline
                                (* 16 16)
                                (any-that (within-pitch-bounds? :C-1 :C2)
                                          (lin s? s?)
                                          [(shuflin s1 s2 s3 s4) (maybe rev)]
                                          (lin d1 d1- s0 s?)
                                          (lin d1- d1 s0 s?))))
                            (par [(chan 2) (patch :french-horn)]
                                 [(chan 3) vel5 o2 (patch :flute)])]
                           [(chan 4) (patch :taiko-drum) vel2
                            (ntup 16 (lin dur3 [o1 vel4 dur2] dur3))]
                           [(chan 5) (patch :acoustic-bass) o2- (ntup 32 t0)]))
                    (adjust 32)
                    (nlin 4 (s-shift -1))))))
        (t/testing "Tritonal experiment"
          (t/is
            (noon.freeze/freeze
              (play
                {:description "tritonal chord sequence shifts by minor thirds"}
                (let [I (one-of [lydian+ (structure [2 3 4 5 6])]
                                [melodic-minor (structure [1 2 4 5 6])])
                      V (one-of [V mixolydian (structure [1 3 4 5 6])]
                                [V phrygian6 (structure [0 1 3 5 6])])
                      [B G Eb] (map root [:B :G :Eb])]
                  [(tup [B V] [B I] [G V] [G I] [Eb V dur2] [Eb I dur2])
                   (rup 4 (transpose d2-)) (h/align-contexts :s :static)
                   (chans
                     [(patch :choir-aahs) vel3 (each (par s0 s1 s2 s3 s4))]
                     [(patch :vibraphone) vel5
                      (each (probs {(par s0 s1 s2 s3 s4) 1,
                                    (shuftup [dur2 (par s0 s2 s4)]
                                             [(one-of dur2 dur3)
                                              (par s1- s1 s3)])
                                      3}))]
                     [(patch :acoustic-bass) vel5
                      (each [tetrad o2- t0
                             (maybe (tup (one-of dur2 dur3) [dur2 o1-]))])]
                     [(patch :taiko-drum) vel3 (each (shuftup s0 s1 s2 s3 s4))
                      (each
                        (probs
                          {vel0 3, same 1, (one-of o1 o1-) 1, (tup t0 t1) 1}))]
                     [vel6
                      (h/grid-zipped
                        [(chans (patch :flute) [o1 (patch :piccolo)])
                         (ntup> (* 32 10)
                                (any-that (within-pitch-bounds? :C-2 :C2)
                                          s1
                                          s2
                                          s1-
                                          s2-
                                          s3
                                          s3-))]
                        (each (probs {vel0 1,
                                      same 4,
                                      (superpose (one-of s1 s2 s3)) 0})))])
                   (adjust 48)])))))
        (t/testing "Elliot smith chords"
          (t/is (noon.freeze/freeze (play dur2
                                          (lin [VI seventh]
                                               [IV add2]
                                               [I]
                                               [III seventh (inversion 2)]
                                               [VI seventh]
                                               [IV add2]
                                               (tup I [III seventh phrygian3])
                                               [IV])
                                          (h/align-contexts :d)
                                          (each (chans [(patch :acoustic-bass)
                                                        o1- t-round]
                                                       h/simple-chord)))))
          (t/is (noon.freeze/freeze
                  (play (chans [(patch :electric-piano-1)
                                (tup (shuftup s0 s1 s2 s3)
                                     (shuftup s2 s3 s4 s5))]
                               [(patch :acoustic-bass) o1- t-round])
                        (dupt 8)
                        (h/grid [(tup [VI seventh]
                                      [IV add2]
                                      [I]
                                      [III seventh (inversion 2)]
                                      [VI seventh]
                                      [IV add2]
                                      (tup I [III seventh phrygian3])
                                      [IV]) (h/align-contexts :d)])
                        (adjust 8)
                        (dup 2)))))
        (t/testing "Minor progression"
          (t/is
            (noon.freeze/freeze
              (play
                (lin [I melodic-minor]
                     [V phrygian3]
                     [V phrygian3]
                     [I melodic-minor]
                     [I phrygian3]
                     [IV dorian]
                     [II locrian]
                     [IIb lydianb7])
                (dup 2)
                (lin {:section :a} [{:section :b} (transpose c6)])
                (h/align-contexts :d)
                (parts
                  {:section :a}
                  (each (chans [(patch :vibraphone) (shuftup s0 s1 s2 s3 s4 s5)]
                               [(patch :flute) o1 (shuftup s0 s1 s2 s3 s4 s5)]
                               [(patch :acoustic-bass) o1- t-round]))
                  {:section :b}
                  (each (chans [(patch :choir-aahs) vel4 (par s0 s1 s2)]
                               [(patch :ocarina) vel4 s2- (shuftup s0 s2 s4)]
                               [(patch :music-box) vel6 o1
                                (shuftup s0 s1 s2 s3 s4 s5 s6 s7 s8)]
                               [(patch :acoustic-bass) o1- t-round])))
                (dup 2)))))
        (t/testing "I V"
          (t/is
            (noon.freeze/freeze
              (play dur3
                    (lin [I (scale :melm) (structure :tetrad)]
                         [V (scale :alt) (structure :sus47)])
                    (append s1-)
                    (append [(transpose c4-)
                             (parts (scale :melm)
                                    (scale :lydian)
                                    (scale :alt)
                                    [(scale :mixolydianb2)
                                     (structure [1 5 9 10])])])
                    (dup 2)
                    (h/align-contexts :s)
                    (let [below (one-of d1- s1-)
                          above (one-of d1 s1)
                          contours [[0 -1 1 0] [0 1 -1 0] [-1 0 1 0] [1 0 -1 0]
                                    [1 0 -1 0] [-1 0 1 0]]
                          passings (mapv (partial mapv {0 _, -1 below, 1 above})
                                     contours)
                          rand-passing (one-of* (map tup* passings))
                          below-step (one-of d1- d3- d4-)
                          above-step (one-of d1 d3 d4)
                          rand-line (rup 4 (one-of below-step above-step))
                          rand-vel
                            (fn [min max]
                              {:velocity
                                 (fn [_] (+ min (rand/rand-int (- max min))))})]
                      (each (chans
                              [(patch :choir-aahs) vel4 (par s0 s1 s2 s3)
                               (h/drop 1)]
                              [(patch :acoustic-bass) t-round o1-]
                              [(shuftup s0 s1 s2 s3)
                               (each (one-of rand-passing rand-line))
                               (chans
                                 [(patch :vibraphone) (each (rand-vel 40 70))
                                  (each (maybe vel0))]
                                 [(patch :flute) (each (rand-vel 60 80)) o1
                                  (each (maybe vel0
                                               [(chan inc) (patch :glockenspiel)
                                                vel4]))])])))))))
        (t/testing "Not too happy birthday"
          (t/is (noon.freeze/freeze
                  (play harmonic-minor
                        (lin I V VII I [IV melodic-minor VII] IV I VII)
                        (h/align-contexts :s)
                        (each (par (par s0 s1 s2) [o1 (shuftup s0 s1 s2)]))
                        (dup 4)))))
        (t/testing "I.m.M7 VI.alt bVI.7.#11 bII.7.sus4"
          (t/is
            (noon.freeze/freeze
              (play (lin [I melodic-minor]
                         [VI superlocrian]
                         [VIb lydianb7]
                         [IIb mixolydian])
                    (h/align-contexts :s)
                    (dup 2)
                    (each (chans
                            [(patch :vibraphone) vel6 t0 (par> d0 d3 d3 d3 d3)]
                            [(patch :acoustic-bass) vel6 t2-]
                            [(patch :taiko-drum)
                             (shuftup vel3 vel5 [vel4 (dupt 2)])]
                            [(ntup> 9
                                    (any-that (within-pitch-bounds? :G-1 :C2)
                                              d1-
                                              d1
                                              d3
                                              d3-
                                              d4
                                              d4-)) vel9
                             (chans (patch :flute)
                                    [o1- vel4 (patch :vibraphone)])]))
                    (lin _ c6)
                    (dup 2))))))
      (t/testing "Melodic experiments"
        (t/testing "Target notes"
          (t/is (noon.freeze/freeze (play aeolian (lin s0 s2 s1 s0))))
          (t/is
            (noon.freeze/freeze
              (def fill-diatonically
                "A very low level way to connect subsequent notes diatonically using `noon.harmony` directly.\n   It feels too complicated for such a simple thing..."
                (sf_
                  (let [sorted (sort-by :position _)
                        couples (partition 2 1 sorted)]
                    (->
                      (reduce
                        (fn [ret [a b]]
                          (let [va (events/pitch-value a)
                                vb (events/pitch-value b)
                                direction (if (> va vb) :down :up)
                                cnt (loop [cnt 0
                                           current (:pitch a)]
                                      (case direction
                                        :up (if (>= (hc/hc->chromatic-value
                                                      current)
                                                    vb)
                                              cnt
                                              (recur (inc cnt)
                                                     (hc/upd current
                                                             (hc/d-step 1))))
                                        :down (if (<= (hc/hc->chromatic-value
                                                        current)
                                                      vb)
                                                cnt
                                                (recur (inc cnt)
                                                       (hc/upd current
                                                               (hc/d-step
                                                                 -1))))))]
                            (score/concat-score ret
                                                (score/update-score
                                                  #{(assoc a :position 0)}
                                                  (rup cnt
                                                       (case direction
                                                         :up d1
                                                         :down d1-))))))
                        #{}
                        couples)
                      (conj (last sorted))))))))
          (t/is (noon.freeze/freeze
                  (play aeolian (lin s0 s2 s1 s0) fill-diatonically)))
          (t/is
            (noon.freeze/freeze
              (defn fill-line
                "This evolution of fill-diatonically let the user specify the harmonic layer.\n   It is still relying on `noon.harmony` which is not great."
                [layer]
                (sf_
                  (let [sorted (sort-by :position _)
                        couples (partition 2 1 sorted)]
                    (-> (reduce (fn [ret [a b]]
                                  (let [va (events/pitch-value a)
                                        vb (events/pitch-value b)
                                        direction (if (> va vb) :down :up)
                                        [check increment] (case direction
                                                            :up [>= 1]
                                                            :down [<= -1])
                                        cnt (loop [cnt 0
                                                   current (:pitch a)]
                                              (if (check (hc/hc->chromatic-value
                                                           current)
                                                         vb)
                                                cnt
                                                (recur (inc cnt)
                                                       (hc/upd current
                                                               (hc/layer-step
                                                                 layer
                                                                 increment)))))]
                                    (score/concat-score
                                      ret
                                      (score/update-score
                                        #{(assoc a :position 0)}
                                        (rup cnt
                                             (ef_ (update _
                                                          :pitch
                                                          (hc/layer-step
                                                            layer
                                                            increment))))))))
                                #{}
                                couples)
                        (conj (last sorted))))))))
          (t/is (noon.freeze/freeze
                  (play aeolian (lin s0 s2 s1 s0) (fill-line :c))))
          (t/is (noon.freeze/freeze (play dur:2
                                          harmonic-minor
                                          tetrad
                                          (patch :orchestral-harp)
                                          (lin s0 s2 s2- s4 s4- s2 s2- s5-)
                                          (lin _ [(transpose c6) s2 rev])
                                          (lin _ s2 s2-)
                                          (fill-line :s))))
          (t/is
            (noon.freeze/freeze
              (defn target
                [layer size direction duration]
                (sfn score
                     (->> score
                          (map (fn [e]
                                 (->> (range size)
                                      (map (fn [i]
                                             (-> (update e
                                                         :pitch
                                                         (hc/layer-step
                                                           layer
                                                           (case direction
                                                             :up (inc i)
                                                             :down (- (inc
                                                                        i)))))
                                                 (update :position
                                                         -
                                                         (* (inc i) duration))
                                                 (assoc :duration duration))))
                                      (into #{e}))))
                          (score/merge-scores))))))
          (t/is (noon.freeze/freeze (play (lin _
                                               [s2 (target :c 3 :up (/ 1 4))]
                                               [s1- (target :d 3 :down (/ 1 4))]
                                               [_ (target :c 3 :up (/ 1 4))])
                                          (out/options
                                            {:filename "test/trash/target"}))))
          (t/is
            (noon.freeze/freeze
              (defn connect
                [& sizes]
                (sf_
                  (let [sorted (sort-by :position _)]
                    (reduce
                      (fn [s [n1 n2]]
                        (let [hcs (loop [sizes sizes]
                                    (if-let [[s & sizes] (seq sizes)]
                                      (or (hc/simplest-connection s
                                                                  (:pitch n1)
                                                                  (:pitch n2))
                                          (recur sizes))))
                              duration (/ (:duration n1) (dec (count hcs)))]
                          (into s
                                (map-indexed (fn [idx pitch]
                                               (assoc n1
                                                 :pitch pitch
                                                 :position (+ (* idx duration)
                                                              (:position n1))
                                                 :duration duration))
                                             (butlast hcs)))))
                      #{(last sorted)}
                      (partition 2 1 sorted)))))))
          (t/is (noon.freeze/freeze
                  (play harmonic-minor
                        (lin I
                             [VI lydianb7]
                             V
                             IV
                             [II phrygian3]
                             [V aeolian]
                             [IIb lydian])
                        (h/align-contexts :s)
                        (m/$lin [(lin s0 s2 s2- s4) (maybe [rev s2])])
                        (lin _ s1 s1- _)
                        (chans [(patch :tango) (connect 5 3 2 1 0)]
                               [(patch :ocarina) vel6 s2 (connect 2 1 0)]
                               [(patch :acoustic-bass) o1- s2-
                                (connect 1 0)]))))
          (t/is (noon.freeze/freeze
                  (play harmonic-minor
                        (lin I
                             [VI lydianb7]
                             V
                             IV
                             [II phrygian3]
                             [V aeolian]
                             [IIb lydian])
                        (h/align-contexts :s)
                        (m/$lin [(lin s0 s2 s2- s4) (maybe [rev s2])])
                        (lin _ s1 s1- _)
                        (chans [(patch :tango) (m/connect 5 3 2 1 0)]
                               [(patch :ocarina) vel6 s2 (m/connect 2 1 0)]
                               [(patch :acoustic-bass) o1- s2-
                                (m/connect 1 0)])))))
        (t/testing "Passing notes"
          (t/testing "simple"
            (t/is (noon.freeze/freeze (play dorian
                                            (rep 4 s1)
                                            (each (tup c1- s2 s1 s0))
                                            (tup _ rev)
                                            (rep 4 (transpose c3))
                                            (append rev))))
            (t/is (noon.freeze/freeze (play dorian
                                            (rep 4 s1)
                                            (each (tup _ s2))
                                            (each (tup c1- d2 d1 d0)))))
            (t/is (noon.freeze/freeze
                    (play melodic-minor
                          dur4
                          (append (transpose c3) (transpose c6) (transpose c3))
                          (dup 2)
                          (each (shuftup s0 s1 s2 s3 s4))
                          (each (tup _ (one-of s1 s2 s1- s2- s3 s3-)))
                          (each (one-of (tup c1- d2 d1 d0)
                                        (tup c1- s1- s0 s2))))))
            (t/is
              (noon.freeze/freeze
                (play dur4
                      (append (transpose c3) (transpose c6) (transpose c3))
                      (each (one-of phrygian6 lydian melodic-minor))
                      (dup 2)
                      (each (chans [(patch :acoustic-bass) t2- (tup _ s2 s1- _)]
                                   [(patch :flute) vel8]
                                   [(patch :vibraphone) vel4
                                    (par s0 d4 d6 d8 d10 d12)]
                                   [(patch :taiko-drum)
                                    (r/gen-tup 10 4 :euclidean)
                                    (each [(one-of s0 s1 s1-)
                                           (one-of vel1 vel3 vel5)])]))
                      (parts (chan 1)
                             [(each (shuftup s0 s1 s2 s3 s4))
                              (each (tup _ (one-of s1 s2 s1- s2- s3 s3-)))
                              (each (one-of (tup c1- d2 d1 d0)
                                            (tup c1- s1- s0 s2)
                                            (tup c1- s1- s2- s0)))
                              (each (one-of vel5 vel6 vel7 vel9))]))))
            (t/is (noon.freeze/freeze
                    (play melodic-minor
                          (shuflin s0 s1 s2 s3)
                          (each (let [step (one-of s1 s2 s3 s1- s2- s3-)
                                      ap (lin c1- d1 s1-)]
                                  (tup [_ ap] [step ap] _ step)))
                          (append c2- c2-))))
            (t/is (noon.freeze/freeze
                    (play melodic-minor
                          (lin (shuflin s0 s1 s2 s3)
                               [{:passing true} (shuflin s0 s1 s2 s3)])
                          (each (let [step (one-of s1 s2 s3 s1- s2- s3-)
                                      ap (lin c1- d1)]
                                  (tup [_ ap] [step ap] _ step (par s2- s2))))
                          (append c4-)
                          (dup 2))))
            (t/is (noon.freeze/freeze
                    (play melodic-minor
                          dur:3
                          (shuflin s0 s2 s4)
                          (each (one-of (shuftup _ c1- d1) (shuftup _ d1 d1-)))
                          (m/permutation :rand)
                          (rep 3 (one-of (s-shift 1) (s-shift -1)))
                          (rep 3 (transpose c3))
                          (dup 2))))
            (t/is (noon.freeze/freeze
                    (play dorian+4
                          (lin I IV)
                          (m/$lin [(shuftup s0 s2 s4) (tup c1- _ d1)
                                   (m/permutation :rand)
                                   (rep 4 (one-of (s-shift 1) (s-shift -1)))])
                          (append (transpose c3))
                          (append (s-shift -1))))))
          (t/testing "intermediate"
            (t/is
              (noon.freeze/freeze
                (defn chromatic-double-passing
                  [side]
                  (sf_ (assert (= 1 (count _))
                               (str (quote org-utils/chromatic-double-passing)
                                    "works only on single note scores"))
                       (let [target (first _)
                             d-suroundings (hc/diatonic-suroundings (:pitch
                                                                      target))
                             c-space (get d-suroundings
                                          (case side
                                            :up 1
                                            :down 0))
                             step (case side
                                    :up 1
                                    :down -1)]
                         (score/update-score
                           _
                           (if (= c-space 2)
                             (tup (d-step step) (c-step step) same)
                             (tup (d-step step)
                                  (case side
                                    :up c1-
                                    :down d1)
                                  same))))))))
            (t/is (noon.freeze/freeze
                    (play dur4
                          (rup 6 (one-of d4 d3-))
                          (each (tup (chromatic-double-passing :down)
                                     [d6 (chromatic-double-passing :up)])))))
            (t/is (noon.freeze/freeze
                    (let [c-d+ (efn e
                                    (if-let [p- (get-in (hc/neibourhood (:pitch
                                                                          e))
                                                        [:down :c])]
                                      (assoc e :pitch p-)
                                      (d1 e)))]
                      (play dur:4 (rep 14 d1) (each (tup c-d+ _)))))))
          (t/testing "interleaving"
            (t/is
              (noon.freeze/freeze
                (defn interpose-with
                  [f]
                  (sf_ (if (m/line? _)
                         (set (mapcat (fn [[a b]] (if b ((f a b)) a))
                                (partition 2 1 nil (sort-by :position _)))))))))
            (t/is
              (noon.freeze/freeze
                (defn interleaved
                  [& xs]
                  (sf_
                    (let [scores (map (partial score/update-score _) xs)
                          counts (map count scores)
                          durations (map score/score-duration scores)]
                      (assert
                        (apply = counts)
                        "interleaved scores should have same number of elements")
                      (assert (apply = durations)
                              "interleaved scores should have same duration")
                      (assert (apply = (mapcat (partial map :duration) scores))
                              "interleaved scores should have even durations")
                      (let [duration (/ (first durations) (first counts))
                            shift (/ duration (count scores))]
                        (:score (reduce (fn [{:as state, :keys [at]} xs]
                                          (-> state
                                              (update :at + duration)
                                              (update :score
                                                      into
                                                      (map-indexed
                                                        (fn [i n]
                                                          (assoc n
                                                            :position
                                                              (+ at (* i shift))
                                                            :duration shift))
                                                        xs))))
                                  {:score #{}, :at 0}
                                  (apply map
                                    vector
                                    (map score/sort-score scores))))))))))
            (t/is (noon.freeze/freeze (play dur4
                                            (interleaved
                                              (rup 8 d1 :skip-first)
                                              (rup 8 d1- :skip-first)))))
            (t/is (noon.freeze/freeze
                    (let [up (one-of d1 s1)
                          down (one-of c1- d1- s1-)
                          rand-double-passing (one-of (tup up _ down _)
                                                      (tup down _ up _)
                                                      (tup down up down _)
                                                      (tup up down up _))]
                      (play harmonic-minor
                            dur4
                            (interleaved [(nlin 4 (shuftup s0 s1 s2 s3))
                                          (each rand-double-passing)]
                                         [(nlin 4 (shuftup s0 s1 s2 s3)) s2
                                          (each rand-double-passing)])))))
            (t/is (noon.freeze/freeze (defn interleaving
                                        [polarities a b]
                                        (loop [s []
                                               ps polarities
                                               a a
                                               b b]
                                          (if-let [[p & ps] (seq ps)]
                                            (let [[nxt a' b']
                                                    (case p
                                                      0 [(first a) (next a) b]
                                                      1 [(first b) a (next b)])]
                                              (recur (conj s nxt) ps a' b'))
                                            s)))))
            (t/is (noon.freeze/freeze
                    (defn rand-interleaving
                      ([a b]
                       (interleaving (rand/shuffle (concat (repeat (count a) 0)
                                                           (repeat (count b)
                                                                   1)))
                                     a
                                     b))
                      ([a b & xs]
                       (reduce rand-interleaving (rand-interleaving a b) xs)))))
            (t/is (noon.freeze/freeze (require (quote
                                                 [clojure.math.combinatorics :as
                                                  combinatorics]))))
            (t/is (noon.freeze/freeze
                    (defn interleavings
                      [a b]
                      (reduce (fn [ret perm] (conj ret (interleaving perm a b)))
                        []
                        (combinatorics/permutations (concat (repeat (count a) 0)
                                                            (repeat (count b)
                                                                    1)))))))
            (t/is
              (noon.freeze/freeze
                (u/defn* randomly-interleaved
                         "randomly interleave the result of the given updates"
                         [xs]
                         (sf_ (:score
                                (reduce (fn [state n]
                                          (-> state
                                              (update :score
                                                      conj
                                                      (assoc n
                                                        :position (:at state)))
                                              (update :at + (:duration n))))
                                  {:at 0, :score #{}}
                                  (apply rand-interleaving
                                    (map (fn [u]
                                           (sort-by :position
                                                    (score/update-score _ u)))
                                      xs))))))))
            (t/is (noon.freeze/freeze (defn n-firsts
                                        [n]
                                        (sf_ (->> (group-by :position _)
                                                  (sort)
                                                  (take n)
                                                  (map second)
                                                  (reduce into #{}))))))
            (t/is
              (noon.freeze/freeze
                (let [up (one-of d1 s1)
                      down (one-of c1- d1- s1-)
                      rand-double-passing (one-of (tup _ up down _)
                                                  (tup _ down up _)
                                                  (tup up _ down _)
                                                  (tup down _ up _)
                                                  (tup down up down _)
                                                  (tup up down up _))]
                  (play harmonic-minor
                        dur2
                        (randomly-interleaved
                          [(chan 1) (nlin 4 (shuftup s0 s1 s2 s3))
                           (each rand-double-passing)]
                          [(chan 2) (nlin 4 (shuftup s0 s1 s2 s3)) s4-
                           (each rand-double-passing)]
                          [(chan 3) (nlin 4 (shuftup s0 s1 s2 s3)) s4
                           (each rand-double-passing)]))))))
          (t/testing "experience 1" nil nil nil nil nil nil nil nil nil nil)
          (t/testing "polarity"
            (t/is (noon.freeze/freeze [0 0 1 0]))
            (t/is (noon.freeze/freeze [0 1 1 0]))
            (t/is (noon.freeze/freeze [0 0 1 0 1 0 0 1]))
            (t/is (noon.freeze/freeze
                    (play
                      phrygian
                      (lin I I VII I VII I VII VII)
                      (mixlin s0 s2)
                      (each (chans [(patch :acoustic-bass) o2- (maybe t-round)]
                                   [(patch :ocarina) s2 (shuftup s0 s2 s4)]))
                      (lin _ [rev (transpose c3-)])
                      (parts (chan 1)
                             (connect-with (one-of (one-of d1 d1-)
                                                   (shuflin (one-of s1 s1-)
                                                            (one-of d1 d1-))))
                             (chan 0)
                             (each (probs {(tup (one-of s1 s1-) _) 1, _ 4}))))))
            (t/is
              (noon.freeze/freeze
                (comment
                  (let [id identity
                        rev (fn [x] (mapv {0 1, 1 0} x))
                        _dup (fn [x] (vec (concat x x)))
                        cat (fn [& xs]
                              (fn [x] (vec (mapcat (fn [f] (f x)) xs))))
                        acc (fn [n f] (apply comp (repeat n f)))
                        each (fn [f] (fn [x] (vec (mapcat (comp f vector) x))))
                        _scan (fn [size step f]
                                (fn [x]
                                  (vec (mapcat f (partition size step x)))))
                        >> (fn [& xs]
                             (fn [x]
                               (reduce (fn* [p1__47303# p2__47302#]
                                         (p2__47302# p1__47303#))
                                 x
                                 xs)))
                        upd (fn [x f] (f x))]
                    (upd [1]
                         (>> (acc 3
                                  (cat id
                                       rev))
                             (each (cat id
                                        rev
                                        id)))))))))
          (t/testing "degree moves"
            (t/is (noon.freeze/freeze (play dorian
                                            (nlin> 8 s1)
                                            [(patch :ocarina)
                                             (connect-with (degree -1))])))
            (t/is (noon.freeze/freeze (play dorian
                                            dur4
                                            o1
                                            (lin _ (nlin> 3 s1-))
                                            [(patch :ocarina)
                                             (connect-with (degree 1))]
                                            (each (tup s0 s2))
                                            (connect-with (degree 1)))))
            (t/is
              (noon.freeze/freeze
                (let [pol+ {:polarity 0}
                      pol- {:polarity 1}
                      invert-pol (each {:polarity (fn [x]
                                                    (case x
                                                      0 1
                                                      1 0))})]
                  (play
                    lydianb7
                    dur2
                    (lin pol+ pol-)
                    (lin _ invert-pol)
                    (tup _ invert-pol)
                    (rep 4 (transpose c3-))
                    (h/align-contexts :s)
                    (dup 2)
                    (parts pol+ _ pol- (each (one-of (degree -1) (degree 1))))
                    (chans [(patch :ocarina)
                            (each [(one-of s0 s1) (shuftup s0 s1 s2 s3)])
                            (connect-with (one-of d1 d1-))]
                           [(patch :acoustic-bass) o1-
                            (each (one-of s0 s1- s2-))])))))
            (t/is
              (noon.freeze/freeze
                (let [pol+ {:polarity 0}
                      pol- {:polarity 1}
                      invert-pol (each {:polarity (fn [x]
                                                    (case x
                                                      0 1
                                                      1 0))})]
                  (play (chans [(patch :ocarina) s2- (ntup> 7 s1)
                                (shuftup [_ (connect-with d1)]
                                         [rev s1- (connect-with d1-)])
                                (dupt 16)]
                               [(patch :acoustic-bass) (dupt 64) o2- t-round
                                (each (maybe s2- s2))])
                        (h/grid [phrygian3 (tup pol+ pol-) (tup _ invert-pol)
                                 (tup _ invert-pol) (rup 4 (transpose c3-))
                                 (h/align-contexts :s) (dupt 2)
                                 (parts pol+ _ pol- (each (degree -1)))])
                        (adjust {:duration 64}))))))
          (t/testing "scanning"
            (t/is (noon.freeze/freeze (play (patch :electric-piano-1)
                                            aeolian
                                            (nlin> 6 s1)
                                            (each (tup _ c1- [s1 c1-] _)))))
            (t/is (noon.freeze/freeze
                    (play (patch :electric-piano-1)
                          dur2
                          aeolian
                          (nlin> 4 s1)
                          (each (tup _ [s2 c1-] c1- _ s2 [s1 d1])))))
            (t/is
              (noon.freeze/freeze
                (quote
                  (defn scan
                    {:doc
                       (str
                         "Chunk the score using the `by` function. "
                           "Chunks are partitioned by `size` and stepped by `step`. "
                         "`f` is applied to each chunks partition and should return a single score. "
                           "Resulting scores are merged together.")}
                    [by size step f]
                    (sf_ (->> (chunk-score _ by)
                              (partition size step)
                              (map f)
                              (score/merge-scores)))))))
            (t/is
              (noon.freeze/freeze
                (play (patch :electric-piano-1)
                      dur2
                      aeolian
                      (nlin> 4 s3)
                      (scan :position 2
                            1 (fn [[a b]]
                                (let [start (first a)
                                      {target-pitch :pitch} (first b)]
                                  (score/update-score
                                    #{start}
                                    (each (tup _
                                               [s2 c1-]
                                               c1-
                                               _
                                               s2
                                               [(ef_ (assoc _
                                                       :pitch target-pitch))
                                                d1])))))))))
            (t/is
              (noon.freeze/freeze
                (quote
                  (defn in-place
                    {:doc
                       (str
                         "Turn the given update `u` into an update that reposition received score to position zero before applying `u` to it. "
                           "The resulting score is then adjusted to its initial duration and shifted to its original position. "
                         "This is useful when you need to scan update a score. "
                           "It is similar to what the `noon.score/each` function is doing.")}
                    [u]
                    (sf_ (let [score-origin (score-origin _)
                               score-duration (- (score/score-duration _)
                                                 score-origin)]
                           (score/update-score
                             (score/shift-score _ (- score-origin))
                             [u
                              (adjust {:position score-origin,
                                       :duration score-duration})])))))))
            (t/is
              (noon.freeze/freeze
                (play (patch :electric-piano-1)
                      aeolian
                      (nlin> 8 [(degree 4) s1-])
                      (scan :position 2
                            1 (fn [[a b]]
                                (let [start (first a)
                                      {target-pitch :pitch} (first b)]
                                  (score/update-score
                                    #{start}
                                    (in-place (tup _
                                                   [s2 c1-]
                                                   c1-
                                                   _
                                                   s2
                                                   [(ef_ (assoc _
                                                           :pitch target-pitch))
                                                    d1])))))))))
            (t/is
              (noon.freeze/freeze
                (quote
                  (defn only-between
                    {:doc
                       (str
                         "Use `f` to update the subscore delimited by `beg` and `end` positions. "
                         "Leave other events unchanged.")}
                    [beg end f]
                    (par [(trim beg end) (in-place f)]
                         (trim nil beg)
                         (trim end nil))))))
            (t/is (noon.freeze/freeze (play (nlin> 8 d1)
                                            (only-between 4 6 o1))))
            (t/is
              (noon.freeze/freeze
                (quote
                  (defn scan>
                    {:doc
                       (str
                         "Accumulative scan. "
                         "Use `f` to accumulatively update time slices of given `size` of the score, stepping by `step`."),
                     :tags [:temporal :accumulative :iterative]}
                    ([size f] (scan> size size f))
                    ([size step f]
                     (sfn s
                          (reduce (fn [s from]
                                    (score/update-score
                                      s
                                      (only-between from (+ from size) f)))
                            s
                            (range 0 (score-duration s) step))))))))
            (t/is (noon.freeze/freeze (play (nlin> 8 d1)
                                            (scan> 4 3 (tup _ d1 d1-)))))))
        (t/testing "Canon"
          (t/is (noon.freeze/freeze (play (shuftup s0 s1 s2))))
          (t/is (noon.freeze/freeze (play (shuftup s0 s1 s2) (m/connect 1))))
          (t/is (noon.freeze/freeze (def decorate
                                      (sf_ (let [sorted (sort-by :position _)]
                                             (reduce (fn [s [n1 n2]]
                                                       (into s
                                                             (score/update-score
                                                               #{n1 n2}
                                                               (maybe (m/connect
                                                                        1)))))
                                               #{(last sorted)}
                                               (partition 2 1 sorted)))))))
          (t/is
            (noon.freeze/freeze
              (play dur2
                    (lin (shuftup s0 s1 s2 s3)
                         [(one-of s1 s1-) (shuftup s0 s1 s2 s3)])
                    decorate
                    (lin _ (s-shift 1) (s-shift -1) _)
                    (lin _ (s-shift 2))
                    (chans [(patch :ocarina) o1 (s-shift -1)]
                           [(sf_ (score/shift-score _ 2))]
                           [(patch :acoustic-bass) o2- (s-shift 1)
                            (sf_ (score/shift-score _ 5))])
                    (h/grid dur2
                            harmonic-minor
                            (lin I
                                 IV
                                 VII
                                 I
                                 [IV melodic-minor VII]
                                 IV
                                 [V harmonic-minor VII]
                                 VII)
                            (dup 4)
                            (h/align-contexts :s)))))))
      (t/testing "Concepts and techniques"
        (t/testing "Barry Harris"
          (t/is (noon.freeze/freeze (def barry-harris
                                      (scale [0 2 4 5 7 8 9 11]))))
          (t/is (noon.freeze/freeze
                  (play barry-harris (tup d0 d3 d4 d7) (tup d0 d2) (rep 4 d1))))
          (t/is (noon.freeze/freeze
                  (let [chord-tones [d0 d2 d4 d7]]
                    (play barry-harris
                          (lin d0 d3)
                          (rep 8 (one-of d1- d1))
                          (each [(chans
                                   [(patch :pad-1-new-age) o1- vel3
                                    (par* chord-tones)]
                                   [(patch :ocarina) vel4 (shuftup* chord-tones)
                                    (each (maybe (tup (one-of d1 d1-) d0)))]
                                   [(patch :vibraphone) vel5 o1
                                    (ntup 6
                                          [(one-of* chord-tones) (maybe o1)
                                           (maybe (tup d1- d0))])])
                                 (maybe rev)])))))
          (t/is (noon.freeze/freeze (def barry-harris2
                                      [barry-harris (structure [0 2 4 7])])))
          (t/is (noon.freeze/freeze
                  (play barry-harris2
                        (lin I VI VII IV)
                        (h/align-contexts :d)
                        (each (chans [(patch :brass) (par s0 s1 s2 s3)]
                                     [(patch :acoustic-bass) o1- t-round]
                                     [(patch :ethnic) o1
                                      (shuftup s0 s1 s2 s3 s4 s5 s6)]))
                        (rep 2 s1)
                        (append (transpose c3)))))
          (t/is (noon.freeze/freeze (play barry-harris2
                                          (lin IV I)
                                          (h/align-contexts :d)
                                          (each (par s0 s1 s2 s3))
                                          (rep 4 (transpose c3))
                                          h/voice-led))))
        (t/testing "Symetric modes"
          (t/is (noon.freeze/freeze (def symetric-modes
                                      {:messian3 (scale [0 2 3 4 6 7 8 10 11]),
                                       :messian4 (scale [0 1 2 5 6 7 8 11]),
                                       :messian5 (scale [0 1 5 6 7 11]),
                                       :messian7 (scale [0 1 2 3 5 6 7 8 9 11]),
                                       :half-augm (scale [0 1 4 5 8 9]),
                                       :messian6 (scale [0 2 4 5 6 8 10 11]),
                                       :whole (scale [0 2 4 6 8 10]),
                                       :augm-half (scale [0 3 4 7 8 11]),
                                       :half-whole (scale [0 1 3 4 6 7 9 10]),
                                       :whole-half (scale [0 2 3 5 6 8 9
                                                           11])})))
          (t/is (noon.freeze/freeze
                  (play (symetric-modes :augm-half)
                        (:two {:one (rup 8 (one-of d1 d1- d2 d2- d3 d3-)),
                               :two (shuftup d1 d2 d3 d4 d5 d6 d7)})
                        (patch :electric-piano-1)
                        (rep 32
                             (one-of (each d3)
                                     (each d3-)
                                     (m/rotation (/ 1 2))
                                     (m/permutation :rand {:grade 2})
                                     (m/contour :similar
                                                {:delta 0, :layer :d}))))))
          (t/is (noon.freeze/freeze
                  (defn rand-structure
                    [size]
                    (ef_ (let [degree-count (-> _
                                                :pitch
                                                :scale
                                                count)
                               degrees (first (take size
                                                    (pr/shuffle
                                                      (range degree-count))))]
                           ((structure (vec (sort degrees))) _))))))
          (t/is (noon.freeze/freeze
                  (def rand-degree
                    (ef_ (let [scale-size (-> _
                                              :pitch
                                              :scale
                                              count)
                               deg (rand/rand-nth (range 1 scale-size))]
                           ((degree (rand/rand-nth [(- deg) deg])) _))))))
          (t/is (noon.freeze/freeze
                  (defn rand-tup
                    [size]
                    (e->s event
                          (let [degree-count (-> event
                                                 :pitch
                                                 :scale
                                                 count)
                                degrees (first (take size
                                                     (pr/shuffle
                                                       (range degree-count))))]
                            (score/update-score #{event}
                                                (tup* (mapv d-step
                                                        degrees))))))))
          (t/is (noon.freeze/freeze
                  (play (symetric-modes :half-whole)
                        (rand-structure 3)
                        (rep 3 rand-degree)
                        (each (chans [vel4 h/simple-chord]
                                     [(patch :music-box) o1 (rand-tup 6)
                                      (each (one-of vel0 vel4 vel6 vel7))]))
                        (append [rev s2])
                        (append (transpose c5))
                        (append (between 0 (/ 1 3)))))))
        (t/testing "Arvo Part"
          (t/is
            (noon.freeze/freeze
              (let [m-line (fn [size]
                             (rand/rand-nth (vals {:up-to [(rep size d1-) rev],
                                                   :up-from (rep size d1),
                                                   :down-to [(rep size d1) rev],
                                                   :down-from (rep size d1-)})))
                    base (rand/shuffle (map vector
                                         [s0 s1 s2 (one-of s0 s1 s2)]
                                         (map m-line
                                           (rand/shuffle
                                             (rand/rand-nth
                                               (u/sums 12 4 [2 3 4 5]))))))]
                (play lydianb7
                      (lin* base)
                      (each (chans [(patch :piccolo) vel6 o1]
                                   [(patch :flute) vel3 o1 d5-]
                                   [(patch :accordion) vel4 d0]
                                   [(patch :choir-aahs) s-floor
                                    (vel-humanize 7 [40 80])]
                                   [(patch :choir-aahs) s-floor o1 s1
                                    (vel-humanize 7 [40 80])]
                                   [(patch :acoustic-bass) C-2 t-floor]))
                      m/connect-repetitions
                      (append [rev (transpose c3-)])
                      (append dorian)
                      (dup 2))))))
        (t/testing "Bartok harmony axis"
          (t/is (noon.freeze/freeze
                  (let [L- (transpose c5)
                        L+ (transpose c5-)
                        R- (transpose c3)
                        R+ (transpose c3-)
                        M (transpose c6)]
                    (play (rep 8
                               [(one-of L- L+) (maybe R- R+ M)
                                (one-of ionian aeolian)])
                          (h/align-contexts :d)
                          (chans [(patch :aahs) (each (par s0 s1 s2))]
                                 [(patch :ocarina) o1
                                  (each (shuftup s2- s1- s0 s1 s2 s3))]
                                 [(patch :acoustic-bass) o1- t-round
                                  (maybe s1 s1-)])
                          (lin _ s1 s1- _)))))
          (t/is
            (noon.freeze/freeze
              (let [L- (transpose c5)
                    L+ (transpose c5-)
                    R- (transpose c3)
                    R+ (transpose c3-)
                    M (transpose c6)
                    tup1 (mixtup s2- s1- s0 s1 s2 s3)
                    tup2 (mixtup s2- s1- s0 s1 s2 s3)]
                (play (rep 8
                           [(one-of L- L+) (maybe R- R+ M)
                            (one-of ionian aeolian) (maybe dur2 dur:2)])
                      (h/align-contexts :d)
                      (chans [(patch :aahs) (each [add2 (par s0 s1 s2 s3)])
                              m/connect-repetitions]
                             [(patch :ocarina) o1 add2
                              (each [(one-of tup1 tup2) (maybe rev)])]
                             [(patch :acoustic-bass) o1- t-round
                              (maybe s1 s1-)])
                      (lin _ s1 s1- _)))))
          (t/is
            (noon.freeze/freeze
              (let [L- (transpose c5)
                    _L+ (transpose c5-)
                    R- (transpose c3)
                    R+ (transpose c3-)
                    M (transpose c6)
                    base [(rand/rand-nth [R- R+ M])
                          (rand/rand-nth [ionian aeolian])]
                    rand-color [(maybe R- R+ M) (one-of ionian aeolian)]
                    tup1 (mixtup s2- s1- s0 s1 s2 s3)
                    tup2 (mixtup s2- s1- s0 s1 s2 s3)]
                (play base
                      (lin _ [L- rand-color] rand-color [L- rand-color] _)
                      (lin _ M rev)
                      (h/align-contexts :d)
                      (chans [(patch :aahs) (each [add2 (par s0 s1 s2 s3)])
                              m/connect-repetitions]
                             [(patch :ocarina) o1 add2
                              (each [(one-of tup1 tup2) (maybe rev)])]
                             [(patch :acoustic-bass) o1- t-round
                              (maybe s1 s1-)])
                      (lin _ s1 [rev s1-] _)))))
          (t/is
            (noon.freeze/freeze
              (let [initial [{:harmonic-coords [0 0]} melodic-minor sixth]
                    up [{:harmonic-coords (fn [[x y]] [x (mod (inc y) 3)])}
                        (transpose c5)]
                    down [{:harmonic-coords (fn [[x y]] [x (mod (dec y) 3)])}
                          (transpose c5-)]
                    left [{:harmonic-coords (fn [[x y]] [(mod (dec x) 4) y])}
                          (transpose c3)]
                    right [{:harmonic-coords (fn [[x y]] [(mod (inc x) 4) y])}
                           (transpose c3-)]]
                (play initial
                      (lin> _ up left down)
                      (lin _ up)
                      (lin _ [rev left])
                      (lin _ [right right])
                      (h/align-contexts :d)
                      (chans [(patch :aahs) (structure [1 2 5 6])
                              (each (par s0 s1 s2 s3))]
                             (let [tup1 (mixtup s2- s1- s0 s1 s2 s3)
                                   tup2 (mixtup s2- s1- s0 s1 s2 s3)]
                               [(patch :ocarina) o1 add2
                                (each [(one-of tup1 tup2) (maybe rev)])])
                             [(patch :acoustic-bass) o1- t-round
                              (maybe s1 s1- s2-)])
                      (lin _ s1 [up s1-] up)))))
          (t/is
            (noon.freeze/freeze
              (let [initial [lydian seventh]
                    up (transpose c5)
                    down (transpose c5-)
                    left (transpose c3)
                    right (transpose c3-)]
                (play [initial (lin> _ up left down)
                       (each (maybe (degree 2) (degree -2))) (lin _ up)
                       (lin _ [rev left]) (lin _ [right right])
                       (h/align-contexts :d)]
                      (chans [(patch :aahs) (each (par s0 s1 s2 s3))]
                             (let [tup1 [(structure [2 3 4 6])
                                         (mixtup s3- s2- s1- s0 s1 s2 s3 s4)]
                                   tup2 (mixtup d3- d2- d1- d0 d1 d2 d3 d4)]
                               [(patch :ocarina) o1
                                (each [(one-of tup1 tup2) (maybe rev)])])
                             [(patch :acoustic-bass) o2- t-round
                              (each (probs {_ 3,
                                            (one-of s1- s2) 3,
                                            (tup _ (one-of s1- s2)) 1,
                                            (tup (one-of s1- s2) _) 1}))])
                      (lin _ s1 [up s1-] up)
                      (out/options :bpm 40 :xml true))))))
        (t/testing "Simple counterpoint" nil nil nil nil nil nil nil nil))
      (t/testing "Tunes"
        (t/testing "Autumn leaves"
          (t/is
            (noon.freeze/freeze
              (play {:title "Autumn Leaves"}
                    vel3
                    [tetrad
                     (lin II
                          V
                          I
                          IV
                          VII
                          [III phrygian3]
                          [VI (lin [melodic-minor sixth] phrygian3)])
                     (h/align-contexts :s) (dup 2)]
                    (h/grid-zipped
                      (nlin 16
                            (chans [(patch :acoustic-bass) o1- t-round]
                                   [(patch :vibraphone) (par s0 s1 s2 s3)]
                                   [(patch :electric-piano-1) vel2 o2
                                    (par s0 s2 s4) (shuftup s0 s2)]
                                   [(patch :whistle) o1 vel5
                                    (each [(shuftup s0 s1 s2 s3)
                                           (tup
                                             same
                                             (one-of s1 s1- s2 s2-))])])))))))
        (t/testing "Giant steps (John Coltrane)"
          (t/is
            (noon.freeze/freeze
              (def GIANT_STEPS
                (let [II [II {:degree :II}]
                      V [V {:degree :V}]
                      I [I {:degree :I}]
                      t1 same
                      t2 (transpose c4-)
                      t3 (transpose c4)
                      s1 (lin [t1 I]
                              [t2 (lin V I)]
                              [t3 (lin V [dur2 I])]
                              [t2 (lin II V)])
                      II-V-I (lin II V [I dur2])]
                  [tetrad
                   (tup s1
                        [t2 s1]
                        [t3 I dur2]
                        [t2 II-V-I]
                        II-V-I
                        [t3 II-V-I]
                        [t1 (lin II V)])
                   (h/align-contexts :structural :static)]))))
          (t/is
            (noon.freeze/freeze
              (play vel3
                    (h/harmonic-zip
                      [GIANT_STEPS (dupt 2)]
                      (chans
                        [(patch :acoustic-bass) o2- (each t-round)]
                        [(patch :electric-piano-1) (each (par s0 s1 s2 s3))]
                        [(patch :ocarina) vel5
                         (each (parts {:degree :II}
                                      (structure [0 3 4 6])
                                      {:degree :V}
                                      (structure [1 2 5 6])
                                      {:degree :I}
                                      (structure :tetrad)))
                         (ntup 32
                               [(one-of o1 o2)
                                (! (rup (rand/rand-nth [5 6 7]) s1))
                                (tup (maybe (m/permutation (/ 1 4)))
                                     [(maybe rev) (one-of s1 s2 s2- s1-)])])]))
                    m/connect-repetitions
                    (adjust 32)))))
        (t/testing "ESP (Wayne Shorter)"
          (t/is
            (noon.freeze/freeze
              (play {:title "ESP", :composer "Wayne Shorter"}
                    (h/harmonic-zip
                      [tetrad
                       (tup [VII superlocrian dur2]
                            [I lydian dur2]
                            [VII superlocrian dur2]
                            [VIIb lydian dur2]
                            [VI superlocrian]
                            [VIIb lydian]
                            [VII superlocrian]
                            (tup [I lydian] [VIIb lydianb7])
                            [VI dorian]
                            [II lydianb7]
                            [II dorian]
                            [IIb lydianb7]) (h/align-contexts :s) (dupt 2)]
                      [vel4
                       (chans [(patch :acoustic-bass) o2- t-round]
                              [(patch :electric-piano-1) vel3 o1-
                               (par> d0 d3 d3 d3 d3)]
                              [(patch :flute) vel6
                               (fill> (/ 1 (* 2 32 6))
                                      (any-that (within-pitch-bounds? :C0 :C3)
                                                d4-
                                                d3-
                                                d1-
                                                d1
                                                d3
                                                d4))])])
                    (adjust 32)
                    (dup 2))))
          (t/is
            (noon.freeze/freeze
              (def ESP_fullgrid
                (let [common (lin [VII superlocrian dur2]
                                  [I lydian dur2]
                                  [VII superlocrian dur2]
                                  [VIIb lydian dur2]
                                  [VI superlocrian]
                                  [VIIb lydian]
                                  [VII superlocrian]
                                  (tup [I lydian] [VIIb lydianb7]))]
                  (tup
                    common
                    (lin [VI dorian] [II lydianb7] [II dorian] [IIb lydianb7])
                    common
                    (lin [VIb lydianb7]
                         [II dorian]
                         (tup [VIb dorian] [IIb lydianb7])
                         I))))))
          (t/is (noon.freeze/freeze
                  (play (h/harmonic-zip
                          [ESP_fullgrid (dupt 2) (h/align-contexts :s)]
                          (chans
                            [(patch :electric-piano-1) o1- vel3
                             (voices> d0 d3 d3 d3 d3)]
                            [(patch :acoustic-bass) vel2 C-2 t-round]
                            [(patch :flute)
                             (fill> (/ 1 (* 6 64))
                                    (maybe (any-that*
                                             (within-pitch-bounds? :G-1 :C2)
                                             [d4- d3- d1- d1 d3 d4])))
                             (each (probs {void 1, same 5}))
                             m/connect-repetitions (vel-humanize 10 [30 70])]))
                        (adjust 48)))))
        (t/testing "Cyclic episode (Sam Rivers)"
          (t/is (noon.freeze/freeze
                  (def CYCLIC_EPISODE
                    (let [a1 [dorian (rep 4 (transpose c3))]
                          a2 [dorian (rep 4 (transpose c3-))]
                          b (lin [IV dorian]
                                 [V superlocrian (structure [2 3 5 6])])
                          c (lin [V mixolydian sus47] [V phrygian sus27])
                          d [dorian (append (transpose c3))]]
                      [tetrad
                       (tup [(root :Bb) a1]
                            [(root :G) b]
                            [(root :D) b]
                            [(root :D) a2]
                            [(root :G) c]
                            [(root :Eb) d]) (dupt 4)
                       (h/align-contexts :s :static)]))))
          (t/is
            (noon.freeze/freeze
              (let [n-bars (* 4 16)
                    bass [(patch :acoustic-bass) (each t2-)]
                    vibe [(patch :vibraphone) vel5 t1 (each (par s0 s1 s2 s3))
                          h/voice-led]
                    _lead1 (ntup> (* n-bars 12)
                                  (any-that (within-pitch-bounds? :C0 :C3)
                                            d1
                                            d1-
                                            d3
                                            d3-
                                            d4
                                            d4-))
                    _lead2 [(repeat-while
                              (within-time-bounds? 0 (* n-bars 10))
                              (append [start-from-last
                                       (any-that (within-pitch-bounds? :C-1 :C2)
                                                 (rep 3 d3 :skip-first)
                                                 (rep 3 d3- :skip-first)
                                                 d1
                                                 d1-)])) (adjust 1)]
                    lead4 [(tup (mixtup s0 s1 s2 s3) (mixtup s2 s3 s4 s5))
                           (rup n-bars
                                (probs {(m/permutation [0 (/ 1 2)]) 2,
                                        (m/rotation :rand) 3,
                                        rev 1,
                                        (any-that* (within-pitch-bounds? :C0
                                                                         :C3)
                                                   (map s-step (range -2 3)))
                                          5}))]]
                (play CYCLIC_EPISODE
                      (chans bass
                             vibe
                             [(h/grid-zipped lead4)
                              (chans [(patch :flute) vel8 s2]
                                     [(patch :electric-piano-1) vel5])
                              (each (probs {vel0 1, same 2}))])
                      (vel-humanize 0.15)
                      (adjust 64))))))
        (t/testing "Inner urge (Joe Henderson)"
          (t/is (noon.freeze/freeze
                  (defn last-n-positions
                    "Limit the score to the n latest positions found."
                    [n]
                    (sf_ (let [_ (->> (group-by :position _)
                                      seq
                                      (sort-by key)
                                      reverse
                                      (take n)
                                      (map second)
                                      (reduce into #{}))]
                           (score/update-score _
                                               (start-from (score/score-origin
                                                             _))))))))
          (t/is
            (noon.freeze/freeze
              (let [n-bars 24
                    choir [(patch :choir-aahs) vel5 (par> d3 d3 d3)]
                    bass [(patch :acoustic-bass) C-2 t-round]
                    lead-line (any-that (within-pitch-bounds? :C0 :C3)
                                        (rep 2 d3 :skip-first)
                                        (rep 2 d3- :skip-first)
                                        d4
                                        d4-
                                        d1
                                        d1-
                                        (rep 3 d2 :skip-first)
                                        (rep 3 d2- :skip-first))]
                (play
                  (h/harmonic-zip
                    [(tup (lin (nlin 4 [(root :F#) locrian2])
                               (nlin 4 [(root :F) lydian])
                               (nlin 4 [(root :Eb) lydian])
                               (nlin 4 [(root :Db) lydian]))
                          [lydian
                           (lin* (map root [:E :Db :D :B :C :A :Bb :G]))])
                     (h/align-contexts :s) (dupt 4)]
                    (tup
                      (chans choir
                             bass
                             [(patch :music-box) vel5 C1
                              (m/simple-tupline (* n-bars 10) lead-line)])
                      (chans choir
                             bass
                             [(patch :ocarina) vel4 C1
                              (m/simple-tupline (* n-bars 24) lead-line)])
                      (chans choir
                             bass
                             [(patch :sawtooth) (dur (/ 1 n-bars)) vel4 C1
                              (tup d0 d3 d6) (tup d0 d4 d8)
                              (m/line (one-of (last-n-positions 10)
                                              (last-n-positions 7))
                                      (any-that (within-pitch-bounds? :C0 :C3)
                                                (m/permutation {:grade 3})
                                                (one-of d1 d1-)
                                                (one-of d2 d2-))
                                      (sf_ (> (score/score-duration _) 1))
                                      (trim 0 1)) (vel-humanize 5 [40 80])])
                      (chans [choir (ntup (/ n-bars 2) same)
                              ($by :position
                                   [(! (one-of
                                         (r/gen-tup 8 3 :euclidean)
                                         (r/gen-tup 8 3 :durations [2 3 4 5])))
                                    (sf_ (let [xs (-> (group-by :position _)
                                                      seq
                                                      sort
                                                      vals)]
                                           (reduce into
                                             #{}
                                             (map score/update-score
                                               xs
                                               (rand/shuffle [d0 d1 d1-])))))])]
                             bass)))
                  (adjust 180)))))))
      (t/testing "snippets"
        (t/testing "textures 1"
          (t/is (noon.freeze/freeze (play dur2
                                          lydian
                                          (patch :flute)
                                          (chans _ d3 d6 d9)
                                          (each [(dupt 24)
                                                 (each (one-of vel1 vel3 vel6)
                                                       (probs {_ 6, d1 1}))])
                                          ($by :channel (maybe rev))
                                          (append (transpose c3-))
                                          (append (transpose c1-)))))
          (t/is (noon.freeze/freeze
                  (play dur3
                        lydian
                        (chans [(patch :marimba) (lin _ c1)]
                               [(patch :vibraphone) (lin d3 d2)]
                               [(patch :celesta) (lin d6 d6)]
                               [(patch :orchestral-harp) (lin d9 d9)])
                        (append (transpose c2-))
                        (dup 2)
                        (each [(dupt 34)
                               (each (one-of vel0 vel3 vel6 vel9)
                                     (probs {_ 4, o1 1}))]))))
          (t/is (noon.freeze/freeze
                  (play dur8
                        o2
                        (dupt 128)
                        (each (par> d4 d4 d4)
                              (one-of vel0 vel1 vel2 vel3 vel4 vel5))))))
        (t/testing "Sparkling waves"
          (t/is (noon.freeze/freeze
                  (play dur:4
                        vel4
                        (scale :lydian)
                        (patch :music-box)
                        (par s0 s2 s4)
                        (rep 3
                             (each [{:mark (rand/rand)} s1
                                    {:velocity (div 1.1), :duration (mul 1.3)}
                                    (shuftup s2- s0 s2)])
                             :skip-first)
                        (lin I [rev III] [o1- V] [rev o1- VII])
                        (append [rev (transpose c3)])))))
        (t/testing "infinite climb"
          (t/is (noon.freeze/freeze
                  (play dur6
                        dur2
                        (patch :ocarina)
                        (rup 36 c1)
                        (sf_ (set (map-indexed
                                    (fn [i n]
                                      (let [vel (* 60 2 (/ (inc i) (count _)))
                                            vel (if (> vel 60)
                                                  (- 60 (- vel 60))
                                                  vel)]
                                        (assoc n :velocity vel)))
                                    (sort-by :position _))))
                        (par _ (m/rotation (/ 1 3)) (m/rotation (/ 2 3)))
                        (dup 4)))))
        (t/testing "violin fast arpegio"
          (t/is (noon.freeze/freeze
                  (play (dur (/ 3 2))
                        dorian
                        (patch :violin)
                        (lin I IV V I)
                        (h/align-contexts :s)
                        (each (ntup 2 (tup s0 s2 s4 s4 s2 s0)))
                        (each (! (vel (mul (+ 0.9 (* (rand/rand) 0.2))))))
                        (append s1-)))))
        (t/testing "zip rythmn"
          (t/is
            (noon.freeze/freeze
              (play lydianb7
                    (h/modal-structure 5)
                    (chans
                      [(patch :vibraphone) (shuflin s0 s1 s2 s3 s4)
                       (nlin 4 (one-of s1 s2 s1- s2-))
                       (sf_
                         (let [rythmn (mk (nlin 2 (! (r/gen-tup 12 5 :shifted)))
                                          (append rev))]
                           (set (map (fn [r n]
                                       (merge
                                         n
                                         (select-keys r [:position :duration])))
                                  (sort-by :position rythmn)
                                  (sort-by :position _)))))]
                      [(patch :woodblock) (r/gen-tup 12 5 :euclidean) (dup 4)]
                      [(patch :tinkle-bell) (dup 4)]
                      [(patch :metallic) (shuflin s0 s1 s2 s3)
                       (each (par s0 s1 s2))]
                      [(patch :acoustic-bass) t2- (dup 4)])
                    (adjust 8)
                    (append [(transpose c3-) s1 rev] _)))))
        (t/testing "Gradual melodic transformation"
          (t/is (noon.freeze/freeze
                  (play
                    (chans [(patch :vibraphone) vel3
                            (ntup 4
                                  [(one-of IV II VI) tetrad
                                   (par [t2- vel5] s0 s1 s2 s3)])]
                           [(patch :ocarina) vel5 (shuftup d1 d2 d3 d4 d5)
                            (each (maybe (par d0 d3)))
                            (rup 16
                                 (probs {(m/permutation :rand) 1,
                                         (m/rotation :rand) 3,
                                         (one-of* (map d-step (range -3 4)))
                                           5}))])
                    (adjust 10)
                    (append [d2- (transpose c3)] [d2 (transpose c3-)] same)))))
        (t/testing "Bach prelude Cm melodic pattern"
          (t/is (noon.freeze/freeze
                  (play
                    harmonic-minor
                    (m/$lin (lin I IV I V))
                    (h/align-contexts :s)
                    (lin _ s1)
                    (each
                      (chans
                        (tup s2 [s1 (lin _ d1- _)] s0 [s1 (lin _ d1- _)])
                        (tup s3- [s2- (lin _ d1 _)] s1- [s2- (lin _ d1 _)])))
                    (lin _ [(transpose c3) rev])
                    (dup 2))))
          (t/is (noon.freeze/freeze
                  (play
                    harmonic-minor
                    (m/$lin (lin I IV I V))
                    (h/align-contexts :s)
                    (lin _ s1)
                    (let [pat1 (tup s2 [s1 (lin _ d1- _)] s0 [s1 (lin _ d1- _)])
                          pat2 [pat1 (m/contour :mirror {:layer :s})]]
                      (each (chans [o1 pat1] [s1- pat2]))))))
          (t/is (noon.freeze/freeze
                  (play harmonic-minor
                        dur2
                        (lin _ (transpose c3) _)
                        (m/$lin (lin I IV I V))
                        (h/align-contexts :s)
                        (let [br (lin _ (one-of d1 d1-) _)
                              pat1 (one-of (tup s2 [s1 br] s0 [s1 br])
                                           (tup [s1 br] s2 [s1 br] s0)
                                           (tup s0 [s1 br] s2 [s1 br])
                                           (tup [s1 br] s0 [s1 br] s2))
                              pat2 (one-of (tup s3- [s2- br] s1- [s2- br])
                                           (tup s1- [s2- br] s3- [s2- br]))]
                          (each (chans [o1 (patch :ocarina) vel8 pat1]
                                       [(patch :vibraphone) pat2])))
                        (dup 2)))))
        (t/testing "Modal chords"
          (t/is
            (noon.freeze/freeze
              (let [rand-color (fn []
                                 (let [k (rand/rand-nth
                                           [:lydian+ :lydian :ionian :dorian
                                            :melodic-minor :mixolydian
                                            :phrygian6])]
                                   [(scale k) (h/modal-structure 4)]))]
                (play
                  dur2
                  (lin* (map (comp transpose c-step) (rand/shuffle (range 12))))
                  (each (! (rand-color)))
                  (h/align-contexts :d :static)
                  (chans [(patch :aahs) (each (par s0 s2 s3 s5))]
                         [(patch :vibraphone) o1
                          (each (par s0 s2 s3) (shuftup s0 s3) (tup s0 s1 s1-))
                          ($by :position
                               (probs {vel0 2,
                                       (one-of vel3 vel5 vel7) 8,
                                       [vel3 (ntup> 4 [s1 (vel+ 15)])] 1}))]
                         [(patch :acoustic-bass) o1- t-round])))))
          (t/is
            (noon.freeze/freeze
              (defn possible-modes
                "given a chromatic degree (int between 0 an 11)\n   return possible modes"
                [cd modal-lvl least-priority]
                (let [modes (constants/lvl->mode->degree-priority modal-lvl)
                      candidates (filter (fn [[_ s]]
                                           (-> (take least-priority s)
                                               (set)
                                               (contains? cd)))
                                   modes)]
                  candidates))))
          (t/is
            (noon.freeze/freeze
              (play
                (patch :aahs)
                dur4
                (shuflin c0 c1 c2 c3)
                (m/contour :similar {:delta 4, :layer :c})
                (par o1 [c6- (m/contour :mirror {:layer :c})])
                ($by :position
                     (sfn
                       score
                       (let [modal-lvl 1
                             chord-size 4
                             [min-pitch-val max-pitch-val] (h/pitch-values
                                                             score)
                             interval (mod (- max-pitch-val min-pitch-val) 12)
                             [mode-kw prio] (rand/rand-nth (possible-modes
                                                             interval
                                                             modal-lvl
                                                             (dec chord-size)))
                             partial-scale (cons 0 (take (dec chord-size) prio))
                             structure' (constants/partial-scale->structure
                                          mode-kw
                                          partial-scale)
                             closed (mk (dissoc (first score) :pitch)
                                        (origin min-pitch-val)
                                        (scale mode-kw)
                                        (structure structure')
                                        (par* (map s-step (range chord-size))))
                             drops (filter (fn [drop]
                                             (= max-pitch-val
                                                (last (h/pitch-values drop))))
                                     (h/drops closed))]
                         (rand/rand-nth drops))))
                ($by :position
                     (chans _
                            [(patch :contrabass) vel3 min-pitch o1-]
                            [max-pitch (patch :ocarina)
                             (mixtup s0 s1- s2- s3- s4- s5-) (tup _ s2- s1)]))
                (lin _ [rev c3])
                (lin _ [rev c3-])
                (out/options :bpm 30 :xml true :preview true)))))
        (t/testing "melodic development"
          (t/is
            (noon.freeze/freeze
              (play
                dorian
                (shuftup d0 d1 d2 d3 d4 d5 d6)
                (repeat-while
                  (within-time-bounds? 0 8)
                  (append (any-that (within-pitch-bounds? :C0 :C3)
                                    [(start-from-nth-last 1) (one-of d1- d1)]
                                    [(start-from-nth-last 8)
                                     (m/permutation [0 (/ 1 4)])]
                                    [(start-from-nth-last 4) rev]
                                    [(start-from-nth-last 4)
                                     (m/contour :similar
                                                {:extent [-2 2], :layer :d})]))
                  (trim 0 8))
                (each (probs {(one-of vel3 vel5 vel7 vel9) 6,
                              (superpose [(chan 2) (patch :vibraphone) vel8
                                          (one-of d3 d4)])
                                1,
                              (superpose [(chan 7) (patch :flute) vel8 o1]) 5}))
                (superpose
                  (k (nlin 4 [(chan 5) (patch :acoustic-bass) t2- vel8 dur2])))
                (rep 4
                     (one-of [(d-shift -2) (transpose c3)]
                             [(d-shift 2) (transpose c3-)]
                             [(d-shift 1) (transpose c1-)]
                             [(d-shift -3) (transpose c6)]))
                (append (superpose (k (nlin 4
                                            [(patch :taiko-drum) (chan 3)
                                             (! [vel4 (maybe o1- d1)
                                                 (r/gen-tup 7 3)])])
                                      (dup 8)))))))))
      (t/testing "Usage"
        (t/testing "Vienna symphonic library" nil nil)
        (t/testing "=noon.lib.rythmn/bintup="
          (t/is
            (noon.freeze/freeze
              (play
                dur6
                (lin [I dorian] [III mixolydian] [VIb lydian] [I lydian])
                (append> (transpose c1-) (transpose c1-) (transpose c1-))
                (dup 2)
                (h/align-contexts)
                (each
                  (chans
                    [(patch :new-age) vel3 o1-
                     (par s0 s1 s2 s3 [o1 (par> d3 d3 d3 d3)])]
                    [(patch :taiko-drum) (r/gen-tup 9 3 :durations [2 3 4])
                     (each (one-of vel4 vel3) (maybe d3 d3-))]
                    [(patch :acoustic-bass) t-floor o1-
                     (r/gen-bintup 9 4 :euclidean :shifted) vel4
                     (vel-humanize (/ 1 5))
                     (parts {:bintup 0}
                            (each (vel+ 20) (one-of s0 s1))
                            {:bintup 1}
                            (each (probs {vel0 2, (one-of d3- d4) 1})))]
                    [(r/gen-bintup 54 11 :shifted :euclidean)
                     (parts
                       {:bintup 0}
                       [(patch :electric-piano-1) sus4
                        (each vel3
                              (vel-humanize (/ 1 10))
                              (one-of d2 d4 d6)
                              (probs {_ 3,
                                      [(one-of s0 s1 s2) (par s0 s1 s2)] 1}))]
                       {:bintup 1}
                       [(patch :marimba) vel4 (vel-humanize (/ 1 5)) (chan+ 1)
                        (each [(one-of d3 d5 d7)
                               (maybe o1 (par _ d4))])])]))))))
        (t/testing "=noon.lib.harmony/grid="
          (t/is (noon.freeze/freeze
                  (play dur3
                        (nlin> 48 (one-of d1 d1-))
                        (each (chans [(patch :aahs) vel5 (par s0 s1 s2 s3)]
                                     [(patch :ocarina) (shuftup s0 s2 s4 s6)
                                      (shuftup d0 d3 d6) (tup _ rev)]
                                     [(patch :acoustic-bass) t2-]))
                        (h/grid dur3
                                tetrad
                                (lin [I lydian (structure [2 3 5 6])]
                                     [IIb dorian (structure [1 2 3 6])]
                                     [V mixolydian (structure [2 3 5 6])]
                                     [Vb melodic-minor (structure [1 2 5 6])])
                                (rep 6 (transpose c2-))
                                (dup 2)
                                (h/align-contexts :d :static)))))
          (t/is
            (noon.freeze/freeze
              (play (ntup> 24 (one-of d1 d1-))
                    (each (chans [(patch :aahs) vel5 (par s0 s1 s2 s3)]
                                 [(patch :ocarina)
                                  (one-of (mixtup s0 s2 s4 s6)
                                          (mixtup s0 s2 s4 s6))
                                  (one-of (mixtup d0 d3 d6) (mixtup d0 d3 d6))
                                  (vel-humanize 10 [40 80]) (tup _ rev)]
                                 [(patch :acoustic-bass) t2-]))
                    (h/grid tetrad
                            (tup [I lydian]
                                 [IIb dorian]
                                 [V mixolydian]
                                 [Vb melodic-minor])
                            (each (h/modal-structure 4))
                            (rup 4 (transpose c2-))
                            (dupt 2)
                            (h/align-contexts :d :static))
                    (adjust 60))))
          (t/is (noon.freeze/freeze
                  (play (chans [(patch :aahs) vel5 (par s0 s1 s2 s3)]
                               [(patch :acoustic-bass) t2-])
                        (h/grid (lin [I lydian (structure [2 3 5 6])]
                                     [IIb dorian (structure [1 2 3 6])]
                                     [V mixolydian (structure [2 3 5 6])]
                                     [Vb melodic-minor (structure [1 2 5 6])])
                                (rep 2 (transpose c2-))
                                (dup 2)
                                (h/align-contexts :d :static)
                                (adjust 1))
                        (parts (patch :acoustic-bass)
                               (each (tup (maybe o1) (one-of d4 d3-))))
                        (adjust 32))))
          (t/is
            (noon.freeze/freeze
              (play (chans [(patch :aahs) vel6
                            (rup 24
                                 (any-that (within-pitch-bounds? :G-1 :G1)
                                           s2
                                           s2-
                                           s3
                                           s3-)) (each (par s0 s1 s2 s3))]
                           [(patch :acoustic-bass) t2-])
                    (h/grid tetrad
                            (lin [I lydian (structure [2 3 5 6])]
                                 [IIb dorian (structure [1 2 3 6])]
                                 [V mixolydian (structure [2 3 5 6])]
                                 [Vb melodic-minor (structure [1 2 5 6])])
                            (rep 2 (transpose c2-))
                            (dup 2)
                            (h/align-contexts :d :static)
                            (adjust 1))
                    (parts (patch :acoustic-bass)
                           (each (tup (maybe o1) (one-of d4 d3-))))
                    (adjust 32))))
          (t/is
            (noon.freeze/freeze
              (play (rup 128
                         (any-that (within-pitch-bounds? :C1 :C3)
                                   s1
                                   s2
                                   s3
                                   s1-
                                   s2-
                                   s3-))
                    (chans
                      (each (probs {_ 2, vel0 1, (shuftup s1- s0 s1 s2) 1}))
                      (each s1- (probs {_ 2, vel0 1, (shuftup s1- s0 s1) 1}))
                      (each [s2- o1- (probs {_ 2, (shuftup s0 s2) 1})]))
                    (h/grid harmonic-minor
                            (tup I
                                 IV
                                 VII
                                 I
                                 [IV melodic-minor VII]
                                 IV
                                 [V harmonic-minor VII]
                                 VII)
                            (dupt 4)
                            (h/align-contexts :s))
                    (adjust {:duration 64})))))))))