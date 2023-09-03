(ns noon.lib.scratch
  (:use noon.score)
  (:require [noon.lib.harmony :as h]
            [noon.lib.melody :as m]
            [noon.lib.rythmn :as r]
            [noon.utils.misc :as u]
            [noon.midi :as midi]
            [clojure.math.combinatorics :as comb]))

(comment :motivation

         (play
          (scale :phrygian)
          (tup d0 d1 d2 d4)
          (append c4 c8)))









(comment

  [:examples
   (play vel2 dur2
         (cat* (map reroot [:C :Eb :F# :A]))
         ($ (tup (rebase (degree 4) (struct :sus47))
                 (rebase (scale :lydian+) (struct :tetrad))))
         (chans
          [($ (par s0 s1 s2 s3)) h/voice-led]
          ($ [vel4 o1 (tupn> 8 (one-of s1 s1-))])
          ($ [(repitch :C-1) t-round]))

         (rep 4 s1))]


  (play dur2
        (chans [(patch :woodblock) (dupt 4)]
               [(patch :tinkle-bell) (r/gen-tup 12 5 {:shifted true :durations [1 2 3]})])
        (dup 4))

  (play dur2
        ;; grid
        (cat I IV I V)
        (h/align-contexts :s)
        ;; on each chord
        ($ (chans
            ;; rythmn
            [(patch :woodblock) C0 (dupt 4)]
            [(patch :tinkle-bell) C0 (r/gen-tup 12 5 {:durations [1 2 3]})]
            ;; comping
            [(patch :marimba) o1- (r/gen-tup 12 5 :euclidean) ($ (par s0 s2)) ($ (one-of s0 s1 s1-))]
            [(patch :acoustic-bass) t2- vel10 (r/gen-tup 12 5 :euclidean :shifted)]
            ;; ornementation
            [(patch :music-box) o1
             (one-of s0 s1 s1-)
             (shuftup s0 s1 s3)
             ($ (probs {[(par s0 s2) (maybe (tup s0 s1))] 3
                        [(tup s3 s1 (par s2 s0) s1-)] 2
                        [(tup d1- s0 d1 s0) (maybe (m/rotation 2))] 1}))]))
        ;; repeat one time
        (dup 2)))

(comment ::m/gen-tup
         (play (scale :melodic-minor) (catn 8 (! (m/gen-tup {:size 10 :delta 2 :steps [-3 -1 1 3]}))))
         (play dur2
               (patch :electric-piano-1)
               (scale :harmonic-minor)
               (cat I VII I IV I [IIb lydian] V I)
               (h/align-contexts)
               (append (transpose c3) (transpose c1-) same)
               ($ (m/gen-tup {:layer :s :size 6 :delta 2 :steps [-2 -1 1 2]}))
               ($ (maybe (par s0 [vel3 s1] [vel3 s2])))
               ($ (superpose [(chan 2) o1 (patch :ocarina) (shuftup s1 s0 s1-) ($ (one-of vel1 vel3 vel7 vel9))]))
               ))

(comment :demos

         (do :utils

             (u/defclosure fill [dur f]
               (sf_ (let [sdur (score-duration _)
                          n (quot sdur dur)]
                      (assert (zero? (rem sdur dur))
                              "fill: division length should be a multiple of score length ")
                      (upd _ (tupn n f)))))

             (u/defclosure fill> [dur f]
               (sf_ (let [sdur (score-duration _)
                          n (quot sdur dur)]
                      (assert (zero? (rem sdur dur))
                              "fill: division length should be a multiple of score length ")
                      (upd _ (tupn> n f)))))

             (def connect-repetitions
               (sf_ (let [[e1 & todo] (sort-by :position _)]
                      (loop [[last-note & prev-notes :as ret] (list e1)
                             todo todo]
                        (if-let [[e & todo] (seq todo)]
                          (if (and (= (pitch-value e) (pitch-value last-note))
                                   (= (dissoc e :position :duration :pitch)
                                      (dissoc last-note :position :duration :pitch)))
                            (recur (cons (update last-note :duration + (:duration e))
                                         prev-notes)
                                   todo)
                            (recur (cons e ret) todo))
                          (set ret)))))))

         (do :roman-degrees-and-more

             (play (scale :harmonic-minor)
                   (cat I IV VII I)
                   (h/align-contexts :s)
                   ($ (tup s0 s1 s2)))

             (play dur:2
                   (scale :harmonic-minor)
                   (cat I IV VII I)
                   (h/align-contexts :s)
                   (cat s0 s1 s2-)
                   ($ [(tup s0 s2)
                       ($ (tup s0 c1- s+ s0))])
                   (cat same rev))

             (play dur2
                   (scale :harmonic-minor)
                   (cat I IV VII I)
                   (h/align-contexts :s)
                   #_(shufcat s0 s1 s2)
                   (cat same (transpose c3) same)

                   (chans

                    [(patch :choir-aahs) vel4
                     ($ [(par s0 s1 s2)
                         (maybe (tup s0 s1-) (tup s0 s1))])]

                    [(patch :ocarina) vel6
                     ($ [(shuftup s0 s1 s2)
                         ($ (one-of (tup s0 (shufcat (one-of c1- s-) s+) s0)
                                    (tup s0 c1- s0 (one-of s2- s2))))])]

                    [(patch :kalimba) vel4 o2
                     ($ [(shuftup s0 s1 s2)
                         ($ (one-of vel0 (par s0 s2-) (shuftup s0 s1 s2)))]
                        )]

                    [(patch :acoustic-bass) vel3
                     o2-])))

         (do :giant-steps

             (def GIANT_STEPS
               (let [II [II {:degree :II}]
                     V [V {:degree :V}]
                     I [I {:degree :I}]
                     t1 same
                     t2 (transpose c4-)
                     t3 (transpose c4)
                     s1 (cat [t1 I] [t2 (cat V I)] [t3 (cat V [dur2 I])] [t2 (cat II V)])
                     II-V-I (cat II V [I dur2])]
                 [tetrad
                  (tup s1
                       [t2 s1]
                       [t3 I dur2] [t2 II-V-I] II-V-I [t3 II-V-I] [t1 (cat II V)]
                       )
                  (h/align-contexts :structural :static)]))

             (qshow (mk GIANT_STEPS))

             (play {:notes "bug grid zip with tonic intervals"}
                   GIANT_STEPS
                   (h/grid-zipped
                    (patch :acoustic-bass)
                    (fill> 1/32 t0))
                   (adjust 16))

             (play {:duration 32}
                   GIANT_STEPS
                   (dupt 2)
                   vel3
                   ($ (chans
                       [(patch :acoustic-bass) o2- t-round]
                       [(patch :electric-piano-1) (par s0 s1 s2 s3)]
                       [(patch :clarinet)
                        vel5 o1
                        (parts {:degree :II} (struct [0 3 4 6])
                             {:degree :V} (struct [1 2 5 6])
                             {:degree :I} (struct :tetrad))
                        (fill> 1/2 (shuftup s0 s1 s2 s3 s4 s5))

                        #_(fill> 1/10 (any-that (within-pitch-bounds? :C-1 :C2)
                                                s3- s2- s1- s1 s2 s3))
                        ($ (probs {vel0 1
                                   same 4}))
                        ]))
                   ($by (juxt :voice :channel)
                        connect-repetitions))

             (play GIANT_STEPS
                   (dupt 2)
                   vel3
                   (chans
                    [(patch :acoustic-bass) o2- ($ t-round)]
                    [(patch :electric-piano-1) ($ (par s0 s1 s2 s3))]
                    [(patch :ocarina)
                     vel5
                     ($ (parts {:degree :II} (struct [0 3 4 6])
                             {:degree :V} (struct [1 2 5 6])
                             {:degree :I} (struct :tetrad)))
                     (h/grid-zipped
                      (tupn 32 [(one-of o1 o2)
                                (! (rup (rand-nth [5 6 7]) s1))
                                (tup (maybe shuffle-line)
                                     [(maybe rev) (one-of s1 s2 s2- s1-)])]))])
                   ($by (juxt :voice :channel)
                        connect-repetitions)
                   (adjust 32)))

         (do :esp

             (play

              {:title "ESP"
               :composer "Wayne Shorter"}

              ;; grid
              tetrad
              (cat [VII superlocrian dur2] [I lydian dur2]
                   [VII superlocrian dur2] [VIIb lydian dur2]
                   [VI superlocrian] [VIIb lydian] [VII superlocrian] (tup [I lydian] [VIIb lydianb7])
                   [VI dorian] [II lydianb7] [II dorian] [IIb lydianb7])
              (h/align-contexts :s)

              ;; parts
              vel4
              (chans [(patch :acoustic-bass) o2-
                      t-round]

                     [(patch :electric-piano-1) vel3 o1-
                      ($ (par> d0 d3 d3 d3 d3))]

                     [(patch :flute) vel6
                      ($ (fill> 1/8 (one-of d4- d3- d1- d1 d3 d4)))])

              ;; repeat
              (dup 2))

             (def ESP_fullgrid
               (let [common (cat [VII superlocrian dur2] [I lydian dur2]
                                 [VII superlocrian dur2] [VIIb lydian dur2]
                                 [VI superlocrian] [VIIb lydian] [VII superlocrian] (tup [I lydian] [VIIb lydianb7]))]
                 (tup common
                      (cat [VI dorian] [II lydianb7] [II dorian] [IIb lydianb7])
                      common
                      (cat [VIb lydianb7] [II dorian] (tup [VIb dorian] [IIb lydianb7]) I))))

             "There is a problem between tonic interval and grid zip"

             (play ESP_fullgrid
                   (dupt 2)
                   (h/align-contexts :s)

                   (h/grid-zipped

                    (chans

                     [(patch :electric-piano-1) o1- vel3
                      (fill (/ 1 (* 2 64)) (voices> d0 d3 d3 d3 d3))]

                     [(patch :acoustic-bass) vel2 o2-
                      (fill (/ 1 (* 2 64)) t-round)] ;; HERE this do not result in playing tonics

                     [(patch :flute) vel6
                      (fill> (/ 1 (* 6 64))
                             (maybe
                              (any-that* (within-pitch-bounds? :C-1 :C2)
                                         [d4- d3- d1- d1 d3 d4])))
                      ($ (probs {vel0 1
                                 same 5}))]))

                   ($by (juxt :voice :channel)
                        connect-repetitions)

                   (adjust 48)))

         (do :line-connection

             (do (u/defclosure catm [n f]
                   (rep n [(sf_ #{(assoc (last (sort-by :position _))
                                         :position 0)})
                           f]))

                 (u/defclosure line [len f]
                   (sf_ (let [nxt (upd _ (append [(sf_ #{(assoc (last (sort-by :position _))
                                                                :position 0)})
                                                  f]))]
                          (if (and (not-empty nxt) (<= (count nxt) len))
                            (recur nxt)
                            (set (take len (sort-by :position nxt)))))))

                 (u/defclosure tupline [len f]
                   (fit (line len f)))

                 (def s? (one-of s2- s1- s1 s2)))


             (play {:desc "catm demo"}
                   lydian+
                   dur8
                   (cat* (map root [:C :Eb :F# :A]))
                   ($ (fit [(catm 16
                                  (any-that (within-pitch-bounds? :C-2 :C2)
                                            (cat s0 c1- s0)
                                            (cat s0 d1 d1- s0)
                                            (cat d1 s0)
                                            (cat s0 d1- d1 s0 s1)
                                            s1
                                            s1-))])))

             (midi/stop2)

             (play {:description "epic lydian sequence by minor thirds"}

                   [lydian sus47
                    (tup* (map root [:C :Eb :F# :A]))
                    (dupt 2)
                    (h/align-contexts :s)]

                   (h/grid-zipped

                    (par [(chan 1) (patch :choir-aahs) vel3
                          (tupn 8 (par s0 s1 s2))]

                         [vel4
                          (tupline (* 16 16)
                                   (any-that (within-pitch-bounds? :C-1 :C2)
                                             (cat s? s?)
                                             [(shufcat s1 s2 s3 s4) (maybe rev)]
                                             (cat d1 d1- s0 s?)
                                             (cat d1- d1 s0 s?)))

                          #_connect-repetitions
                          (par [(chan 2) (patch :french-horn)]
                               [(chan 3) vel5 o2 (patch :flute)])]

                         [(chan 4) (patch :taiko-drum)
                          vel2 (tupn 16 (cat dur3 [o1 vel4 dur2] dur3))]

                         [(chan 5) (patch :acoustic-bass)
                          o2- (tupn 32 t0)]))

                   #_(sub {:channel 5} ($ tonic-round))

                   (adjust 32)
                   (catn 4 (s-shift -1)))



             (play {:description "tritonal chord sequence shifts by minor thirds"}

                   (let [I (one-of [lydian+ (struct [2 3 4 5 6])] [melodic-minor (struct [1 2 4 5 6])])
                         V (one-of [V mixolydian (struct [1 3 4 5 6])] [V phrygian6 (struct [0 1 3 5 6])])
                         [B G Eb] (map root [:B :G :Eb])]

                     [(tup [B V] [B I] [G V] [G I] [Eb V dur2] [Eb I dur2])
                      (rup 4 (transpose d2-))
                      (h/align-contexts :s :static)

                      (chans

                       [(patch :choir-aahs)
                        vel3
                        ($ (par s0 s1 s2 s3 s4))]

                       [(patch :vibraphone)
                        vel5
                        ($ (probs {(par s0 s1 s2 s3 s4) 1
                                   (shuftup [dur2 (par s0 s2 s4)] [(one-of dur2 dur3) (par s1- s1 s3)]) 3}))]

                       [(patch :acoustic-bass)
                        vel5
                        ($ [tetrad o2- t0 (maybe (tup (one-of dur2 dur3) [dur2 o1-]))])]

                       [(patch :taiko-drum)
                        vel3
                        ($ (shuftup s0 s1 s2 s3 s4))
                        ($ (probs {vel0 3 same 1 (one-of o1 o1-) 1 (tup t0 t1) 1}))]

                       [vel5
                        (h/grid-zipped
                         (chans (patch :flute) [o1 (patch :piccolo)])
                         (tupn> (* 32 10)
                                (any-that (within-pitch-bounds? :C-2 :C2)
                                          s1 s2 s1- s2- s3 s3-))
                         ($ (probs {vel0 1
                                    same 4
                                    (superpose (one-of s1 s2 s3)) 0})))])

                      (adjust 48)])))

         (do :tries

             (play dur:2
                   lydian+2
                   (cat C0 Eb0 F#0 A0)
                   (rep 4 s1)
                   ($ (tup s0 [s1 d1] s1 s2 [s2 d1-])))

             (play [lydian+2 sus47]
                   [dur:2 (cat C0 Eb0 F#0 A0)]
                   ($ (chans
                       s0
                       (shuftup s0 s1 s2 s3)))
                   (rep 8
                        (parts (chan 0) s1-
                               (chan 1) s1))))

         (do :grid-zipped

             (play {:description "grid zipping demo"}
                   (patch :electric-piano-1)
                   ;; a simplistic grid
                   harmonic-minor
                   (tup* (map root [:C :Eb :Gb :A]))
                   (h/align-contexts :s)
                   ;; a melodic pattern
                   (h/grid-zipped
                    (tupn 4 (tup s0 s1 s2))
                    ($ (tup s0 [s2 (superpose o1)] s4)))
                   ;; adjust and repeat
                   (adjust 2)
                   (rep 4 ($ c1-)))

             (play vel3
                   ;; grid
                   ;; 4 tonalities
                   (tup* (map root [:C :Eb :F# :A]))
                   ;; V I on each
                   ($ (tup [V sus47] [lydian+ tetrad]))
                   ;; align structurally
                   (h/align-contexts :s)

                   (h/grid-zipped
                    (tupn 4
                          (par
                           ;; simple arpegio
                           [o1 (tup (rup 6 s1) [s6 (rup 6 s1-)])]
                           ;; closed chord
                           [(par s0 s1 s2 s3) (tupn 2 (maybe (shuftup s1- s1)))])))

                   (adjust 8)))

         (do :autumn-leaves

             (play {:title "Autumn Leaves"}

                   vel3
                   [tetrad
                    (cat II V I IV VII [III phrygian3] [VI (cat [melodic-minor sixth] phrygian3)])
                    (h/align-contexts :s)
                    (dup 2)]

                   (h/grid-zipped
                    (catn 16 (chans [(patch :acoustic-bass)
                                     o1- t-round]

                                    [(patch :vibraphone)
                                     (par s0 s1 s2 s3)]

                                    [(patch :electric-piano-1) vel2
                                     o2 (par s0 s2 s4) (shuftup s0 s2)]

                                    [(patch :whistle) o1 vel5
                                     ($ [(shuftup s0 s1 s2 s3)
                                         (tup same (one-of s1 s1- s2 s2-))])]))))))

(comment
  (play)
  (play (cat [I melodic-minor] [VI superlocrian] [VIb lydianb7] [IIb mixolydian])
        (h/align-contexts :s)
        (dup 2)
        ($ (chans [(patch :vibraphone) vel6 t0 (par> d0 d3 d3 d3 d3)]
                  [(patch :acoustic-bass) vel6 t2-]
                  [(patch :taiko-drum) (tup vel3 vel5 vel0)]
                  [(tupn> 9 (any-that (within-pitch-bounds? :G-1 :C2)
                                       d1- d1 d3 d3- d4 d4-))
                   vel9
                   (chans (patch :flute)
                          [o1- vel6 (patch :vibraphone)])]))
        (cat _ c6)
        (dup 2)))

(comment :cyclic-episode

         (play vel0)

         (let [a1 [dorian (rep 4 (transpose c3))]
               a2 [dorian (rep 4 (transpose c3-))]
               b (cat [IV dorian] [V superlocrian (struct [2 3 5 6])])
               c (cat [V mixolydian sus47] [V phrygian sus27])
               d [dorian (append (transpose c3))]

               grid [tetrad
                     (tup [(root :Bb) a1]
                          [(root :G) b] [(root :D) b]
                          [(root :D) a2]
                          [(root :G) c] [(root :Eb) d])
                     (dupt 4)
                     (h/align-contexts :s :static)]

               n-bars (* 4 16)

               bass [(patch :acoustic-bass) ($ t2-)]
               vibe [(patch :vibraphone) vel6 t1 ($ (par s0 s1 s2 s3)) h/voice-led]

               ;; alternate leads

               lead1 (tupn> (* n-bars 12)
                            (any-that (within-pitch-bounds? :C0 :C3)
                                      d1 d1- d3 d3- d4 d4-))

               lead2 [(while (within-time-bounds? 0 (* n-bars 10))
                        (append [start-from-last
                                 (any-that (within-pitch-bounds? :C-1 :C2)
                                           (rep 3 d3 :skip-first)
                                           (rep 3 d3- :skip-first)
                                           d1 d1-)]))
                      (adjust 1)]

               lead4 [#_(tup s0 s1 s2 s3 s4 s5 s6)
                      (shuftup d0 d1 d2 d3 d4 d5 d6 d7 d8)

                      (rup n-bars
                           (probs {(m/permutation [0 1/2]) 2
                                   (m/rotation :rand) 3
                                   rev 1
                                   (any-that* (within-pitch-bounds? :C0 :C3)
                                              (map d-step (range -3 4))) 5
                                   }))

                      ]
               ]

           (play grid
                 (chans bass
                        vibe
                        [(h/grid-zipped lead4)
                         (chans [(patch :flute) vel8 o1 d1]
                                [(patch :electric-piano-1) vel5])
                         ($ (probs {vel0 1
                                    same 5}))])
                 (adjust 64))))

(comment :melodic-development

         (defn take-lst [n]
           (sf_ (let [sorted (sort-by :position _)]
                  (if (>= (count sorted) n)
                    (let [taken (take-last n sorted)]
                      (set (upd (set taken) {:position (sub (:position (first taken)))})))))))

         (stop)
         (play
          dorian
          dur:7
          (while (within-time-bounds? 0 8)
            (append
             (any-that (within-pitch-bounds? :C0 :C3)
                       [(take-lst 1) (one-of d1- d1)]
                       [(take-lst 8) (m/permutation [0 1/4])]
                       [(take-lst 4) rev]
                       [(take-lst 4) (m/contour :similar {:extent [-2 2] :layer :d})]))
            (trim 0 8))
          ($
           (probs {(one-of vel3 vel5 vel7 vel9) 6
                   (superpose [(chan 2) (patch :vibraphone) vel8 (one-of d3 d4)]) 1
                   (superpose [(chan 7) (patch :flute) vel8 o1]) 5
                   ;vel0 1
                   }))
          (superpose (k (catn 4 [(chan 5) (patch :acoustic-bass) t2- vel8 dur2])))

          #_ ($ (d-shift 2))
          (rep 4 (one-of [(d-shift -2) (transpose c3)]
                         [(d-shift 2) (transpose c3-)]
                         [(d-shift 1) (transpose c1-)]
                         [(d-shift -3) (transpose c6)]))
          (append (superpose (k (catn 4 [(patch :taiko-drum) (chan 3) (! [vel4 (maybe o1- d1) (r/gen-tup 7 3)])])
                                (dup 8))))
          (start-from 16)
          )


         (play (patch :taiko-drum)
               (rep 4 o1))

         (stop)



         )





(comment :passings

         (defn lowest-layer [n]
           (let [p (get-in n [:pitch :position])]
             (cond
               (:c p) :c
               (:d p) :d
               (:s p) :s
               (:t p) :t)))

         (defn neibourhood [n]
           (let [s (hash-set n)]
             {:up {:c (upd s c1)
                   :d (upd s d1)
                   :s (upd s s1)
                   :t (upd s t1)}
              :down {:c (upd s c1-)
                     :d (upd s d1-)
                     :s (upd s s1-)
                     :t (upd s t1-)}}))

         (play dorian
               (rep 4 s1)
               ($ (tup c1- s2 s1 s0))
               (tup _ rev)
               (rep 4 (transpose c3))
               (append rev))

         (play dorian
               (rep 4 s1)
               ($ (tup _ s2))
               ($ (tup c1- d2 d1 d0))
               )

         (play melodic-minor
               dur4
               (append (transpose c3) (transpose c6) (transpose c3))
               (dup 2)
               ($ (shuftup s0 s1 s2 s3 s4))
               ($ (tup _ (one-of s1 s2 s1- s2- s3 s3-)))
               ($ (one-of (tup c1- d2 d1 d0)
                          (tup c1- s1- s0 s2)))
               )

         (stop)
         (play dur4
               (append (transpose c3) (transpose c6) (transpose c3))
               ($ (one-of phrygian6 lydian melodic-minor))
               (dup 2)
               ($ (chans [(patch :acoustic-bass) t2- (tup _ s2 s1- _)]
                         [(patch :flute) vel8]
                         [(patch :vibraphone) vel4 (par s0 d4 d6 d8 d10 d12)]
                         [(patch :taiko-drum)
                          (r/gen-tup 10 4 :euclidean)
                          ($ [(one-of s0 s1 s1-) (one-of vel1 vel3 vel5)])]))
               (parts (chan 1)
                      [($ (shuftup s0 s1 s2 s3 s4))
                       ($ (tup _ (one-of s1 s2 s1- s2- s3 s3-)))
                       ($ (one-of (tup c1- d2 d1 d0)
                                  (tup c1- s1- s0 s2)
                                  (tup c1- s1- s2- s0)))
                       ($ (one-of vel5 vel6 vel7 vel9))])

               )

         (play melodic-minor
               (shufcat s0 s1 s2 s3)
               ($ (let [step (one-of s1 s2 s3 s1- s2- s3-)
                        ap (cat c1- d1 s1-)]
                    (tup [_ ap] [step ap] _ step)))
               (append c2- c2-))

         (play melodic-minor
               (cat (shufcat s0 s1 s2 s3)
                    [{:passing true} (shufcat s0 s1 s2 s3)]
                    )
               ($ (let [step (one-of s1 s2 s3 s1- s2- s3-)
                        ap (cat c1- d1)]
                    (tup [_ ap] [step ap] _ step (par s2- s2))))
               (append c4-)
               (dup 2))

         (play melodic-minor
               dur:3
               (shufcat s0 s2 s4)
               ($ (one-of (shuftup _ c1- d1)
                          (shuftup _ d1 d1-)))
               shuffle-line
               (rep 3 (one-of (s-shift 1) (s-shift -1)))
               (rep 3 (transpose c3))
               (dup 2))

         (play hungarian

               (cat I IV)
               ($cat [(shuftup s0 s2 s4)
                      (one-of (tup c1- _ d1) (tup d1- _ d1))
                      shuffle-line
                      (rep 4 (one-of (s-shift 1) (s-shift -1)))])
               (append (transpose c3))
               (append (s-shift -1)))

         (require '[noon.harmony :as nh])
         (defn chromatic-double-passing [side]
           (sf_
            (assert (= 1 (count _))
                    (str `chromatic-double-passing
                         "works only on single note scores"))
            (let [target (first _)
                  d-suroundings (nh/diatonic-suroundings (:pitch target))
                  c-space (get d-suroundings (case side :up 1 :down 0))
                  step (case side :up 1 :down -1)]
              (upd _
                   (if (= c-space 2)
                     (tup (d-step step) (c-step step) _)
                     (tup (d-step step) (case side :up c1- :down d1) _))))))

         (play dur4
               (rup 6 (one-of d4 d3-))
               ($ (tup (chromatic-double-passing :down) [d6 (chromatic-double-passing :up)])))

         (def diatonic?
           (sf_ (if (every? (comp nh/diatonic? nh/normalise :pitch)
                            _)
                  _)))

         (mk c2
             diatonic?)

         (nh/neibourhood (:pitch (first (mk d1-))))

         (play harmonic-minor
               dur2
               (cat I IV V I)
               (append (transpose c3) (transpose c6))
               (h/align-contexts :s)
               ($ (chans [(patch :string-ensemble-1) vel4 (par s2- s0 s2)]
                         [(patch :ocarina) (shuftup s0 s1 s2) ($ (tup c1- [s2- (cat d1 _)] d1 _ s1 s2))])))

         (let [c-d+ (efn e (if-let [p- (get-in (nh/neibourhood (:pitch e)) [:down :c])]
                             (assoc e :pitch p-)
                             (d1 e)))]
           (play dur:4
                 (rep 14 d1)
                 ($ (tup c-d+ _))))

         (defn interpose-with [f]
           (sf_ (if (line? _)
                  (set (mapcat (fn [[a b]] (if b ((f a b)) a))
                               (partition 2 1 nil (sort-by :position _)))))))

         (defn interleaved [& xs]
           (sf_ (let [scores (map (partial upd _) xs)
                      counts (map count scores)
                      durations (map score-duration scores)]
                  (assert (apply = counts)
                          "interleaved scores should have same number of elements")
                  (assert (apply = durations)
                          "interleaved scores should have same duration")
                  (assert (apply = (mapcat (partial map :duration) scores))
                          "interleaved scores should have even durations")
                  (let [duration (/ (first durations) (first counts))
                        shift (/ duration (count scores))]
                    (:score
                     (reduce (fn [{:as state :keys [at]} xs]
                               (-> state
                                   (update :at + duration)
                                   (update :score into (map-indexed (fn [i n] (assoc n :position (+ at (* i shift)) :duration shift)) xs))))
                             {:score #{} :at 0}
                             (apply map vector (map sort-score scores))))))))

         (play dur4
               (interleaved
                (rup 8 d1 :skip-first)
                (rup 8 d1- :skip-first)))

         (let [up (one-of d1 s1)
               down (one-of c1- d1- s1-)
               rand-double-passing
               (one-of (tup up _ down _)
                       (tup down _ up _)
                       (tup down up down _)
                       (tup up down up _))]
           (play harmonic-minor
                 dur4
                 (interleaved
                  [(catn 4 (shuftup s0 s1 s2 s3)) ($ rand-double-passing)]
                  [(catn 4 (shuftup s0 s1 s2 s3)) s2 ($ rand-double-passing)])))

         (defn interleaving [polarities a b]
           (loop [s [] ps polarities a a b b]
             (if-let [[p & ps] (seq ps)]
               (let [[nxt a' b'] (case p 0 [(first a) (next a) b] 1 [(first b) a (next b)])]
                 (recur (conj s nxt) ps a' b'))
               s)))

         (defn rand-interleaving
           ([a b]
            (interleaving (shuffle (concat (repeat (count a) 0) (repeat (count b) 1)))
                          a b))
           ([a b & xs]
            (reduce rand-interleaving
                    (rand-interleaving a b)
                    xs)))

         (defn interleavings [a b]
           (reduce (fn [ret perm]
                     (conj ret (interleaving perm a b)))
                   []
                   (comb/permutations (concat (repeat (count a) 0) (repeat (count b) 1)))))

         (defclosure* randomly-interleaved
           "randomly interleave the result of the given updates"
           [xs]
           (sf_ (:score
                 (reduce (fn [state n]
                           (-> state
                               (update :score conj (assoc n :position (:at state)))
                               (update :at + (:duration n))))
                         {:at 0 :score #{}}
                         (apply rand-interleaving (map (fn [u] (sort-by :position (upd _ u))) xs))))))

         (defn n-firsts [n]
           (sf_ (->> (group-by :position _)
                     (sort)
                     (take n)
                     (map second)
                     (reduce into #{}))))

         (let [up (one-of d1 s1)
               down (one-of c1- d1- s1-)
               rand-double-passing
               (one-of (tup _ up down _)
                       (tup _ down up _)
                       (tup up _ down _)
                       (tup down _ up _)
                       (tup down up down _)
                       (tup up down up _))]
           (play harmonic-minor
                 dur2
                 (randomly-interleaved
                  [(chan 1) (catn 4 (shuftup s0 s1 s2 s3)) ($ rand-double-passing)]
                  [(chan 2) (catn 4 (shuftup s0 s1 s2 s3)) s4- ($ rand-double-passing)]
                  [(chan 3) (catn 4 (shuftup s0 s1 s2 s3)) s4 ($ rand-double-passing)])))

         )
