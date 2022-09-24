(ns noon.lib.scratch
  (:use noon.score)
  (:require [noon.lib.harmony :as h]
            [noon.lib.melody :as m]
            [noon.lib.rythmn :as r]))

(comment

  [:examples
       (play vel2 dur2
             (cat* (map reroot [:C :Eb :F# :A]))
             ($ (tup (rebase (degree 4) (struct :sus47))
                     (rebase (scale :lydian+) (struct :tetrad))))
             (chans
              [($ (par s0 s1 s2 s3)) h/voice-led]
              ($ [vel4 o1 (tupn> 8 (one-of s1 s1-))])
              ($ [(repitch :C-1) tonic-round]))

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
            [(patch :marimba) o1- (r/euclidean-tup 5 12) ($ (par s0 s2)) ($ (one-of s0 s1 s1-))]
            [(patch :acoustic-bass) t2- vel10 (r/rand-euclidean-tup 5 12 true)]
            ;; ornementation
            [(patch :music-box) o1
             (one-of s0 s1 s1-)
             (shuftup s0 s1 s3)
             ($ (probs {[(par s0 s2) (maybe (tup s0 s1))] 3
                        [(tup s3 s1 (par s2 s0) s1-)] 2
                        [(tup d1- s0 d1 s0) (maybe (m/rotation 2))] 1}))]))
        ;; repeat one time
        (dup 2)))

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
                       [(patch :acoustic-bass) o2- tonic-round]
                       [(patch :electric-piano-1) (par s0 s1 s2 s3)]
                       [(patch :clarinet)
                        vel5 o1
                        (sub {:degree :II} (struct [0 3 4 6])
                             {:degree :V} (struct [1 2 5 6])
                             {:degree :I} (struct :tetrad))
                        (fill 1/2 (shuftup s0 s1 s2 s3 s4 s5))

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
                    [(patch :acoustic-bass) o2- ($ tonic-round)]
                    [(patch :electric-piano-1) ($ (par s0 s1 s2 s3))]
                    [(patch :ocarina)
                     vel5
                     ($ (sub {:degree :II} (struct [0 3 4 6])
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
                      tonic-round]

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

             (play ESP_fullgrid (dupt 2) (h/align-contexts :s)

                   (h/grid-zipped

                    (chans

                     [(patch :electric-piano-1) o1- vel3
                      (fill (/ 1 (* 2 64)) (voices> d0 d3 d3 d3 d3))]

                     [(patch :acoustic-bass) vel2 o2-
                      (fill (/ 1 (* 2 64)) tonic-round)] ;; HERE this do not result in playing tonics

                     [(patch :flute) vel6
                      (fill> (/ 1 (* 6 64))
                             (maybe
                              (any-that* (within-pitch-bounds? :C0 :C3)
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
                    (align-contexts :s)]

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
                   (catn 4 (interval 0 -1)))



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

             (play d:2
                   lydian+2
                   (cat C0 Eb0 F#0 A0)
                   (rep (si 1) 4)
                   ($ (tup si0 (si 1 1) si1 si2 (si 2 -1))))

             (play [lydian+2 sus47]
                   [dur:2 (cat C0 Eb0 F#0 A0)]
                   ($ (chans
                       (si 0)
                       (! (tup* (shuffle [si0 si1 si2 si3])))))
                   (rep (sub {:channel 0} (si -1)
                             {:channel 1} (si 1))
                        8)))

         (do :grid-zipped

             (play {:description "grid zipping demo"}
                   (patch :electric-piano-1)
                   ;; a simplistic grid
                   harmonic-minor
                   (tup* (map root [:C :Eb :Gb :A]))
                   (align-contexts :s)
                   ;; a melodic pattern
                   (h/grid-zipped
                    (tupn 4 (tup s0 s1 s2))
                    ($ (tup s0 [s2 (superpose o1)] s4)))
                   ;; adjust and repeat
                   (adjust 2)
                   (rep 4 ($ (interval 0 -1))))

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
                                     o1- tonic-round]

                                    [(patch :vibraphone)
                                     (par s0 s1 s2 s3)]

                                    [(patch :electric-piano-1) vel2
                                     o2 (par s0 s2 s4) (shuftup s0 s2)]

                                    [(patch :whistle) o1 vel5
                                     ($ [(shuftup s0 s1 s2 s3)
                                         (tup same (one-of s1 s1- s2 s2-))])]))))))
