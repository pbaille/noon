(ns noon.tries.scratch
  (:use noon.updates)
  (:require [noon.lib.harmony :as h]
            [noon.lib.melody :as m]
            [noon.lib.rythmn :as r]
            [noon.score :as score :refer [score]]
            [noon.output :refer [play noon]]
            [noon.utils.pseudo-random :as pr]
            [noon.harmony :as nh]))

(comment

  (swap! options* assoc :tracks {0 :chorium})

  (spit "simple-tup.edn"
        (str (midifiable-score (score (tup s0 s1 s2)))))

  (play (tup s0 s1))
  (play vel2 dur2
        (lin* (map reroot [:C :Eb :F# :A]))
        (each (tup (rebase V (structure :sus47))
                   (rebase (scale :lydian) (structure [0 2 3 6]))))
        (chans
         [(each (par s0 s1 s2 s3)) h/voice-led]
         (each [vel0 o1 (ntup> 8 (one-of d1 d1- d3 d3-))])
         (each [(repitch :C-1) t-round]))

        (rep 4 s1)))

(comment ::m/gen-tup
         (play (scale :melodic-minor)
               (nlin 8 (! (m/gen-tup {:layer :d :length 10 :delta 2 :steps [-3 -1 1 3]}))))
         (noon {:play true
                :tracks {0 :chorium}
                :filename "./test/data/scratch/1"}
               (score dur4
                   (patch :electric-piano-1)
                   (scale :harmonic-minor)
                   (lin I VII I IV I [IIb lydian] V I)
                   (h/align-contexts)
                   (append (transpose c3) (transpose c1-) same)
                   (each (m/gen-tup {:layer :s :length 6 :delta 2 :steps [-2 -1 1 2]}))
                   (each (maybe (par s0 [vel3 s1] [vel3 s2])))
                   (each (superpose [(chan 2) o1 (patch :ocarina)
                                     (m/gen-tup {:layer :s :length 4 :delta 1 :steps [-1 1 -2 2]})
                                     (maybe rev)
                                     (vel-humanize 1/2 [0.1 0.5])])))))

(comment :demos

         (do :roman-degrees-and-more

             (swap! options* assoc :tracks {0 :chorium}))

         (do :tries

             (play dur:2
                   lydian+2
                   (lin C0 Eb0 F#0 A0)
                   (rep 4 s1)
                   (each (tup s0 [s1 d1] s1 s2 [s2 d1-])))

             (play [lydian+2 sus47]
                   [dur:2 (lin C0 Eb0 F#0 A0)]
                   (each (chans
                          s0
                          (shuftup s0 s1 s2 s3)))
                   (rep 8
                        (parts (chan 0) s1-
                               (chan 1) s1))))

         (do :harmonic-zipping

             (play {:description "grid zipping demo 2"}
                   ;; a melodic pattern
                   [(patch :electric-piano-1)
                    (ntup 4 (tup s0 s1 s2))
                    (each (tup s0 [s2 (superpose o1)] s4))]
                   ;; simplistic grid
                   (h/grid harmonic-minor
                           (tup* (map root [:C :Eb :Gb :A]))
                           (h/align-contexts :s))
                   ;; adjust and repeat
                   (adjust 2)
                   (rep 4 (each c1-)))

             (play vel3
                   (h/harmonic-zip
                    ;; grid
                    [;; 4 tonalities
                     (tup* (map root [:C :Eb :F# :A]))
                     ;; V I on each
                     (each (tup [V sus47] [lydian+ tetrad]))
                     ;; align structurally
                     (h/align-contexts :s)]

                    ;; content
                    (ntup 4
                          (par
                           ;; simple arpegio
                           [o1 (tup (rup 6 s1) [s6 (rup 6 s1-)])]
                           ;; closed chord
                           [(par s0 s1 s2 s3) (shuftup s1- s0 s1)])))

                   (adjust 8))))

(comment :elements
         (let [r {:a (r/gen-tup 8 5)
                  :b (r/gen-tup 8 3)
                  :c (r/gen-tup 12 5)}
               m {:a (m/gen-tup :d 4 3)}
               h {:a [(scale :melm) (structure :tetrad) (lin I [IV dorian] [V (scale :alt) (structure :sus47)] I)]}]
           (play dur2
                 (h :a)
                 (h/align-contexts :d)
                 (dup 2)
                 (append (transpose c3-))
                 (each (chans [(patch :taiko-drum) o1-]
                              [(patch :woodblock) vel3 C2 (r :c) (maybe (m/rotation :rand))]
                              [(patch :acoustic-bas) o1- t-round]
                              [(patch :vibraphone) vel3 h/simple-chord]
                              [(patch :flute) (m :a) (mixtup s0 s2 s4)])))))

(comment :garzone-triads

         "George Garzone has this interesting way of building lines"
         "https://youtu.be/dTIwWFa2Rnw?si=lhQUZKaa3SaL6O9R&t=2594"
         "The idea is to mix all triads that shares at least one note with the chord you are playing on"
         "Let's start with an hardcoded example on Cmajor"
         (let [maj [ionian triad]
               min [eolian triad]
               triads [[(root :C) maj]
                       [(root :E) maj]
                       [(root :C) min]
                       [(root :G) maj]
                       [(root :B) maj]
                       [(root :F) maj]
                       [(root :D) maj]
                       [(root :Eb) maj]

                       [(root :E) min]
                       [(root :G) min]
                       [(root :F) min]
                       [(root :B) min]
                       [(root :D) min]
                       [(root :Ab) maj]
                       [(root :A) min]
                       [(root :C#) min]]]
           (play
            (chans [(patch :aahs) (par s0 s1 s2)]
                   [(ntup> (* (count triads) 3)
                           (any-that (within-pitch-bounds? :C-1 :C1)
                                     s1 s1-))
                    (h/grid [(tup* (shuffle triads))
                             (h/align-contexts :s)])])
            (adjust 8)))

         "We got a taste of it but it is not really satisfying for"
         "Garzone is more precise regarding to how subsequent triads could be connected."
         "the last note of the triad you play can go either 1 semiton up or down"
         "from this note pick another triad but changing the inversion used"

         [:aborted '(let [main-triad [(root :C) ionian triad]
                          main-pitch-classes (set (map pitch-class-value (mk main-triad (par s0 s1 s2))))
                          all-triads (for [root' [:C :Db :D :Eb :E :F :Gb :G :Ab :A :Bb :B]
                                           kind [ionian eolian lydian+ superlocrian]]
                                       [(root root') kind])

                          triad->pitch-class-values (reduce (fn [ret t]
                                                              (assoc ret t (mapv pitch-class-value (mk t (par s0 s1 s2)))))
                                                            {} all-triads)

                          all-transitions (reduce (fn [ret [t vs]]
                                                    (assoc ret t (reduce (fn [ret [t' vs']]
                                                                           (update ret (- 6 (count (into #{} (concat vs vs'))) conj t')))
                                                                         {} triad->pitch-class-values)))
                                                  {} triad->pitch-class-values)

                          available-triads (filter (fn [u]
                                                     (some main-pitch-classes
                                                           (map pitch-class-value (mk u (par s0 s1 s2)))))
                                                   all-triads)

                          transitions (reduce (fn [ret triad]
                                                (assoc ret triad (filter (fn [t]) available-triads)))
                                              {} available-triads)
                          triad-line (loop [ret [] current main-triad triads available-triads]
                                       (if (seq triads)
                                         (let [])))]
                      (play (lin* (shuffle available-triads))
                            (h/align-contexts :s)
                            (each (tup s0 s1 s2))))]

         "We will no longer contrained available triads to ones that contains one note of the main triad."
         "We will only pick a random note of the current triad, move it 1 semitone up or down and pick another that contain this note."

         (let [all-triads (for [root' [:C :Db :D :Eb :E :F :Gb :G :Ab :A :Bb :B]
                                kind [ionian eolian lydian+ superlocrian]]
                            [(root root') kind])

               triad->pitch-class-values (reduce (fn [ret t]
                                                   (assoc ret t (mapv pitch-class-value (score t (par s0 s1 s2)))))
                                                 {} all-triads)

               _all-transitions (reduce (fn [ret [t vs]]
                                          (assoc ret t (reduce (fn [ret [t' vs']]
                                                                 (update ret (- 6 (count (set (concat vs vs')))) conj t'))
                                                               {1 [] 2 [] 3 []} triad->pitch-class-values)))
                                        {} triad->pitch-class-values)

               pitch-class-value->triads (reduce (fn [ret [t vs]] (reduce #(update %1 %2 conj t) ret vs))
                                                 {} triad->pitch-class-values)
               triads (loop [current 0 ret []]
                        (if (> (count ret) 32)
                          ret
                          (let [picked (rand-nth (pitch-class-value->triads current))
                                pitch-class-values (triad->pitch-class-values picked)
                                inv (rand-nth [0 1 2])]
                            (recur (-> (get pitch-class-values inv)
                                       ((rand-nth [inc dec]))
                                       (mod 12))
                                   (conj ret [picked inv])))))]
           (noon {:play true
                  :midi true
                  :filename "test/data/garzone1"}
                 (score (lin* (map (fn [[triad inv]] [dur:2
                                                   triad
                                                   (chans [(patch :acoustic-bass) o1- t-round]
                                                          [(patch :electric-piano-1)
                                                           ({0 (tup s1 s2 s0)
                                                             1 (tup s0 s2 s1)
                                                             2 (tup s0 s1 s2)} inv)
                                                           (par _
                                                                [o1 rev])])])
                                triads))
                     (lin _ s1 s1- _))))

         (let [all-triads (for [root' [:C :Db :D :Eb :E :F :Gb :G :Ab :A :Bb :B]
                                kind [ionian eolian #_lydian+ #_superlocrian]]
                            [(root root') kind])

               triad->pitch-class-values (reduce (fn [ret t]
                                                   (assoc ret t (mapv pitch-class-value (score t (par s0 s1 s2)))))
                                                 {} all-triads)

               pitch-class-value->triads (reduce (fn [ret [t vs]] (reduce #(update %1 %2 conj t) ret vs))
                                                 {} triad->pitch-class-values)
               triads (loop [triad (pr/rand-nth all-triads)
                             inver (pr/rand-nth [0 1 2])
                             ret []]
                        (if (> (count ret) 4)
                          ret
                          (let [pitch-class-values (triad->pitch-class-values triad)
                                connection (get pitch-class-values inver)
                                candidates (-> ((pr/rand-nth [inc dec]) connection)
                                               (mod 12)
                                               (pitch-class-value->triads))
                                next-triad (pr/rand-nth (remove (partial = triad) candidates))
                                next-inversion (pr/rand-nth (remove (partial = inver) [0 1 2]))]
                            (recur next-triad
                                   next-inversion
                                   (conj ret [triad (inversion inver)])))))]
           (play
            (lin* (map (fn [triad]
                         [dur4
                          triad
                          (chans [(patch :acoustic-bass) o1- t-round]
                                 [(patch :electric-piano-1)
                                  (par s0 s1 s2)
                                  #_(par _
                                         [o1 rev])])])
                       triads)))))

(comment "simple melodic development"

         "I would like to list/sort the most basic transformations that can be applied to a simple melodic fragment."
         "In order to create some really simple pieces relying on one or two simple motives"

         "transposition"

         (play (tup s0 s1 s2)
               (lin _ vel0
                    s1 vel0
                    s1- vel0))

         "reversion"

         (play (tup s0 s1 s2)
               (lin _ vel0 rev))

         "mirror"

         (play (tup s0 s2 s1 s2)
               (lin _ vel0 (m/contour :mirror)))

         "mixing"

         (play dur:2
               (tup s0 s1 s2)
               (append s1)
               (append [s1 rev])
               (append (degree -1) rev [(degree 3) s1-]))

         "dodecaphonic"

         (let [serie (mapv c-step (pr/shuffle (range 12)))]
           (play (chans [(patch :ocarina)
                         (lin* serie)
                         (tup _ rev (m/contour :mirror) [(m/contour :mirror) rev])]
                        [(patch :choir-aahs) (lin* serie) o1-])
                 (lin* serie))))

(comment :harmonic-modifiers

         (def secondary-dominant
           (ef_ (let [{:as ctx :keys [scale]} (:pitch _)]
                  (if (and (= 7 (count scale))
                           (<= 6 (nth scale 4) 8))
                    (assoc _ :pitch (nh/upd ctx
                                            (nh/rescale (assoc (vec scale) 3 5 6 11 ;4 7
                                                               ))
                                            (nh/redegree 4)))))))

         (noon {:play true
                :midi true
                :filename "test/trash/secondary-dominants"
                :tracks {0 :chorium}}
               (score (structure :tetrad)
                   (rep 6 (degree 3))
                   (each (tup secondary-dominant same))
                   (h/align-contexts :d)
                   (each (chans (par s0 s1 s2 s3)
                                [C-2 t-round]))
                   (adjust 16)
                   (dup 2)))

         )

(comment
  (play (h/upd :altered.s2367)
        (par s0 s1 s2 s3))

  (play (h/lin :Imelm.s2367 :Valt.s2367)
        (par s0 s1 s2 s3)
        (dup 2))

  (play (h/upd :s2367)
        (h/lin :Imelm :Valt)
        #_(h/align-contexts :s)
        (par s0 s1 s2 s3)
        (dup 2))

  (play (h/upd :s2367)
        (par s0 s1 s2 s3)
        (dup 2))

  (play (h/lin :IM7 :VIIm7b5 :III7 :VI7 :IIm7 :II7 :V7sus4 :V7b9omit1)
        (chans (each (par s0 s1 s2 s3))
               [(patch :ocarina) o1
                (each [(mixtup s0 s1 s2 s3)
                       (shuftup s0 (one-of s3- s2- s1- s1 s2 s3))])])
        (dup 2))

  (play (h/tup :IM7 :V/III7 :IIIm7 :bIIIM7 :V/V7sus4 :bVI7#11omit5 :V7sus4 :bIIM7)
        (chans (each [(par s0 s2 s3) (mixtup s0 s1 s2)])
               [(patch :acoustic-bass) C-2 t-round])
        (dup 2)
        (adjust 16))

  (play (h/lin :I [:II :V7] :V/V7 [vel1 :V7] :I)
        (each (tup s0 s1 s2 s3)))

  (score (h/upd [vel1 :I]))

  (->> (score (h/upd :V7b9omit1) (lin s0 s1 s2 s3 s4))
       (sort-by :position)
       (map pitch-value))

  )

(comment :fractal-harmony-xp

         (defn fract [n f g]
           (let [pol-seq (interleave (repeat f) (repeat g))]
             (if (odd? n)
               (tup* (take n pol-seq))
               (tup* (concat (take (- n 2) pol-seq) (list f f))))))

         (play (scale :harmonic-minor)
               {:pol 1}
               (fract 3 _ {:pol (mul -1)})
               (each (fract 4 _ {:pol (mul -1)}))
               (parts {:pol 1} _
                      {:pol -1} (degree -1))
               (each [(one-of s0 s1 s2) (shuftup s0 s2)])
               (tup s0 s2 s1)
               (adjust 12))

         (play (scale :eolian)
               (fract 3 _ (h/upd :V7omit1))
               (tup _ s2-)
               (chans [(patch :choirs) (each (par s0 s1 s2))]
                      [o1
                       (each (fract 4 _ (h/upd :V7omit1) ))
                       (each (tup s0 s2 s4))
                       (m/connect 3)])
               (adjust 36)))

(comment :simple-triad-motive

         (play
          aeolian
          (h/lin :Im :VIIo :Im :IIIo :IVm :#IVo :Vsus4 :VIIo)
          (h/align-contexts :s)
          (lin s0 s1 s0 s1-)
          (chans [(patch :acoustic-bass) (each t-round o2-)]
                 [(patch :choir) vel4 (each (par s0 s2 s4))]
                 [(patch :ocarina) (each [(maybe s1 s1-)
                                          (one-of (tup s0 [s2 (lin d1 s0)] s1 s0 s1)
                                                  (tup s2 [s0 (lin c1- s0)] s1 s2 s1))])]))
         '[:export-to-cljs
           (mapv (fn [e]
                   (-> e
                       (update :position float)
                       (update :duration float)))
                 (numerify-pitches
                  (mk
                   aeolian
                   (h/lin :Im :VIIo :Im :IIIo :IVm :#IVo :Vsus4 :VIIo)
                   (h/align-contexts :s)
                   (lin s0 s1 s0 s1-)
                   (chans [(patch :acoustic-bass) (each t-round o2-)]
                          [(patch :choir) vel4 (each (par s0 s2 s4))]
                          [(patch :ocarina) (each [(maybe s1 s1-)
                                                   (one-of (tup s0 [s2 (lin d1 s0)] s1 s0 s1)
                                                           (tup s2 [s0 (lin c1- s0)] s1 s2 s1))])]))))]

         (play
          dur2
          aeolian
          (h/lin :Im :VIIo :Im :IIIo [:IVm :aeolian] :#IVo :Vsus4 :VIIo)
          (h/align-contexts :s)
          (lin s0 s1 s0 s1-)
          (chans [(patch :acoustic-bass) (each t-round o2-)]
                 [(patch :choir) vel4 (each (tup (par s0 s2 [s4 d1]) (par s0 s2 s4)))]))

         (swap! options* assoc-in [:tracks 0] :chorium)

         (play
          dur2
          aeolian
          (h/lin :Im :VIIo :Im :IIIo [:IVm :aeolian] :#IVo :Vsus4 :VIIo)
          (h/align-contexts :s)
          (lin s0 s1 s0 s1-)
          (chans [(patch :choir) vel5 (each t-round o2-)]
                 [(patch :choir) vel5 (each (par (tup (par s0 [s2 d1] s4) (par s0 s2 s4))
                                                 [vel4
                                                  (par s6 s8)
                                                  (shuftup s0 s1 s2)]))]))

         (play
          dur2
          aeolian
          (h/lin :Im :bVI :Vsus4 :VIIo :IIIo :IVm :Im :bII :Vsus4 :VIIo)
          (h/align-contexts :s)
          (lin s0 s1 s0 s1-)
          (chans [(patch :choir) vel5 (each t-round o2-)]
                 [(patch :choir) vel5 (each (par (tup (par s0 [s2 d1] s4) (par s0 s2 s4))
                                                 [vel4
                                                  (par s6 s8)
                                                  (shuftup s0 s1 s2)]))]
                 [(patch :harp) vel4
                  (each o1
                        [(shuftup s0 s1 s2 s3 s4 s5)
                         (mixtup s0 s2- s2)]
                        (vel-humanize 10))])))
