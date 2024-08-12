(ns noon.tries.scratch
  (:use noon.score)
  (:require [noon.lib.harmony :as h]
            [noon.lib.melody :as m]
            [noon.lib.rythmn :as r]
            [noon.utils.misc :as u]
            [noon.midi :as midi]
            [clojure.math.combinatorics :as comb]))

(comment

  (swap! options* assoc :tracks {0 :chorium})

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
               (mk dur4
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
