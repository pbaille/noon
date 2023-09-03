(ns noon.doc.intro-fr-snippets
  (:use noon.score))


(comment

  ;; Initial

  (play)







  ;; Notes

  (play E0)

  (play F#2)







  ;; Durées

  (play dur2)

  (play dur:3)








  ;; Vélocités

  (play vel1)

  (play vel6)

  (play vel12)








  ;; combinaisons

  (play [E1 dur2 vel12])

  (play [E-1 dur:3 vel4])








  ;; Lignes et accords

  (play (cat vel3 vel5 vel7 vel11))

  (play (cat dur:2 dur:4 dur2 dur:2))

  (play (cat C0 E0 G0))

  (play (cat vel3 dur:2 G-1))

  (play (cat [C0 dur:2] [Eb0 dur:4] [G0 dur:4] C1))

  (play (par C0 E0 G0))

  (play (cat (par C0 E0)
             (par D0 F0)
             (par E0 G0)))

  (play (par (cat C0 D0)
             (cat E0 F0)
             (cat G0 A0)))









  ;; sons, instruments

  (play (patch :flute)
        (cat C0 E0 G0 C1))

  (play (patch :vibraphone)
        dur:4 vel5
        (cat C0 E0 G0 (par B0 D1)))









  ;; channels

  (play (chans [(patch :ocarina) dur:2
                (cat G0 Eb0 C0 G-1 F0 D0 A-1 F-1)]

               [(patch :vibraphone) dur2 vel4
                (cat (par C0 Eb0 G0) (par A-1 D0 F0))]

               [(patch :acoustic-bass) vel9
                (cat [dur3 C-2] G-2)])

        (dup 4))







  ;; intervals

  ())
