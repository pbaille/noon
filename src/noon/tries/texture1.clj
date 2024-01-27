(ns noon.tries.texture1
  (:use noon.score)
  #_(:require [noon.lib.rythmn :as r]
              [noon.utils.misc :as u]))

(comment
  (play dur2
        lydian
        (patch :flute)
        (chans _ d3 d6 d9)
        ($ [(dupt 24) ($ (one-of vel1 vel3 vel6)
                         (probs {_ 6 d1 1}))])
        ($by :channel (maybe rev))
        (append (transpose c3-))
        (append (transpose c1-)))

  (play dur3
        lydian
        (chans [(patch :marimba) (cat _ c1)]
               [(patch :vibraphone) (cat d3 d2)]
               [(patch :celesta) (cat d6 d6)]
               [(patch :orchestral-harp) (cat d9 d9)])
        (append (transpose c2-))
        (dup 2)

        ($ [(dupt 34)
            ($ (one-of vel0 vel3 vel6 vel9)
               (probs {_ 4 o1 1}))]))

  (play dur8
        o2
        (dupt 128)
        ($ (par> d4 d4 d4)
           (one-of vel0 vel1 vel2 vel3 vel4 vel5))))
