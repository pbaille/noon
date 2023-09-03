(ns noon.tries.bintup1
  (:use noon.score)
  (:require [noon.lib.harmony :as h]
            [noon.lib.rythmn :as r]))


(play dur5
      (cat melodic-minor [lydian (transpose c4)])
      (dup 2)
      (h/align-contexts)
      ($ (chans [(patch :vibraphone) vel4 (par> d3 d3 d3)]
                [(patch :taiko-drum) (r/gen-tup 9 3 :durations [2 3 4]) ($ (one-of vel4 vel3) (maybe d3 d3-))]
                [(patch :acoustic-bass)
                 t-floor o1- (r/gen-bintup 9 4)
                 ($ (one-of vel3 vel5 vel7))
                 (parts {:bintup 0} _
                        {:bintup 1} ($ (one-of vel0 d3-)))]
                [(r/gen-bintup 27 11  :shifted :euclidean)
                 ($ (one-of vel0 vel3 vel5 vel7))
                 (parts {:bintup 0} [(patch :electric-piano-1)
                                     ($ (one-of d2 d4 d6)
                                        (probs {_ 3 [vel4 o1 (par _ d4 d5)] 1}))]
                        {:bintup 1} [(patch :marimba)  {:channel inc :velocity (mul 0.75)}
                                     ($ [(one-of d3 d5 d7) (maybe o1 (par _ d4))])])]))
      (append (transpose c1))
      (cat rev))
