(ns noon.scratch
  (:use noon.score)
  (:use noon.lib.reaper))

(play (chans [(patch :clarinet)
            (cat s1 s2 s3)]
           [(patch :acoustic-bass)
            o1- (cat s1 s2 s3)]))
(lin (rep 6 d3)
    (rep 4 d1-))

(cat d1 d2)
(tup d1- d1)
(chans _ o2-)
[rev]
[{:patch :ocarina}]

($ (tup s1 s2-))
(mk (patch :vibraphone)
   (catn 4 (shuftup d0 d3- (chans d3 [(patch :marimba) d6]) d4))
   (rep 6 (! (one-of d3 d3-
                     (transpose c3)
                     (transpose c1-)))))
