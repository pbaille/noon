(ns noon.scratch
  (:use noon.score))

(comment
  (play (chans [(patch :clarinet)
                (lin s1 s2 s3)]
               [(patch :acoustic-bass)
                o1- (lin s1 s2 s3)]))

  (chain (rep 6 d3)
         (rep 4 d1-))

  (play (patch :vibraphone)
        (nlin 4 (shuftup d0 d3- (chans d3 [(patch :marimba) d6]) d4))
        (rep 6 (! (one-of d3 d3-
                          (transpose c3)
                          (transpose c1-))))))
