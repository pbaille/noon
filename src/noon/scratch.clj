(ns noon.scratch
  (:use noon.score)
  (:use noon.lib.reaper0))

(mk (rep 6 d3)
    (rep 4 d1-))

(tup s1 s2-)
(play (k (patch :vibraphone)
    (catn 4 (shuftup d0 d3- d3 d4))
    (rep 6 (! (one-of d3 d3-
                      (transpose c3)
                      (transpose c1-))))))
