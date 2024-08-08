(ns noon.scratch
  (:use noon.score)
  (:require [noon.utils.maps :as m]))

(comment :debug-filter-score
         (mk (dup 4)
             (between 1 3))

         (mk (dup 4)
             (from 1))

         (filter-score {:position (gte 1)}
                       (mk (dup 4)))

         (set (filter (fn [e] (m/match e {:position (gte 1)}))
                      (mk (dup 4))))

         (set (filter (->event-matcher {:position (gte 1)})
                      (mk (dup 4)))))

(comment
  (play (chans [(patch :clarinet)
                (lin s1 s2 s3)]
               [(patch :acoustic-bass)
                o1- (lin s1 s2 s3)]))
  (chain (rep 6 d3)
         (rep 4 d1-))

  (lin d1 d2)
  (tup d1- d1)
  (chans _ o2-)
  [rev]
  [{:patch :ocarina}]

  (each (tup s1 s2-))
  (mk (patch :vibraphone)
      (nlin 4 (shuftup d0 d3- (chans d3 [(patch :marimba) d6]) d4))
      (rep 6 (! (one-of d3 d3-
                        (transpose c3)
                        (transpose c1-))))))
