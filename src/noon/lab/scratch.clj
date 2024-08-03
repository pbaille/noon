(ns noon.lab.scratch
  (:use noon.score)
  (:require [noon.lab.multiscore :as ms]
            [noon.lab.multiscore2 :as ms2]
            [noon.utils.multi-val :as m]))

(comment
  (ms/play (par s0 s1 s2)
           (m/mix d0 d1 d2)
           (lin d0 d3))

  (ms/play (par s0 s1 s2)
           (m/join d0 d1 d2)
           (ms/lin* _))

  (ms/play (par s0 s1 s2)
           (m/join d0 d1 d2)
           ms/collect))

(defn cnt [ms]
  (count (m/get-all ms)))

(comment
  (ms2/update-multiscore
   (m/once score0)
   (ms2/chain* [d0 d1 d2]))

  (ms2/update-multiscore
   (m/once score0)
   (ms2/one-of* [d0 d1 (ms2/chain* [d1 d1])]))

  (ms2/play
   (ms2/one-of* [d0 d1 (ms2/chain* [d1 d1])])
   ms2/collect))
