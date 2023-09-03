(ns noon.tries.try1
  (:use noon.score))

(let [triad (par s0 s1 s2)]

  (play (cat triad [IV triad])))
