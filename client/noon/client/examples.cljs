(ns noon.client.examples
  (:require [noon.utils.misc :as u]
            [noon.score :as score]))

(def examples
  {:trivial (u/source-str (play (tup s0 s1 s2)))

   :complex-tritonal
   (u/source-str (play (let [I (one-of [lydian+ (structure [2 3 4 5 6])] [melodic-minor (structure [1 2 4 5 6])])
                             V (one-of [V mixolydian (structure [1 3 4 5 6])] [V phrygian6 (structure [0 1 3 5 6])])
                             [B G Eb] (map root [:B :G :Eb])]

                         [(tup [B V] [B I] [G V] [G I] [Eb V dur2] [Eb I dur2])
                          (rup 4 (transpose d2-))
                          (h/align-contexts :s :static)

                          (chans

                           [(patch :choir-aahs)
                            vel4
                            (each (par s0 s1 s2 s3 s4))]

                           [(patch :vibraphone)
                            vel3
                            (each (probs {(par s0 s1 s2 s3 s4) 1
                                          (shuftup [dur2 (par s0 s2 s4)] [(one-of dur2 dur3) (par s1- s1 s3)]) 3}))]

                           [(patch :acoustic-bass)
                            vel5
                            (each [tetrad o2- t0 (maybe (tup (one-of dur2 dur3) [dur2 o1-]))])]

                           [(patch :taiko-drum)
                            vel3
                            (each (shuftup s0 s1 s2 s3 s4))
                            (each (probs {vel0 3 same 1 (one-of o1 o1-) 1 (tup t0 t1) 1}))]

                           [vel7
                            (h/grid-zipped
                             [(chans (patch :flute) [o1 (patch :piccolo)])
                              (ntup> (* 32 10)
                                     (any-that (within-pitch-bounds? :C-2 :C2)
                                               s1 s2 s1- s2- s3 s3-))]
                             (each (probs {vel0 1
                                           same 4
                                           (superpose (one-of s1 s2 s3)) 0})))])

                          (adjust 48)])))

   :canon
   (u/source-str (play
                  (let [decorate (sf_ (let [sorted (sort-by :position _)]
                                        (reduce (fn [s [n1 n2]]
                                                  (into s (noon.score/update-score #{n1 n2} (maybe (m/connect 1)))))
                                                #{(last sorted)} (partition 2 1 sorted))))]
                    [dur2
                     (lin (shuftup s0 s1 s2 s3)
                          [(one-of s1 s1-) (shuftup s0 s1 s2 s3)])
                     decorate
                     (lin _ (s-shift 1) (s-shift -1) _)
                     (lin _ (s-shift 2))
                     (chans [(patch :ocarina) o1 (s-shift -1)]
                            [(sf_ (noon.score/shift-score _ 2))]
                            [(patch :acoustic-bass) o2- (s-shift 1) (sf_ (noon.score/shift-score _ 5))])
                     (h/grid dur2
                             harmonic-minor
                             (lin I IV VII I [IV melodic-minor VII] IV [V harmonic-minor VII] VII)
                             (dup 4)
                             (h/align-contexts :s))])))})
