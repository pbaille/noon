(ns noon.tries.scratch
  (:use noon.score)
  (:require [noon.lib.harmony :as h]
            [noon.lib.melody :as m]
            [noon.lib.rythmn :as r]
            [noon.utils.misc :as u]
            [noon.midi :as midi]
            [clojure.math.combinatorics :as comb]))

(comment

  (swap! options* assoc :tracks {0 :chorium})

  (play vel2 dur2
        (lin* (map reroot [:C :Eb :F# :A]))
        (each (tup (rebase V (structure :sus47))
                   (rebase (scale :lydian) (structure [0 2 3 6]))))
        (chans
         [(each (par s0 s1 s2 s3)) h/voice-led]
         (each [vel0 o1 (ntup> 8 (one-of d1 d1- d3 d3-))])
         (each [(repitch :C-1) t-round]))

        (rep 4 s1)))

(comment ::m/gen-tup
         (play (scale :melodic-minor)
               (nlin 8 (! (m/gen-tup {:layer :d :length 10 :delta 2 :steps [-3 -1 1 3]}))))
         (noon {:play true
                :tracks {0 :chorium}
                :filename "./test/data/scratch/1"}
               (mk dur4
                   (patch :electric-piano-1)
                   (scale :harmonic-minor)
                   (lin I VII I IV I [IIb lydian] V I)
                   (h/align-contexts)
                   (append (transpose c3) (transpose c1-) same)
                   (each (m/gen-tup {:layer :s :length 6 :delta 2 :steps [-2 -1 1 2]}))
                   (each (maybe (par s0 [vel3 s1] [vel3 s2])))
                   (each (superpose [(chan 2) o1 (patch :ocarina)
                                     (m/gen-tup {:layer :s :length 4 :delta 1 :steps [-1 1 -2 2]})
                                     (maybe rev)
                                     (vel-humanize 1/2 [0.1 0.5])])))))

(comment :demos

         (do :roman-degrees-and-more

             (play (scale :harmonic-minor)
                   (lin I IV VII I)
                   (h/align-contexts :s)
                   (each (tup s0 s1 s2)))

             (play dur:2
                   (scale :harmonic-minor)
                   (lin I IV VII I)
                   (h/align-contexts :s)
                   (lin s0 s1 s2-)
                   (each [(tup s0 s2)
                          (each (tup s0 c1- s+ s0))])
                   (lin same rev))

             (swap! options* assoc :tracks {0 :chorium}))

         (do :tries

             (play dur:2
                   lydian+2
                   (lin C0 Eb0 F#0 A0)
                   (rep 4 s1)
                   (each (tup s0 [s1 d1] s1 s2 [s2 d1-])))

             (play [lydian+2 sus47]
                   [dur:2 (lin C0 Eb0 F#0 A0)]
                   (each (chans
                          s0
                          (shuftup s0 s1 s2 s3)))
                   (rep 8
                        (parts (chan 0) s1-
                               (chan 1) s1))))

         (do :harmonic-zipping

             (play {:description "grid zipping demo 2"}
                   ;; a melodic pattern
                   [(patch :electric-piano-1)
                    (ntup 4 (tup s0 s1 s2))
                    (each (tup s0 [s2 (superpose o1)] s4))]
                   ;; simplistic grid
                   (h/grid harmonic-minor
                           (tup* (map root [:C :Eb :Gb :A]))
                           (h/align-contexts :s))
                   ;; adjust and repeat
                   (adjust 2)
                   (rep 4 (each c1-)))

             (play vel3
                   (h/harmonic-zip
                    ;; grid
                    [;; 4 tonalities
                     (tup* (map root [:C :Eb :F# :A]))
                     ;; V I on each
                     (each (tup [V sus47] [lydian+ tetrad]))
                     ;; align structurally
                     (h/align-contexts :s)]

                    ;; content
                    (ntup 4
                          (par
                           ;; simple arpegio
                           [o1 (tup (rup 6 s1) [s6 (rup 6 s1-)])]
                           ;; closed chord
                           [(par s0 s1 s2 s3) (shuftup s1- s0 s1)])))

                   (adjust 8))))

(comment :melodic-development

         (play
          dorian
          dur:7
          (repeat-while (within-time-bounds? 0 8)
                        (append
                         (any-that (within-pitch-bounds? :C0 :C3)
                                   [(start-from-nth-last 1) (one-of d1- d1)]
                                   [(start-from-nth-last 8) (m/permutation [0 1/4])]
                                   [(start-from-nth-last 4) rev]
                                   [(start-from-nth-last 4) (m/contour :similar {:extent [-2 2] :layer :d})]))
                        (trim 0 8))
          (each
           (probs {(one-of vel3 vel5 vel7 vel9) 6
                   (superpose [(chan 2) (patch :vibraphone) vel8 (one-of d3 d4)]) 1
                   (superpose [(chan 7) (patch :flute) vel8 o1]) 5
                   ;vel0 1
                   }))
          (superpose (k (nlin 4 [(chan 5) (patch :acoustic-bass) t2- vel8 dur2])))

          #_(each (d-shift 2))
          (rep 4 (one-of [(d-shift -2) (transpose c3)]
                         [(d-shift 2) (transpose c3-)]
                         [(d-shift 1) (transpose c1-)]
                         [(d-shift -3) (transpose c6)]))
          (append (superpose (k (nlin 4 [(patch :taiko-drum) (chan 3) (! [vel4 (maybe o1- d1) (r/gen-tup 7 3)])])
                                (dup 8))))
          #_(start-from 16)))


(comment :passings

         (defn lowest-layer [n]
           (let [p (get-in n [:pitch :position])]
             (cond
               (:c p) :c
               (:d p) :d
               (:s p) :s
               (:t p) :t)))

         (defn neibourhood [n]
           (let [s (hash-set n)]
             {:up {:c (update-score s c1)
                   :d (update-score s d1)
                   :s (update-score s s1)
                   :t (update-score s t1)}
              :down {:c (update-score s c1-)
                     :d (update-score s d1-)
                     :s (update-score s s1-)
                     :t (update-score s t1-)}}))

         (play dorian
               (rep 4 s1)
               (each (tup c1- s2 s1 s0))
               (tup _ rev)
               (rep 4 (transpose c3))
               (append rev))

         (play dorian
               (rep 4 s1)
               (each (tup _ s2))
               (each (tup c1- d2 d1 d0)))

         (play melodic-minor
               dur4
               (append (transpose c3) (transpose c6) (transpose c3))
               (dup 2)
               (each (shuftup s0 s1 s2 s3 s4))
               (each (tup _ (one-of s1 s2 s1- s2- s3 s3-)))
               (each (one-of (tup c1- d2 d1 d0)
                             (tup c1- s1- s0 s2))))

         (stop)
         (play dur4
               (append (transpose c3) (transpose c6) (transpose c3))
               (each (one-of phrygian6 lydian melodic-minor))
               (dup 2)
               (each (chans [(patch :acoustic-bass) t2- (tup _ s2 s1- _)]
                            [(patch :flute) vel8]
                            [(patch :vibraphone) vel4 (par s0 d4 d6 d8 d10 d12)]
                            [(patch :taiko-drum)
                             (r/gen-tup 10 4 :euclidean)
                             (each [(one-of s0 s1 s1-) (one-of vel1 vel3 vel5)])]))
               (parts (chan 1)
                      [(each (shuftup s0 s1 s2 s3 s4))
                       (each (tup _ (one-of s1 s2 s1- s2- s3 s3-)))
                       (each (one-of (tup c1- d2 d1 d0)
                                     (tup c1- s1- s0 s2)
                                     (tup c1- s1- s2- s0)))
                       (each (one-of vel5 vel6 vel7 vel9))]))

         (play melodic-minor
               (shuflin s0 s1 s2 s3)
               (each (let [step (one-of s1 s2 s3 s1- s2- s3-)
                           ap (lin c1- d1 s1-)]
                       (tup [_ ap] [step ap] _ step)))
               (append c2- c2-))

         (play melodic-minor
               (lin (shuflin s0 s1 s2 s3)
                    [{:passing true} (shuflin s0 s1 s2 s3)])
               (each (let [step (one-of s1 s2 s3 s1- s2- s3-)
                           ap (lin c1- d1)]
                       (tup [_ ap] [step ap] _ step (par s2- s2))))
               (append c4-)
               (dup 2))

         (play melodic-minor
               dur:3
               (shuflin s0 s2 s4)
               (each (one-of (shuftup _ c1- d1)
                             (shuftup _ d1 d1-)))
               (m/permutation :rand)
               (rep 3 (one-of (s-shift 1) (s-shift -1)))
               (rep 3 (transpose c3))
               (dup 2))

         (play dorian+4

               (lin I IV)
               (m/$lin [(shuftup s0 s2 s4)
                        ;; (one-of (tup c1- _ d1) (tup d1- _ d1))
                        (tup c1- _ d1)
                        (m/permutation :rand)
                        (rep 4 (one-of (s-shift 1) (s-shift -1)))])
               (append (transpose c3))
               (append (s-shift -1)))

         (require '[noon.harmony :as nh])
         (defn chromatic-double-passing [side]
           (sf_
            (assert (= 1 (count _))
                    (str `chromatic-double-passing
                         "works only on single note scores"))
            (let [target (first _)
                  d-suroundings (nh/diatonic-suroundings (:pitch target))
                  c-space (get d-suroundings (case side :up 1 :down 0))
                  step (case side :up 1 :down -1)]
              (update-score _
                            (if (= c-space 2)
                              (tup (d-step step) (c-step step) _)
                              (tup (d-step step) (case side :up c1- :down d1) _))))))

         (play dur4
               (rup 6 (one-of d4 d3-))
               (each (tup (chromatic-double-passing :down) [d6 (chromatic-double-passing :up)])))

         (def diatonic?
           (sf_ (if (every? (comp nh/diatonic? nh/normalise :pitch)
                            _)
                  _)))

         (mk c2
             diatonic?)

         (nh/neibourhood (:pitch (first (mk d1-))))

         (play harmonic-minor
               dur2
               (lin I IV V I)
               (append (transpose c3) (transpose c6))
               (h/align-contexts :s)
               (each (chans [(patch :string-ensemble-1) vel4 (par s2- s0 s2)]
                            [(patch :ocarina) (shuftup s0 s1 s2) (each (tup c1- [s2- (lin d1 _)] d1 _ s1 s2))])))

         (let [c-d+ (efn e (if-let [p- (get-in (nh/neibourhood (:pitch e)) [:down :c])]
                             (assoc e :pitch p-)
                             (d1 e)))]
           (play dur:4
                 (rep 14 d1)
                 (each (tup c-d+ _))))

         (defn interpose-with [f]
           (sf_ (if (line? _)
                  (set (mapcat (fn [[a b]] (if b ((f a b)) a))
                               (partition 2 1 nil (sort-by :position _)))))))

         (defn interleaved [& xs]
           (sf_ (let [scores (map (partial update-score _) xs)
                      counts (map count scores)
                      durations (map score-duration scores)]
                  (assert (apply = counts)
                          "interleaved scores should have same number of elements")
                  (assert (apply = durations)
                          "interleaved scores should have same duration")
                  (assert (apply = (mapcat (partial map :duration) scores))
                          "interleaved scores should have even durations")
                  (let [duration (/ (first durations) (first counts))
                        shift (/ duration (count scores))]
                    (:score
                     (reduce (fn [{:as state :keys [at]} xs]
                               (-> state
                                   (update :at + duration)
                                   (update :score into (map-indexed (fn [i n] (assoc n :position (+ at (* i shift)) :duration shift)) xs))))
                             {:score #{} :at 0}
                             (apply map vector (map sort-score scores))))))))

         (play dur4
               (interleaved
                (rup 8 d1 :skip-first)
                (rup 8 d1- :skip-first)))

         (let [up (one-of d1 s1)
               down (one-of c1- d1- s1-)
               rand-double-passing
               (one-of (tup up _ down _)
                       (tup down _ up _)
                       (tup down up down _)
                       (tup up down up _))]
           (play harmonic-minor
                 dur4
                 (interleaved
                  [(nlin 4 (shuftup s0 s1 s2 s3)) (each rand-double-passing)]
                  [(nlin 4 (shuftup s0 s1 s2 s3)) s2 (each rand-double-passing)])))

         (defn interleaving [polarities a b]
           (loop [s [] ps polarities a a b b]
             (if-let [[p & ps] (seq ps)]
               (let [[nxt a' b'] (case p 0 [(first a) (next a) b] 1 [(first b) a (next b)])]
                 (recur (conj s nxt) ps a' b'))
               s)))

         (defn rand-interleaving
           ([a b]
            (interleaving (shuffle (concat (repeat (count a) 0) (repeat (count b) 1)))
                          a b))
           ([a b & xs]
            (reduce rand-interleaving
                    (rand-interleaving a b)
                    xs)))

         (defn interleavings [a b]
           (reduce (fn [ret perm]
                     (conj ret (interleaving perm a b)))
                   []
                   (comb/permutations (concat (repeat (count a) 0) (repeat (count b) 1)))))

         (u/defn* randomly-interleaved
           "randomly interleave the result of the given updates"
           [xs]
           (sf_ (:score
                 (reduce (fn [state n]
                           (-> state
                               (update :score conj (assoc n :position (:at state)))
                               (update :at + (:duration n))))
                         {:at 0 :score #{}}
                         (apply rand-interleaving (map (fn [u] (sort-by :position (update-score _ u))) xs))))))

         (defn n-firsts [n]
           (sf_ (->> (group-by :position _)
                     (sort)
                     (take n)
                     (map second)
                     (reduce into #{}))))

         (let [up (one-of d1 s1)
               down (one-of c1- d1- s1-)
               rand-double-passing
               (one-of (tup _ up down _)
                       (tup _ down up _)
                       (tup up _ down _)
                       (tup down _ up _)
                       (tup down up down _)
                       (tup up down up _))]
           (play harmonic-minor
                 dur2
                 (randomly-interleaved
                  [(chan 1) (nlin 4 (shuftup s0 s1 s2 s3)) (each rand-double-passing)]
                  [(chan 2) (nlin 4 (shuftup s0 s1 s2 s3)) s4- (each rand-double-passing)]
                  [(chan 3) (nlin 4 (shuftup s0 s1 s2 s3)) s4 (each rand-double-passing)]))))
