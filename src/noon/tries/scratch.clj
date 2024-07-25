(ns noon.tries.scratch
  (:use noon.score)
  (:require [noon.lib.harmony :as h]
            [noon.lib.melody :as m]
            [noon.lib.rythmn :as r]
            [noon.utils.misc :as u]
            [noon.midi :as midi]
            [clojure.math.combinatorics :as comb]))

(comment :motivation

         (play
          (scale :phrygian)
          (tup d0 d1 d2 d4)
          (append c4 c8)))

(comment

  (swap! options* assoc :tracks {0 :chorium})

  (play vel2 dur2
        (cat* (map reroot [:C :Eb :F# :A]))
        ($ (tup (rebase V (struct :sus47))
                (rebase (scale :lydian) (struct [0 2 3 6]))))
        (chans
         [($ (par s0 s1 s2 s3)) h/voice-led]
         ($ [vel0 o1 (tupn> 8 (one-of d1 d1- d3 d3-))])
         ($ [(repitch :C-1) t-round]))

        (rep 4 s1))

  (play dur2
        (chans [(patch :woodblock) (dupt 4)]
               [(patch :tinkle-bell) (r/gen-tup 12 5 {:shifted true :durations [1 2 3]})])
        (dup 4)))

(comment ::m/gen-tup
         (play (scale :melodic-minor)
               (catn 8 (! (m/gen-tup {:layer :d :length 10 :delta 2 :steps [-3 -1 1 3]}))))
         (noon {:play true
                :tracks {0 :chorium}
                :filename "./test/data/scratch/1"}
               (mk dur2
                   (patch :electric-piano-1)
                   (scale :harmonic-minor)
                   (cat I VII I IV I [IIb lydian] V I)
                   (h/align-contexts)
                   (append (transpose c3) (transpose c1-) same)
                   ($ (m/gen-tup {:layer :s :length 6 :delta 2 :steps [-2 -1 1 2]}))
                   ($ (maybe (par s0 [vel3 s1] [vel3 s2])))
                   ($ (superpose [(chan 2) o1 (patch :ocarina)
                                  (m/gen-tup {:layer :s :length 4 :delta 1 :steps [-1 1 -2 2]})
                                  (maybe rev)
                                  (vel-humanize 1/2 [0.1 0.5])])))))

(comment :demos

         (do :utils

             (defn fill [dur f]
               (sf_ (let [sdur (score-duration _)
                          n (quot sdur dur)]
                      (assert (zero? (rem sdur dur))
                              "fill: division length should be a multiple of score length ")
                      (upd _ (tupn n f)))))

             (defn fill> [dur f]
               (sf_ (let [sdur (score-duration _)
                          n (quot sdur dur)]
                      (assert (zero? (rem sdur dur))
                              "fill: division length should be a multiple of score length ")
                      (println n)
                      (upd _ (tupn> n f))))))

         (do :roman-degrees-and-more

             (play (scale :harmonic-minor)
                   (cat I IV VII I)
                   (h/align-contexts :s)
                   ($ (tup s0 s1 s2)))

             (play dur:2
                   (scale :harmonic-minor)
                   (cat I IV VII I)
                   (h/align-contexts :s)
                   (cat s0 s1 s2-)
                   ($ [(tup s0 s2)
                       ($ (tup s0 c1- s+ s0))])
                   (cat same rev))

             (swap! options* assoc :tracks {0 :chorium}))

         (do :line-connection

             (do (defn catm [n f]
                   (rep n [(sf_ #{(assoc (last (sort-by :position _))
                                         :position 0)})
                           f]))

                 (defn line [len f]
                   (sf_ (let [nxt (upd _ (append [(sf_ #{(assoc (last (sort-by :position _))
                                                                :position 0)})
                                                  f]))]
                          (if (and (not-empty nxt) (<= (count nxt) len))
                            (recur nxt)
                            (set (take len (sort-by :position nxt)))))))

                 (defn tupline [len f]
                   (fit (line len f)))

                 (def s? (one-of s2- s1- s1 s2)))

             (play {:desc "catm demo"}
                   lydian+
                   dur8
                   (cat* (map root [:C :Eb :F# :A]))
                   ($ (fit [(catm 16
                                  (any-that (within-pitch-bounds? :C-2 :C2)
                                            (cat s0 c1- s0)
                                            (cat s0 d1 d1- s0)
                                            (cat d1 s0)
                                            (cat s0 d1- d1 s0 s1)
                                            s1
                                            s1-))])))

             (midi/stop2))

         (do :tries

             (play dur:2
                   lydian+2
                   (cat C0 Eb0 F#0 A0)
                   (rep 4 s1)
                   ($ (tup s0 [s1 d1] s1 s2 [s2 d1-])))

             (play [lydian+2 sus47]
                   [dur:2 (cat C0 Eb0 F#0 A0)]
                   ($ (chans
                       s0
                       (shuftup s0 s1 s2 s3)))
                   (rep 8
                        (parts (chan 0) s1-
                               (chan 1) s1))))

         (do :grid-zipped

             (play {:description "grid zipping demo"}
                   (patch :electric-piano-1)
                   ;; a simplistic grid
                   harmonic-minor
                   (tup* (map root [:C :Eb :Gb :A]))
                   (h/align-contexts :s)
                   ;; a melodic pattern
                   (h/grid-zipped
                    (tupn 4 (tup s0 s1 s2))
                    ($ (tup s0 [s2 (superpose o1)] s4)))
                   ;; adjust and repeat
                   (adjust 2)
                   (rep 4 ($ c1-)))

             (play vel3
                   ;; grid
                   ;; 4 tonalities
                   (tup* (map root [:C :Eb :F# :A]))
                   ;; V I on each
                   ($ (tup [V sus47] [lydian+ tetrad]))
                   ;; align structurally
                   (h/align-contexts :s)

                   (h/grid-zipped
                    (tupn 4
                          (par
                           ;; simple arpegio
                           [o1 (tup (rup 6 s1) [s6 (rup 6 s1-)])]
                           ;; closed chord
                           [(par s0 s1 s2 s3) (tupn 2 (maybe (shuftup s1- s1)))])))

                   (adjust 8))))

(comment
  (play)
  (play (cat [I melodic-minor] [VI superlocrian] [VIb lydianb7] [IIb mixolydian])
        (h/align-contexts :s)
        (dup 2)
        ($ (chans [(patch :vibraphone) vel6 t0 (par> d0 d3 d3 d3 d3)]
                  [(patch :acoustic-bass) vel6 t2-]
                  [(patch :taiko-drum) (tup vel3 vel5 vel0)]
                  [(tupn> 9 (any-that (within-pitch-bounds? :G-1 :C2)
                                       d1- d1 d3 d3- d4 d4-))
                   vel9
                   (chans (patch :flute)
                          [o1- vel6 (patch :vibraphone)])]))
        (cat _ c6)
        (dup 2)))

(comment :melodic-development

         (defn take-lst [n]
           (sf_ (let [sorted (sort-by :position _)]
                  (if (>= (count sorted) n)
                    (let [taken (take-last n sorted)]
                      (set (upd (set taken) {:position (sub (:position (first taken)))})))))))

         (stop)
         (play
          dorian
          dur:7
          (while (within-time-bounds? 0 8)
            (append
             (any-that (within-pitch-bounds? :C0 :C3)
                       [(take-lst 1) (one-of d1- d1)]
                       [(take-lst 8) (m/permutation [0 1/4])]
                       [(take-lst 4) rev]
                       [(take-lst 4) (m/contour :similar {:extent [-2 2] :layer :d})]))
            (trim 0 8))
          ($
           (probs {(one-of vel3 vel5 vel7 vel9) 6
                   (superpose [(chan 2) (patch :vibraphone) vel8 (one-of d3 d4)]) 1
                   (superpose [(chan 7) (patch :flute) vel8 o1]) 5
                   ;vel0 1
                   }))
          (superpose (k (catn 4 [(chan 5) (patch :acoustic-bass) t2- vel8 dur2])))

          #_($ (d-shift 2))
          (rep 4 (one-of [(d-shift -2) (transpose c3)]
                         [(d-shift 2) (transpose c3-)]
                         [(d-shift 1) (transpose c1-)]
                         [(d-shift -3) (transpose c6)]))
          (append (superpose (k (catn 4 [(patch :taiko-drum) (chan 3) (! [vel4 (maybe o1- d1) (r/gen-tup 7 3)])])
                                (dup 8))))
          #_(start-from 16))

         (play (patch :taiko-drum)
               (rep 4 o1))

         (stop))


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
             {:up {:c (upd s c1)
                   :d (upd s d1)
                   :s (upd s s1)
                   :t (upd s t1)}
              :down {:c (upd s c1-)
                     :d (upd s d1-)
                     :s (upd s s1-)
                     :t (upd s t1-)}}))

         (play dorian
               (rep 4 s1)
               ($ (tup c1- s2 s1 s0))
               (tup _ rev)
               (rep 4 (transpose c3))
               (append rev))

         (play dorian
               (rep 4 s1)
               ($ (tup _ s2))
               ($ (tup c1- d2 d1 d0)))

         (play melodic-minor
               dur4
               (append (transpose c3) (transpose c6) (transpose c3))
               (dup 2)
               ($ (shuftup s0 s1 s2 s3 s4))
               ($ (tup _ (one-of s1 s2 s1- s2- s3 s3-)))
               ($ (one-of (tup c1- d2 d1 d0)
                          (tup c1- s1- s0 s2))))

         (stop)
         (play dur4
               (append (transpose c3) (transpose c6) (transpose c3))
               ($ (one-of phrygian6 lydian melodic-minor))
               (dup 2)
               ($ (chans [(patch :acoustic-bass) t2- (tup _ s2 s1- _)]
                         [(patch :flute) vel8]
                         [(patch :vibraphone) vel4 (par s0 d4 d6 d8 d10 d12)]
                         [(patch :taiko-drum)
                          (r/gen-tup 10 4 :euclidean)
                          ($ [(one-of s0 s1 s1-) (one-of vel1 vel3 vel5)])]))
               (parts (chan 1)
                      [($ (shuftup s0 s1 s2 s3 s4))
                       ($ (tup _ (one-of s1 s2 s1- s2- s3 s3-)))
                       ($ (one-of (tup c1- d2 d1 d0)
                                  (tup c1- s1- s0 s2)
                                  (tup c1- s1- s2- s0)))
                       ($ (one-of vel5 vel6 vel7 vel9))]))

         (play melodic-minor
               (shufcat s0 s1 s2 s3)
               ($ (let [step (one-of s1 s2 s3 s1- s2- s3-)
                        ap (cat c1- d1 s1-)]
                    (tup [_ ap] [step ap] _ step)))
               (append c2- c2-))

         (play melodic-minor
               (cat (shufcat s0 s1 s2 s3)
                    [{:passing true} (shufcat s0 s1 s2 s3)])
               ($ (let [step (one-of s1 s2 s3 s1- s2- s3-)
                        ap (cat c1- d1)]
                    (tup [_ ap] [step ap] _ step (par s2- s2))))
               (append c4-)
               (dup 2))

         (play melodic-minor
               dur:3
               (shufcat s0 s2 s4)
               ($ (one-of (shuftup _ c1- d1)
                          (shuftup _ d1 d1-)))
               (m/permutation :rand)
               (rep 3 (one-of (s-shift 1) (s-shift -1)))
               (rep 3 (transpose c3))
               (dup 2))

         (play hungarian

               (cat I IV)
               ($cat [(shuftup s0 s2 s4)
                      (one-of (tup c1- _ d1) (tup d1- _ d1))
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
              (upd _
                   (if (= c-space 2)
                     (tup (d-step step) (c-step step) _)
                     (tup (d-step step) (case side :up c1- :down d1) _))))))

         (play dur4
               (rup 6 (one-of d4 d3-))
               ($ (tup (chromatic-double-passing :down) [d6 (chromatic-double-passing :up)])))

         (def diatonic?
           (sf_ (if (every? (comp nh/diatonic? nh/normalise :pitch)
                            _)
                  _)))

         (mk c2
             diatonic?)

         (nh/neibourhood (:pitch (first (mk d1-))))

         (play harmonic-minor
               dur2
               (cat I IV V I)
               (append (transpose c3) (transpose c6))
               (h/align-contexts :s)
               ($ (chans [(patch :string-ensemble-1) vel4 (par s2- s0 s2)]
                         [(patch :ocarina) (shuftup s0 s1 s2) ($ (tup c1- [s2- (cat d1 _)] d1 _ s1 s2))])))

         (let [c-d+ (efn e (if-let [p- (get-in (nh/neibourhood (:pitch e)) [:down :c])]
                             (assoc e :pitch p-)
                             (d1 e)))]
           (play dur:4
                 (rep 14 d1)
                 ($ (tup c-d+ _))))

         (defn interpose-with [f]
           (sf_ (if (line? _)
                  (set (mapcat (fn [[a b]] (if b ((f a b)) a))
                               (partition 2 1 nil (sort-by :position _)))))))

         (defn interleaved [& xs]
           (sf_ (let [scores (map (partial upd _) xs)
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
                  [(catn 4 (shuftup s0 s1 s2 s3)) ($ rand-double-passing)]
                  [(catn 4 (shuftup s0 s1 s2 s3)) s2 ($ rand-double-passing)])))

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
                         (apply rand-interleaving (map (fn [u] (sort-by :position (upd _ u))) xs))))))

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
                  [(chan 1) (catn 4 (shuftup s0 s1 s2 s3)) ($ rand-double-passing)]
                  [(chan 2) (catn 4 (shuftup s0 s1 s2 s3)) s4- ($ rand-double-passing)]
                  [(chan 3) (catn 4 (shuftup s0 s1 s2 s3)) s4 ($ rand-double-passing)]))))
