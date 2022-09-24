(ns noon.lib.melody
  "providing facilities to deal with melodic development"
  (:use noon.score)
  (:refer-clojure :exclude [cat struct while])
  (:require [noon.utils.chance :as g]
            [noon.utils.contour :as c]
            [noon.utils.sequences :as s]
            [noon.utils.misc :as u :refer [f_ defclosure]]
            [noon.harmony :as h]))

(do :help

    (defn layer-split
      "partition a score by position and harmonic layer index.
       returns a seq of maps with keys :position :layer-idx :score"
      [layer s]

      (assert (apply = (map (fn [e] (dissoc (:pitch e) :position)) s))
              "for now only mono harmony scores are supported here")

      (let [layer-idx (fn [e] (h/layer-idx layer (:pitch e)))

            parts (map (fn [[p xs]]
                         {:position p
                          :layer-idx (last (sort (map layer-idx xs)))
                          :score (set xs)})
                       (group-by :position s))]

        (reduce (fn [[r1 & rs :as ret]
                    {:as current :keys [layer-idx position score]}]
                  (if (= (:layer-idx r1) layer-idx)
                    (cons (update r1 :score into score) rs)
                    (cons current ret)))
                () (sort-by :position parts))))

    (defn sorted-layer-splits [layer s]
      (->> (layer-split layer s)
           (sort-by :position)
           (map (fn [{:keys [score position]}]
                  (shift-score score (- position))))))

    (defn sorted-position-splits [s]
      (->> (group-by :position s)
           (sort-by key)
           (map (fn [[position events]]
                  (shift-score (set events) (- position))))))

    (layer-split :s
                 (mk (tup s0 s1 s2)
                     ($ (tup d0 d1 d2)))))

(do :permutations-rotations

    (defn rotation
      ([]
       (rotation :random {}))
      ([x]
       (if (map? x)
         (rotation :random x)
         (rotation x {})))
      ([pick {:as options :keys [layer layers]}]
       (let [layers (or layers (if layer (list layer)))]
         (sf_ (concat-scores
               (if-let [nxt-layers (next layers)]
                 (map (rotation pick (assoc options :layers nxt-layers))
                      (sorted-layer-splits (first layers) _))
                 (s/rotation (if layers
                               (sorted-layer-splits (last layers) _)
                               (sorted-position-splits _))
                             pick)))))))

    (defn permutation
      ([]
       (permutation :random {}))
      ([x]
       (if (map? x)
         (permutation :random x)
         (permutation x {})))
      ([pick {:as options :keys [layer layers]}]
       (let [layers (or layers (if layer (list layer)))]
         (sf_ (concat-scores
               (if-let [nxt-layers (next layers)]
                 (map (permutation pick (assoc options :layers nxt-layers))
                      (sorted-layer-splits (first layers) _))
                 (s/permutation (if layers
                                  (sorted-layer-splits (last layers) _)
                                  (sorted-position-splits _))
                                pick options)))))))

    (comment :examples

             (play (rup 8 d1)
                   (permutation {:grade 5 :layer :d}))

             (play (rup 8 d1)
                   (permutation))

             (play dur2
                   (tup s0 s1 s2 s3)
                   ($ (tup d1 d1- d0))
                   (permutation :rand {:layer :s}))

             (play dur2
                   (tup s0 s1 s2)
                   ($ (rup 4 d1))
                   (permutation 0 {:layers [:s :d]}))

             (play {:description "rand harmonic seq using IV II and VI degrees on vibraphone,
                                  ocarina melody derives using transposition, rotation and permutation."}

                   (chans

                    [(patch :vibraphone)
                     vel3
                     (tupn 4 [(one-of IV II VI) tetrad (par [t2- vel5] s0 s1 s2 s3)])]

                    [(patch :ocarina)
                     vel5
                     (shuftup d1 d2 d3 d4 d5)
                     ($ (maybe (par d0 d3)))
                     (rup 16
                          (probs {(permutation :rand) 1
                                  (rotation :rand) 3
                                  (one-of* (map di (range -3 4))) 5}))])

                   (adjust 10)
                   (append [d2- (transpose c3)]
                           [d2 (transpose c3-)]
                           same))))

(do :contour

    (defn layer-kw->position-key [layer-kw]
      (case layer-kw
        (:tonic :t) :t
        (:structural :s) :s
        (:diatonic :d) :d
        (:chromatic :c) :c))

    (defn contour-change [layer f]

      (sfn s
           (assert (apply = (map (fn [e] (dissoc (:pitch e) :position)) s))
                   "for now only mono harmony scores are supported here")

           (let [splits (sort-by :position (layer-split layer s))
                 idxs (map :layer-idx splits)
                 contour (mapv (sub (apply min idxs)) idxs)
                 new-contour (f contour)

                 deltas (mapv - new-contour contour)
                 position-key (layer-kw->position-key layer)]

             (concat-scores
              (map-indexed (fn [i {:keys [position score layer-idx]}]
                             (->> score
                                  (map (fn [e]
                                         (-> (update e :position - position)
                                             (update-in [:pitch :position position-key] + (deltas i)))))
                                  (into #{})))
                           splits)))))

    (defn contour
      "changing the melodic contour of a score.

       forms:
       (contour :mirror <options>) : mirror the contour of the score.
       (contour :rotation <options>) : rotate the contour of the score.
       (contour :similar <options>) : get a different score with the same contour.

       <options>
       a map that may contain some of those keys:

       :layer : (all commands, default to :chromatic)
           The harmonic layer on which the contour transformation is performed

       :pick|:nth : (:rotation and :similar commands, default to :random)
           A 'member-pick (see `member function) to select one particular outcome.

       :extent : (:similar command only)
           A vector of min and max amount of deformation that we want to apply to the score.

       :delta : (:similar command only)
           The amount of shrinking or growing we want to apply to the score."
      ([cmd]
       (contour cmd {}))
      ([cmd
        {:as opts :keys [pick nth layer extent delta]
         :or {layer :chromatic}}]
       (let [pick (or pick nth :random)]
         (contour-change
          layer
          (case cmd
            :mirror c/contour-mirror
            :rotation #(s/member (c/contour-inversions %) pick)
            :similar #(s/member (c/similars % (or extent delta)) pick))))))

    (comment :contour-tries

             (mk (tup d0 d1 d2)
                 (contour :mirror {:layer :d}))

             (mk dur:2
                 (shufcat s0 s1 s2 s4)
                 ($ (tup d0 c1- d1 d0))
                 (cat same
                      [dur:4 vel0]
                      (contour :mirror {:layer :s})
                      [dur:4 vel0]
                      (contour :similar {:extent [4 4] :layer :s})
                      [dur:4 vel0]
                      (contour :rotation {:layer :s})))))

(do :line

    (u/defclosure line
      "creating a melodic line.
       1. 'connect is called on the current score, to produce another one that will be fed into 'step
       2. 'step is called on the result of 'connect, and the result concatenated with the original score.
       3. if 'done? called with this new score is true, feed it to 'finish and return.
       4. else go to step 1."
      [connect step done? finish]
      (sf_ (let [nxt (concat-score _ (upd (connect _) step))]
             (cond (empty? nxt) nil
                   (done? nxt) (upd nxt finish)
                   :else (recur nxt)))))

    (u/defclosure simple-line
      "a simple way to create a line of given 'length using the given 'step"
      [length step]
      (sf_ (let [last-event (fn [s] (-> (sort-by :position s) last))
                 {:as connection dur :duration} (last-event _)
                 normalise (fn [e] (assoc e :position 0 :duration dur))
                 connect (fn [s] (-> (last-event s) normalise hash-set))
                 total-duration (* dur length)
                 done? (fn [s] (> (score-duration s) total-duration))]
             (upd _ (line connect step done? (trim 0 total-duration))))))

    (defn simple-tupline [len step]
      (fit (simple-line len step)))

    (comment :line-tries

             (play (simple-line 32 (one-of s1 s1- d1 d1-))
                   (adjust 4))

             (play (simple-line 32 (one-of s1 s1- (tup d1 d0 s2-) (tup d1- d0 s2)))
                   (adjust 8))

             (play dur:4
                   (simple-line 64
                                (one-of (catn> 4 (one-of d1- d1))
                                        (tup d1 d1- s0)
                                        (cat s2 s1 s1-)
                                        (catn> 4 (one-of s1- s1))))
                   (chans (patch :electric-piano-1)
                          [(patch :ocarina) o1 ($ d3)]))

             (play {:description "another way to build a melodic line from a bunch of randomly chosen updates"}
                   (patch :acoustic-guitar-nylon)
                   (while (within-time-bounds? 0 18)
                     (append [start-from-last
                              (any-that (within-pitch-bounds? :C-1 :C2)
                                        (rep 3 d3)
                                        (rep 3 d3-)
                                        d1 d1-)]))
                   (adjust 3))))
