(ns noon.lib.melody
  "providing facilities to deal with melodic development"
  (:refer-clojure :exclude [cat struct while])
  (:require [noon.score :as n]
            #_[noon.utils.chance :as g]
            [noon.utils.contour :as c]
            [noon.utils.sequences :as s]
            [noon.utils.misc :as u]
            [noon.harmony :as h]
            [clojure.math.combinatorics :as comb]
            [noon.utils.pseudo-random :as pr]))

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
                     {:as current :keys [layer-idx score]}]
                  (if (= (:layer-idx r1) layer-idx)
                    (cons (update r1 :score into score) rs)
                    (cons current ret)))
                () (sort-by :position parts))))

    (defn sorted-layer-splits [layer s]
      (->> (layer-split layer s)
           (sort-by :position)
           (map (fn [{:keys [score position]}]
                  (n/shift-score score (- position))))))

    (defn sorted-position-splits [s]
      (->> (group-by :position s)
           (sort-by key)
           (map (fn [[position events]]
                  (n/shift-score (set events) (- position))))))

    (comment
      (layer-split :s
                   (mk (tup s0 s1 s2)
                       ($ (tup d0 d1 d2))))))

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
         (n/sf_ (n/concat-scores
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
         (n/sf_ (n/concat-scores
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
                                  (one-of* (map d-step (range -3 4))) 5}))])

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

    (defn score-lowest-layer
      "return the lowest harmonic layer of a score.
       (:c < :d < :s < :t)"
      [s]
      (let [layers (into #{} (mapcat (comp keys :position :pitch) s))]
        (first (keep layers [:c :d :s :t]))))

    (defn contour-change [layer f]

      (n/sfn s
             (assert (apply = (map (fn [e] (dissoc (:pitch e) :position)) s))
                     "For now, only mono harmony scores are supported here")

             (let [layer (or layer (score-lowest-layer s))
                   layer-converter (partial h/down-to-layer layer)
                   splits (sort-by :position (layer-split layer s))
                   idxs (map :layer-idx splits)
                   contour (mapv (n/sub (apply min idxs)) idxs)
                   new-contour (f contour)

                   deltas (mapv - new-contour contour)
                   position-key (layer-kw->position-key layer)]
               (n/concat-scores
                (map-indexed (fn [i {:keys [position score]}]
                               (->> score
                                    (map (fn [e]
                                           (-> (update e :pitch layer-converter)
                                               (update :position - position)
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

       :layer : (all commands, default to lowest common layer)
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
        {:as _opts :keys [pick nth layer extent delta]}]
       (let [pick (or pick nth :random)]
         (contour-change
          layer
          (case cmd
            :mirror c/contour-mirror
            :rotation #(s/member (c/contour-inversions %) pick)
            :similar #(s/member (c/similars % (or extent delta 0)) pick))))))

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
      (n/sf_ (let [nxt (n/concat-score _ (n/upd (connect _) step))]
               (cond (empty? nxt) nil
                     (done? nxt) (n/upd nxt finish)
                     :else (recur nxt)))))

    (u/defclosure simple-line
      "a simple way to create a line of given 'length using the given 'step"
      [length step]
      (n/sf_ (let [last-event (fn [s] (-> (sort-by :position s) last))
                   {:as _connection dur :duration} (last-event _)
                   normalise (fn [e] (assoc e :position 0 :duration dur))
                   connect (fn [s] (-> (last-event s) normalise hash-set))
                   total-duration (* dur length)
                   done? (fn [s] (> (n/score-duration s) total-duration))]
               (n/upd _ (line connect step done? (n/trim 0 total-duration))))))

    (defn simple-tupline [len step]
      (n/fit (simple-line len step)))

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
                   (while (within-time-bounds? 0 32)
                     (append [start-from-last
                              (any-that (within-pitch-bounds? :C-1 :C2)
                                        (rep 3 d3 :skip-first)
                                        (rep 3 d3- :skip-first)
                                        d1 d1-)]))
                   (adjust 3))))

(do :connect

    (defn $connect [f]
      (n/sf_ (let [sorted (sort-by :position _)]
             (reduce (fn [s [n1 n2]]
                       (into s (f n1 n2)))
                     #{(last sorted)} (partition 2 1 sorted)))))

    (defn simple-connection [sizes]
      (fn [start end]
        (let [hcs (loop [sizes sizes]
                    (if-let [[s & sizes] (seq sizes)]
                      (or (h/simplest-connection s (:pitch start) (:pitch end))
                          (recur sizes))))
              duration (/ (:duration start) (dec (count hcs)))]

          (map-indexed (fn [idx pitch]
                         (assoc start
                                :pitch pitch
                                :position (+ (* idx duration) (:position start))
                                :duration duration))
                       (butlast hcs)))))

    (defn connect
      "Tries to connect subsequent notes using one of the given step-sizes.
       Intermediate step notes are selected on lowset layer in priority."
      [& step-sizes]
      ($connect (simple-connection step-sizes))))

(defn stup
  "build a tup of steps on the specified layer
   form: (stup layer steps)
     layer: :c | :d | :s | :t
     steps: a sequence of ints."
  [layer steps]
  (n/tup* (map (partial n/layer-step layer) steps)))

(defn stup>
  "build a tup of successive steps on the specified layer
   form: (stup layer steps)
     layer: :c | :d | :s | :t
     steps: a sequence of ints."
  [layer steps]
  (n/tup>* (map (partial n/layer-step layer) steps)))

(n/defclosure* append>
  "accumulative append"
  [xs]
  (n/lin* (map n/append xs)))

(comment :gen-tup

         (defn gen-line [{:as opts :keys [_contour _grow layer _pick]}]
           (tup* (map (partial layer-step layer) (c/gen-line opts))))

         (play dur:2
               (patch :whistle)
               (gen-line {:contour [6 4] :grow 6 :layer :c})
               (append c3)
               (append c4)
               (append c2)
               (append rev)
               (append ($ (maybe o1 o1-))))

         (play dur:2
               (patch :whistle)
               (gen-line {:contour [6 4] :grow 6 :layer :c})
               (append> c3 c4 c2 rev)
               (dup 2))

         #_(defn gen-steps [{:keys [min max]}])

         (defn gen-tup
           ([{:as _options
              :keys [layer size steps pick delta]
              :or {layer :d steps (range -7 8) pick :rand delta 0}}]
            (let [step (partial n/layer-step layer)]
              (n/tup>* (map step (pr/shuffle (s/member (u/sums delta size steps)
                                                       pick)))))))

         (def DEFAULT_LAYERS_DELTAS
           {:c 12 :d 7 :s 3 :t 1})

         (defn gen-tup2
           ([{:as _options
              :keys [layer length size steps bounds]
              :or {layer :d size 0}}]
            (let [delta (DEFAULT_LAYERS_DELTAS layer)
                  steps (or steps (let [abs-rng (range 1 (inc delta))]
                                    (concat (map - abs-rng) abs-rng)))
                  _bounds (or bounds [(- delta) delta])]
              (n/tup>* (map (partial n/layer-step layer)
                            (pr/shuffle (pr/rand-nth (u/sums size length steps)))))))
           ([layer length size & {:as options}]
            (gen-tup2 (assoc options :layer layer :length length :size size)))))

(comment :sum-scratch-useless

         "When building a step line it is important to be aware of the intermediate values of the sum we use"

         (def SAMPLE_SUMS
           (u/sums 0 4 (remove zero? (range -5 6))))

         (def SAMPLE_STEP_LINES
           (mapcat comb/permutations SAMPLE_SUMS))


         (defn step-line-infos [steps]
           (let [vals (reductions + steps)
                 abs-steps (map u/abs steps)
                 abs-vals (map u/abs vals)
                 cnt (count steps)]
             {:steps steps
              :values vals
              :step-bounds (c/bounds steps)
              :val-bounds (c/bounds vals)
              :abs-step-bounds (c/bounds abs-steps)
              :abs-vals-bounds (c/bounds abs-vals)
              :val-repetitions (- cnt (count (set vals)))
              :step-repetitions (- cnt (count (set steps)))
              :mean-step (/ (reduce + (map u/abs steps)) cnt)}))

         (step-line-infos (rand-nth SAMPLE_STEP_LINES))

         (play (patch :electric-piano-1)
               (tup* (take 32 (map (partial stup> :d) (pr/shuffle SAMPLE_STEP_LINES))))
               (append>
                (superpose [(chan 1) o1 d3 (patch :flute) vel7]
                           [(k (tup IV VI)) (dupt 2) (chan 2) (patch :acoustic-bass) t2- vel10])
                (transpose c3)

                #_ [(transpose c4-)
                    (superpose [(chan 2) o1 d5 (patch :vibraphone) ($ (probs {vel0 3 _ 1}))])])
               (adjust 48))

         (count SAMPLE_STEP_LINES)
         (sort-by (comp (juxt :val-repetitions :abs-step-bounds)
                        step-line-infos)
                  SAMPLE_STEP_LINES)

         (defn sums

           ([total size steps]
            (let [smin (apply min steps)
                  smax (apply max steps)]
              (sums total size (sort steps) (* size smin) (* size smax) smin smax)))
           ([total size steps tmin tmax]
            (sums total size (sort steps) tmin tmax (apply min steps) (apply max steps)))
           ([total size steps tmin tmax min-step max-step]
            (if (and (>= tmax total tmin)
                     (>= (* size max-step) total (* size min-step)))
              (cond
                (= 1 size) (if ((set steps) total)
                             (list (list total)))
                :else (mapcat (fn [step]
                                (map (partial cons step)
                                     (sums (- total step) (dec size)
                                           (drop-while (fn [s] (< s step)) steps)
                                           tmin tmax step max-step)))
                              steps)))))

         (count (sums 10 5 [-5 -1 0 1 5] -5 15))
         (count (sums 10 5 (range -10 11) -50 50)))
