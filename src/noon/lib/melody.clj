(ns noon.lib.melody
  "providing facilities to deal with melodic development"
  (:require [noon.score :as n]
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
                  (n/shift-score (set events) (- position)))))))

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
                                  pick options))))))))

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
            :similar #(s/member (c/similars % (or extent delta 0)) pick)))))))

(do :line

    (defn line
      "Create a melodic line.
       1. 'connect is called on the current score, to produce another one that will be fed into 'step
       2. 'step is called on the result of 'connect, and the result is concatenated with the original score.
       3. if 'done? called with this new score is true, feed it to 'finish and return.
       4. else go to step 1."
      [connect step done? finish]
      (n/sf_ (let [nxt (n/concat-score _ (n/upd (connect _) step))]
               (cond (empty? nxt) nil
                     (done? nxt) (n/upd nxt finish)
                     :else (recur nxt)))))

    (defn simple-line
      "A simple way to create a line of given 'length using the given 'step"
      [length step]
      (n/sf_ (let [last-event (fn [s] (-> (sort-by :position s) last))
                   {:as _connection dur :duration} (last-event _)
                   normalise (fn [e] (assoc e :position 0 :duration dur))
                   connect (fn [s] (-> (last-event s) normalise hash-set))
                   total-duration (* dur length)
                   done? (fn [s] (> (n/score-duration s) total-duration))]
               (n/upd _ (line connect step done? (n/trim 0 total-duration))))))

    (defn simple-tupline [len step]
      (n/fit (simple-line len step))))

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

          (if hcs
            (map-indexed (fn [idx pitch]
                           (assoc start
                                  :pitch pitch
                                  :position (+ (* idx duration) (:position start))
                                  :duration duration))
                         (butlast hcs))
            [start]))))

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

(defn gen-line
  "Generate a tuple by generating a contour, producing lines from it and picking one.
   options are:
   - :layer, the harmonic layer the contour is based on (default to :diatonic)
   - :contour, a vector [contour-length contour-height].
   - :grow, a vector [min-grow max-grow] that is used to grow the generated contour.
   - :pick, a member-pick argument used to pick one line from generated ones. (default :rand)"
  [opts]
  (n/tup* (map (partial n/layer-step (:layer opts :d))
             (c/gen-line opts))))

(defn step-seqs
  "Return a collection of step sequences according to given options.
   A step sequence is a vector of integers like [-1 2 1 -3]
   - :delta is the overall step of the resulting sequences.
     e.g a step sequence [1 2 1] have a delta of 4.
   - :length is the number of steps the resulting sequences will count.
   - :bounds specifies the allowed range for sequence's intermediates values
     it is a vector of the form [minimal-intermediate-value maximum-intermediate-value]
   - :steps specifies available steps
   - :step-range is an alternative to :steps
     it specifies the minimal and maximal steps via a vector [min-step max-step]
     all intermediate values are allowed.
     e.g :step-range [-2 2] is equivalent to :steps [-2 -1 1 2] (zero step is excluded)."
  [{:keys [delta length bounds steps step-range]
    :or {delta 0 step-range [-3 3]}}]
  (let [steps (or steps (remove zero? (range (get step-range 0 -3) (inc (get step-range 1 3)))))
        within-bounds
        (fn [s]
          (or (not bounds)
              (let [reds (reductions + 0 s)]
                (and (>= (apply min reds) (get bounds 0))
                     (<= (apply max reds) (get bounds 1))))))
        drop-until-in-bound
        (fn [sum-permutations]
          (if-let [[perm & perms] (seq sum-permutations)]
            (if (within-bounds perm)
              [perm perms]
              (recur perms))))]
    (letfn [(looop [sums-permutations]
              (if-not (empty? sums-permutations)
                (if-let [[s perms] (drop-until-in-bound (first sums-permutations))]
                  (cons s (lazy-seq (looop (concat (rest sums-permutations) (list perms)))))
                  (looop (rest sums-permutations)))))]
      (looop (u/lazy-map (pr/shuffle (u/sums delta length steps))
                         (comp comb/permutations pr/shuffle))))))

(defn gen-tup
  ([{:as options
     :keys [layer]}]
   (if-let [step-seq (first (step-seqs options))]
     (n/tup>* (map (partial n/layer-step layer)
                   step-seq))))
  ([layer length delta & {:as options}]
   (gen-tup (assoc options :layer layer :length length :delta delta))))

(def connect-repetitions
  (n/$by (juxt :track :channel :voice)
         (n/sf_ (let [[e1 & todo] (sort-by :position _)]
                  (loop [[last-note & prev-notes :as ret] (list e1)
                         todo todo]
                    (if-let [[e & todo] (seq todo)]
                      (if (and (= (n/pitch-value e) (n/pitch-value last-note))
                               (= (dissoc e :position :duration :pitch)
                                  (dissoc last-note :position :duration :pitch)))
                        (recur (cons (update last-note :duration + (:duration e))
                                     prev-notes)
                               todo)
                        (recur (cons e ret) todo))
                      (set ret)))))))
