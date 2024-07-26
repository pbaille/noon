(ns noon.lib.harmony
  "Harmony related utilities"
  (:refer-clojure :exclude [drop])
  (:require [noon.score :as n]
            [noon.harmony :as h]
            [noon.constants :as nc]
            [noon.utils.misc :as u]
            [noon.utils.contour :as uc]
            [noon.utils.sequences :as s]
            [clojure.core :as c]
            [clojure.math.combinatorics :as comb]))

(do :help

    (defn bounds-gte
      "Check if bounds [a b] contains bounds [c d]."
      [[a b] [c d]]
      (and (<= a c) (>= b d)))

    (defn in-bounds
      "Check if the score `s` is within given pitch `bounds`."
      [bounds s]
      (bounds-gte bounds (n/pitch-value-bounds s))))

(do :voicings

    (def ^{:doc "The maximal step that can occur betwwen extreme voices of a voice leading."}
      VOICE_LEADING_MAX_SHIFT 7)

    (def ^{:private true}
      sorted-octave-splits
      (letfn [(octave-split [perm]
                (loop [[x1 & xs] (next perm)
                       current [(first perm)]
                       ret []]
                  (cond (not x1) (conj ret current)
                        (c/> x1 (peek current)) (recur xs (conj current x1) ret)
                        :else (recur xs [x1] (conj ret current)))))

              (comparable [x]
                ((juxt (partial apply max) (partial apply min) identity)
                 (vec (mapcat (fn [octave offset]
                                (mapv (partial + (* offset 10E6)) octave))
                              x (range)))))]
        (memoize
         (fn [base inversions]
           (sort-by
            comparable
            (map octave-split
                 (if inversions
                   (comb/permutations base)
                   (let [[bass & others] (sort base)]
                     (map (partial cons bass)
                          (comb/permutations others))))))))))

    (defn abstract-drops
      "Get a list of abstract drops for `x`.
       An abstract-drop is a vec of the form [[first-octave-degree...][second-octave-degree...]...]
       e.g a drop 2 for a tetrad can be represented as `[[0 2 3][1]]`
       `x` can be either:
       - a natural number indicating the number of notes.
       - a sequence of numbers representing different notes."
      [x & [include-inversions]]
      (let [base (cond (int? x) (range x)
                       (sequential? x) x)]
        (sorted-octave-splits base include-inversions)))

    (def ^{:doc "Put a chord into closed position.
                 Bring every notes within the octave following the bass note.
                 If some notes have the same pitch class, it can produce unisons."
           :tags [:harmonic :voicing]}
      closed
      (n/sf_ (let [[[v1 bass] & others]
                   (sort (map (juxt n/pitch-value identity) _))]
               (->> others
                    (map (fn [[v note]]
                           ((n/t-shift (quot (c/- v1 v) 12)) note)))
                    (into #{bass})))))

    (def ^{:doc "Put a chord into closed position.
                 Starting at bass note, bring every other notes as close as possible above it."
           :tags [:harmonic :voicing]}
      closed-no-unison
      (n/sf_ (let [[[v1 bass] & others]
                   (sort (map (juxt n/pitch-value identity) _))]
               (loop [ret #{bass} pitch-values #{v1} notes others]
                 (if-let [[[v note] & notes] (seq notes)]
                   (letfn [(looop [n]
                             (let [pitch-value (n/pitch-value n)]
                               (if (contains? pitch-values pitch-value)
                                 (looop (n/o1 n))
                                 [n pitch-value])))]
                     (let [[n v] (looop ((n/t-shift (quot (c/- v1 v) 12)) note))]
                       (recur (conj ret n) (conj pitch-values v) notes)))
                   ret)))))

    (def ^{:doc "Computes all possible drops of the given score (that is supposed to represent a chord).
                 An :inversions option can be given to include inversions and their drops."}
      drops
      (memoize
       (fn [s & {:keys [inversions]}]
         (assert (c/< (count s) 8)
                 "cannot drop more than 7 notes")
         (let [notes (vec (sort-by n/pitch-value (closed s)))
               contour (uc/contour (map n/pitch-value notes))
               abstract-drops (abstract-drops contour inversions)]
           (map (fn [d]
                  (set (mapcat (fn [o idxs]
                                 (map (fn [idx]
                                        ((n/t-shift o) (notes (inc idx))))
                                      idxs))
                               (range) d)))
                abstract-drops)))))

    (defn drop
      "Build an update that produce a drop of the received score (that is expected to represent a chord).
       `x` is a member-pick argument that is used to pick a drop from the complete list of possible drops.
       refer to `noon.utils.sequences/member` for complete documentation"
      {:tags [:harmonic :voicing]}
      [x]
      (n/sf_ (s/member (drops _ :inversions false) x)))

    (defn shiftings
      "compute downward and upward inversion of the given chord (score).
       return a map containing
       - :self, the received chord (score)
       - :upward, the list of upward inversions
       - :downward, the list of downward inversions."
      ([s]
       (shiftings s [0 127]))
      ([s bounds]
       (let [size (count s)
             pitch-values (sort (map n/pitch-value (closed s)))

             neighbourhoods (->> (-> (cons (- (last pitch-values) 12) pitch-values)
                                     (u/snoc (+ 12 (first pitch-values))))
                                 (partition 3 1)
                                 (map (fn [[dwn x up]]
                                        [(mod x 12)
                                         {:down (- dwn x)
                                          :up (- up x)}]))
                                 (into {}))

             get-neighbourhood (fn [n]
                                 (get neighbourhoods (n/pitch-class-value n)))
             shift (fn [dir x]
                     (set (map (fn [n]
                                 (update n :pitch
                                         (comp h/c->t (h/c-step (get (get-neighbourhood n) dir)))))
                               x)))

             space-upward (- (bounds 1) (last pitch-values))
             space-downward (- (first pitch-values) (bounds 0))
             length-upward (* size (inc (quot space-upward 12)))
             length-downward (* size (inc (quot space-downward 12)))]

         {:self s
          :upward (take length-upward (iterate (partial shift :up) s))
          :downward (take length-downward (iterate (partial shift :down) s))})))

    (defn inversion
      "Build an update that produce an inversion of the received chord (score).
       `x` is an integer that correspond to the index of the desired inversion.
       - negative `x` picks the nth downward inversion
       - positive `x` picks the nth upward inversion."
      {:tags [:harmonic :voicing]}
      [n]
      (n/sf_
       (cond (zero? n) _
             (pos? n) (nth (:upward (shiftings _)) n)
             :else (nth (:downward (shiftings _)) (- n)))))

    (defn voicings
      "Computes a list of possible voicings for the given chord (score `s`).
       The second argument is an option map that contain:
       - :bounds, a vector of the form [lowest-pitch-value highest-pitch-value]"
      [s {:as _opts :keys [bounds]}]
      (let [check (partial in-bounds bounds)]
        (mapcat (fn [{:keys [self upward downward]}]
                  (let [self-bounds (n/pitch-value-bounds self)]
                    (concat (if (bounds-gte bounds self-bounds) [self])
                            (if (>= (bounds 1) (self-bounds 1))
                              (->> upward (drop-while (complement check)) (take-while check)))
                            (if (<= (bounds 0) (self-bounds 0))
                              (->> downward (drop-while (complement check)) (take-while check))))))
                (map #(shiftings % bounds)
                     (drops s :inversions true)))))

    (defn pitch-values
      "Returns a sorted vector of all pitch-values in chord (score)."
      [chord]
      (vec (sort (map n/pitch-value chord))))

    (def ^{:doc "Apply voice leading to the received score."
           :tags [:harmonic :voicing]}
      voice-led
      (letfn [(voice-leading-score
                [a b]
                (let [vas (map n/pitch-value a)
                      vbs (map n/pitch-value b)
                      best-moves (concat (map (fn [va] (first (sort (map (partial u/dist va) vbs)))) vas)
                                         (map (fn [vb] (first (sort (map (partial u/dist vb) vas)))) vbs))]
                  [(/ (reduce + best-moves)
                      (count best-moves))
                   (apply max best-moves)]))

              (voice-lead2
                [a b]
                (let [[mina maxa] (n/pitch-value-bounds a)
                      candidates (voicings b {:bounds [(- mina VOICE_LEADING_MAX_SHIFT)
                                                       (+ maxa VOICE_LEADING_MAX_SHIFT)]})]
                  (first
                   (sort-by (partial voice-leading-score a)
                            candidates))))]

        (n/sf_ (let [[x1 & xs :as _groups] (map (comp set val) (sort-by key (group-by :position _)))]
                 (loop [ret [x1] todo xs]
                   (if-let [[x & xs] (seq todo)]
                     (recur (conj ret (voice-lead2 (peek ret) x)) xs)
                     (reduce into #{} ret))))))))

(defn align-contexts
  "align successive harmonic contexts based on the given 'layer:
     :tonic (:t) | :structural (:s) | :diatonic (:d) | :chromatic (:c)

   can also take a second argument 'mode:
     :incremental | :static
   that stays if the alignement is done on the first chord only or incrementally."
  {:tags [:harmonic]}
  ([] (align-contexts :structural :incremental))
  ([layer] (align-contexts layer :incremental))
  ([layer mode]
   (n/sf_ (let [[x1 & xs] (sort-by :position _)]
            (loop [ret [x1] todo xs]
              (if-let [[x & xs] (seq todo)]
                (let [aligned (h/align layer
                                       (:pitch (case mode :incremental (peek ret) :static x1))
                                       (:pitch x))]
                  (recur (conj ret (assoc x :pitch aligned)) xs))
                (set ret)))))))

(u/defn* grid-zipped
  "zip the current score (which should represent an harmonic grid)
   to the resulting of applying 'xs updates to a fresh score."
  {:tags [:harmonic]}
  [xs]
  (n/sf_ (let [seed (dissoc (first _) :position :duration :pitch)
               zip-fn (fn [x y] (n/upd y {:pitch (h/hc+ (:pitch (first x)))}))]
           (n/upd _ (n/zip zip-fn (n/k seed (n/lin* xs)))))))

(defn- connect-trimmed-chunks [xs]
  (reduce (fn [score x]
            (let [{trimmed-fws true score-rest nil} (group-by :trimed-fw score)
                  {trimmed-bws true x-rest nil} (group-by :trimed-bw x)]
              (loop [ret (set (concat score-rest x-rest)) fws trimmed-fws bws (set trimmed-bws)]
                (if-not (seq fws)
                  (into ret bws)
                  (let [[fw & fws] fws]
                    (if-let [bw (some (fn [x] (and (= (n/pitch-value x) (n/pitch-value fw))
                                                   (= (:position x) (+ (:position fw) (:duration fw)))
                                                   x))
                                      bws)]
                      (recur (conj ret (-> bw
                                           (update :position - (:duration fw))
                                           (update :duration + (:duration fw))))
                             fws (disj bws bw))
                      (recur (conj ret fw) fws bws)))))))
          #{} xs))

(u/defn* grid
  "Build an update that applies an harmonic grid to the received score.
   The harmonic grid is created by threading a fresh score through the sequence of update `xs`.
   e.g `(noon.score/mk* xs)`.
   The resulting score (which represent an harmonic grid) is zipped over the received score,
   All harmonies are applied accordingly to their position and duration."
  [xs]
  (n/sf_ (->> (map (fn [[position [{:keys [duration pitch]}]]]
                     (n/upd _
                            [(n/trim position (+ position duration))
                             {:pitch (h/hc+ pitch)}]))
                   (sort-by key (group-by :position (n/mk* xs))))
              (connect-trimmed-chunks))))

(defn modal-struct
  "Change the harmonic struct of the received event to its <`size`> most characteristic degrees"
  {:tags [:harmonic :chord]}
  [size]
  (n/ef_ (if-let [s (some-> _ :pitch :scale nc/scale->mode-keyword nc/degree-priority)]
           (update _ :pitch  h/upd (h/struct (vec (sort (take size s)))))
           _)))

(def ^{:doc "Build a structural chord on top of received event."
       :tags [:event-update :chord :harmonic]}
  simple-chord
  (n/ef_ (let [struct-size (-> _ :pitch :struct count)]
           (n/upd #{_} (n/par* (mapv n/s-step (range struct-size)))))))

(comment :tries

         (time (->> (shiftings (mk (par> d0 d3 d3 d3 d3)))
                    :upward
                    (take 10)
                    (map pitch-values)))

         (closed (mk (par> d0 d3 d3 d3 d3)))

         (time (count (voicings (mk tetrad (par s0 s1 s2 s3))
                                {:bounds [30 90]}))))
