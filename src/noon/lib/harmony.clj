(ns noon.lib.harmony
  (:refer-clojure :exclude [cat struct while drop])
  (:require [noon.score :as n]
            [noon.harmony :as h]
            [noon.constants :as nc]
            [noon.utils.misc :as u]
            [noon.utils.sequences :as s]
            [clojure.core :as c]
            [clojure.math.combinatorics :as comb]))

(do :help

    (defn bounds-gte [[a b] [c d]]
      (and (<= a c) (>= b d)))

    (defn in-bounds [bounds s]
      (bounds-gte bounds (n/pitch-value-bounds s))))

(do :voicings

    (def VOICE_LEADING_MAX_SHIFT 7)
    (def INVERSIONS_MAX_SHIFT 4)

    (def abstract-drops
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
         (fn [size]
           (sort-by
            comparable
            (map octave-split
                 (comb/permutations (range size))))))))

    (def closed
      (n/sf_ (let [[[v1 bass] & others]
                   (sort (map (juxt n/pitch-value identity) _))]
               (->> others
                    (map (fn [[v note]]
                           ((n/t-shift (quot (c/- v1 v) 12)) note)))
                    (into #{bass})))))

    (def drops
      (do memoize ; TODO uncomment memo
          (fn [s]
            (let [size (count s)
                  _ (assert (c/< size 8) "cannot drop more than 7 notes")
                  notes (vec (sort-by n/pitch-value (closed s)))]
              (map (fn [d] (set (cons (notes 0)
                                      (mapcat (fn [o idxs] (map (fn [idx] ((n/t-shift o) (notes (inc idx)))) idxs))
                                              (range) d))))
                   (abstract-drops (dec size)))))))

    (u/defclosure drop
      [x]
      (n/sf_ (s/member (drops _) x)))

    (def shiftings
      "try to speed up shiftings"
      (fn
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
            :downward (take length-downward (iterate (partial shift :down) s))}))))

    (u/defclosure inversion
      [n]
      (n/sf_
       (cond (zero? n) _
             (pos? n) (nth (:upward (shiftings _)) n)
             :else (nth (:downward (shiftings _)) (- n)))))

    (defn voicings
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
                     (drops (closed s))))))

    (defn pitch-values [chord]
      (vec (sort (map n/pitch-value chord))))

    (def voice-led

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

(u/defclosure align-contexts

  "align successive harmonic contexts based on the given 'layer:
     :tonic (:t) | :structural (:s) | :diatonic (:d) | :chromatic (:c)

   can also take a second argument 'mode:
     :incremental | :static
   that stays if the alignement is done on the first chord only or incrementally."

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

(n/defclosure* grid-zipped
  "zip the current score (which should represent an harmonic grid)
   to the resulting of applying 'xs updates to a fresh score."
  [xs]
  (n/sf_ (let [seed (dissoc (first _) :position :duration :pitch)
               zip-fn (fn [x y] (n/upd y {:pitch (h/hc+ (:pitch (first x)))}))]
           (n/upd _ (n/zip zip-fn (n/k seed (n/lin* xs)))))))

(defn- connect-trimmed-chunks [xs]
  (reduce (fn [score x]
            ;; TODO do it
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

(n/defclosure* grid
  "grid"
  [xs]
  (n/sf_ (->> (map (fn [[position [{:keys [duration pitch]}]]]
                     (n/upd _
                            [(n/trim position (+ position duration))
                             {:pitch (h/hc+ pitch)}]))
                   (sort-by key (group-by :position (n/mk* xs))))
              (connect-trimmed-chunks))))

(defn modal-struct [size]
  (n/ef_ (if-let [s (some-> _ :pitch :struct nc/struct->mode-keyword)]
           (n/upd _ (n/struct (vec (sort (take size s)))))
           _)))

(comment :tries

         (time (->> (shiftings (mk (par> d0 d3 d3 d3 d3)))
                    :upward
                    (take 10)
                    (map pitch-values)))

         (closed (mk (par> d0 d3 d3 d3 d3)))

         (time (count (voicings (mk tetrad (par s0 s1 s2 s3))
                                {:bounds [30 90]}))))
