(ns noon.lib.rythmn
  (:refer-clojure :exclude [cat])
  (:require [noon.score :as n]
            [noon.utils.euclidean-sums :as eucl]
            [noon.utils.sequences :as s]
            [noon.utils.misc :as u]
            [clojure.core :as c]
            [noon.utils.pseudo-random :as pr]))

(do :impl

    (def random-kw?
      #{:random :rand})

    (def memo-sums (memoize u/sums))

    (defn rand-sum [total size members]
      (pr/shuffle (pr/rand-nth (memo-sums total size members))))

    (defn score-fw-shifts
      "returns a sequence of forward time shifts by 'increment preserving the original score duration.
       the shifting stops when the last event goes out of score."
      [score increment]
      (let [duration (n/score-duration score)
            last-event (last (sort-by :position score))]
        (map (fn [shift]
               (n/update-score (n/shift-score score shift)
                    (n/trim 0 duration)))
             (range 0 (:duration last-event) increment))))

    (defn rand-shift [resolution]
      (n/sf_ (let [increment (/ (n/score-duration _) resolution)]
             (pr/rand-nth (score-fw-shifts _ increment)))))

    (defn slice-score
      "slice a score into n parts of equal duration."
      [score n]
      (let [duration (n/score-duration score)
            increment (/ duration n)
            points (map (n/mul increment) (range 0 (inc n)))]
        (map (fn [[from to]]
               (n/update-score score [(n/between from to) (n/trim from to)
                           (n/sf_ (if (empty? _) (n/mk n/vel0 {:duration increment :position from}) _))
                           (n/sf_ (n/shift-score _ (- from)))]))
             (partition 2 1 points))))

    (comment :tries

             (play (dup 3)
                   (trim 0.5 2.5))

             (map qshow (tup-shifts 1/3
                                    (mk (dup 3))))))

(do :basic

    (defn sum->tup [xs]
      (n/tup* (map n/dur xs)))

    (u/defn* durtup
      "build a tup from some numbers"
      [xs]
      (sum->tup xs))

    (defn rotation
      "time rotation of a score.
       forms:
          (rotation <offset>) : rotate by the given offset
          (rotation :relative <fractional>) : rotate to a fraction of the total length
          (rotation :rand-by <increment>) : pick a random rotation using increment as resolution.
          (rotation :rand-sub <n>) : split the score in 'n parts and rotate to a randomly picked one.
       "
      ([offset]
       (n/sf_ (let [duration (n/score-duration _)]
                (n/update-score _
                       [(n/$ {:position (fn [p] (mod (+ p offset) duration))})
                        (n/trim 0 duration)]))))
      ([k arg]
       (case k
         :relative (n/sf_ (n/update-score _ (rotation (* (n/score-duration _) arg))))
         :rand-by (n/sf_ (n/update-score _ (rotation (pr/rand-nth (range 0 (n/score-duration _) arg)))))
         :rand-sub (n/sf_ (n/update-score _ (rotation (* (pr/rand-nth (range 0 arg)) (/ (n/score-duration _) arg))))))))

    (defn permutation
      "permute a score by time slices,
        'n is the number of slices,
        'i is a permutation index (int:absolute | float:gradual | :random)."
      ([n]
       (permutation n :random))
      ([n i]
       (n/sf_ (n/concat-scores (s/permutation (slice-score _ n) i)))))

    (do :euclidean

        (defn euclidean-tup
          "make a tuple from an euclidean sum"
          [resolution size]
          (sum->tup (eucl/euclidean-sum size resolution)))

        (defn euclidean-tups
          "an euclidean tuple and its rotations"
          [resolution size]
          (->> (eucl/euclidean-sum size resolution)
               s/rotations distinct
               (mapv sum->tup)))

        #_(defn euclidean-shifted-tups
            "A shifted euclidean tuple is one that do not necessarly start at the begining position.
       This function list all potentially rotated and/or shifted euclidean tuples for the given arguments."
            [size resolution]
            (->> (eucl/euclidean-sum size resolution)
                 s/rotations distinct
                 (mapcat (fn [xs]
                           (map (fn [shift]
                                  (->> (- (last xs) shift)
                                       (u/snoc (butlast xs))
                                       (map dur)
                                       (cons [vel0 (dur shift)])
                                       tup*))
                                (range 0 (last xs))))))))

    (defn gen-tup-options [x]
      (let [flags (map first (take-while (comp keyword? second) (partition 2 1 (concat x [:end]))))
            rst (drop (count flags) x)
            [kvs m] (if (even? (count rst)) [rst {}] [(butlast rst) (last rst)])]
        (merge (zipmap flags (repeat true))
               (apply hash-map kvs)
               m)))

    (defn gen-tup
      "generate a rythmic tup based on the given arguments:
       resolution: the number of subdivisions that we will use.
       size: the number of notes that the generated tup will contain.
       options:
         euclidean: generates an euclydean tup.
         durations: the multiples of 'resolution that we are allowed to use (fractionals allowed).
         shifted: the possibility for the generated tup to not begin on beat."

      ([resolution]
       (gen-tup resolution (inc (pr/rand-int resolution)) {}))
      ([resolution size]
       (gen-tup resolution size {}))
      ([resolution size & options]
       (let [{:keys [shifted durations euclidean]
              :or {durations (range 1 (inc resolution))}} (gen-tup-options options)
             t (if euclidean
                 (pr/rand-nth (euclidean-tups resolution size))
                 (sum->tup (rand-sum resolution size durations)))]
         (if shifted
           (n/chain t (rand-shift resolution))
           t))))

    (do :bintup

        "experimental, may be integrated with gen-tup via an option"

        (defn sum->bins [sum]
          (mapcat (fn [n] (cons {:bintup 0} (next (repeat n {:bintup 1})))) sum))

        (defn gen-bintup
          "works like gen-tup except:
             every notes length equal `resolution`
             every note is marked with :gentup 0

           in addition to this, every hole between notes is filled with notes of duration `resolution` and marked :bintup 1.

           this will allow you to get some binary accentuated tups, for instance like this:
           (play
            (gen-bintup 18 10 :shifted)
            (parts {:bintup 0} vel2
                   {:bintup 1} vel6))

           gen-tup doc:
           generate a rythmic tup based on the given arguments:
           resolution: the number of subdivisions that we will use.
           size: the number of notes that the generated tup will contain.
           options:
             euclidean: generates an euclydean tup.
             durations: the multiples of 'resolution that we are allowed to use (fractionals allowed).
             shifted: the possibility for the generated tup to not begin on beat."

          ([resolution]
           (gen-bintup resolution (inc (pr/rand-int resolution)) {}))
          ([resolution size]
           (gen-bintup resolution size {}))
          ([resolution size & options]
           (let [{:keys [shifted durations euclidean]
                  :or {durations (range 1 (inc resolution))}} (gen-tup-options options)
                 sum (if euclidean
                       (eucl/euclidean-sum size resolution)
                       (rand-sum resolution size durations))
                 bins (sum->bins sum)]
             (n/tup* (if shifted
                       (pr/rand-nth (s/rotations bins))
                       bins)))))))
