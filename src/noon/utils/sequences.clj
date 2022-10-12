(ns noon.utils.sequences
  "sequences transformation with the consideration of increasing complexity"
  (:require [clojure.math.combinatorics :as c]
            [noon.utils.misc :as u]))

;; impl ----

(defn rotate [coll n]
  (let [c (count coll)
        n (if (>= n 0) (mod n c) (+ n (count coll)))
        splited (split-at n coll)]
    (concat (splited 1) (splited 0))))

(def shuffle-no-rep
  "shuffle a sequence while avoiding to produce successive equal elements."
  (letfn [(remove-nth [xs i]
            (concat (take i xs) (drop (inc i) xs)))
          (nth->first [xs i]
            (cons (nth xs i) (remove-nth xs i)))]
    (fn [xs]
      (let [cnt (count xs)
            safe-nth (fn [xs i]
                       (if (and (not (neg? i)) (< i cnt))
                         (nth xs i)))]
        (reduce
         (fn [xs i]
           (if (or (= (first xs) (nth xs i))
                   (= (safe-nth xs (dec i))
                      (safe-nth xs (inc i))))
             xs
             (nth->first xs i)))
         xs (take cnt (repeatedly #(rand-int cnt))))))))

;; member ---------

(defn mirror-idx [s idx]
  (- (dec (count s)) idx))

(defn decimal->idx
  "take a sequence and a number between 0 and 1,
   0 being the first idx and 1 the last.
   also supports negative numbers to (-1 being the first idx and -0 the last)
   return the corresponding integer index."
  [s d]
  (let [mirror? (neg? d)
        abs (u/abs d)
        cnt (count s)
        abs-idx (min (dec cnt)
                     (u/round (* abs cnt)))]
    (if mirror?
      (- (dec cnt) abs-idx)
      abs-idx)))
(= 2 (mirror-idx (list 1 2 3 4) 1))

(= 0 (mirror-idx (list 1 2 3 4) 3))

(defn seq-idx [s x]
  (cond (int? x) (if (neg? x) (mirror-idx s (u/abs (inc x))) x)
        (number? x) (decimal->idx s x)
        (u/random-kw? x) (rand-int (count s))
        (vector? x) (u/rand-int-between (x 0) (x 1))
        :else (u/throw* `seq-idx "expects a number: " x)))

(defn seq-section [s [from to]]
  (let [[i1 i2] (sort (map (partial seq-idx s) [from to]))]
    (->> s
         (take i2)
         (drop i1))))

(defn member
  "Find or pick an element within a sequence 's.
   available forms:
   (member s <integer>) normal nth like get
   (member s <negative-integer>) nth from the end of the list
   (member s <float-or-rational>) a non integer between -1 and 1, is picking a member relatively to the length of the list, forward if positive, backward if negative.
   (member s <[min max]>) picks a member randomly between the given idxs (every type of index allowed)
   (member s <:rand|:random>) picks a random member"
  [s x]
  (cond (number? x) (nth s (seq-idx s x))
        (or (u/random-kw? x) (nil? x)) (nth s (rand-int (count s)))
        (vector? x) (if (= 2 (count x))
                      (rand-nth (seq-section s x))
                      (u/throw* `member "expected a vector of two elements and got: " x))
        (fn? x) (x s)
        :else (u/throw* `member "unexpected argument: " x)))

(do :member-tries
    (member (range 6) [1/4 -1/4])
    (seq-section (range 6) [1/4 -1/4])
    (member (range 6) -1/4)
    (member (range 6) 1/4)
    (member (range 6) -1))

;; transformations -----

(defn factors [x]
  (-> (u/factorize x)
      next
      c/subsets
      butlast
      (->> (map (partial apply *))
           distinct)))

(defn rotations [x]
  (map #(rotate x %)
       (range 1 (count x))))

(defn rotation [s x]
  (rotate s (seq-idx s x)))

(defn partitions [x]
  (reverse
    (map #(partition % x)
         (factors (count x)))))

(defn simple-subseqs [x]
  (mapcat identity (butlast (partitions x))))

(defn simple-permutations
  "lazy seq of permutations
   by increasing complexity"
  [x]
  (->> (butlast (partitions x))
       (mapcat c/permutations)
       (map #(mapcat identity %)) ;flat individual results
       distinct))

;; permutations ------------------

(def MAX_GRADE 9)

(def idx-permutations
  (memoize
   (fn [n]
     (c/permutations (range n)))))

(defn permutation-grades
  "return a seq of available permutation grades for the given seq"
  [s]
  (range (min (count s) (inc MAX_GRADE))))

(defn default-split-sizes
  "default available split sizes for the given sequence"
  [s]
  (range 1 (inc (count s))))

(defn splits
  "takes a seq 's and a number of splits 'n
   return all possible splits of 's of size 'n"
  ([s n]
   (splits s n (range 1 (inc (count n)))))
  ([s n sizes]
   (->> (u/sums (count s) n sizes)
        reverse
        (mapcat c/permutations)
        (map (fn [xs] (loop [ret [] s s xs xs]
                       (if-let [[x & xs] (seq xs)]
                         (recur (conj ret (take x s)) (drop x s) xs)
                         ret)))))))

(def split-permutations
  (letfn [(primary? [s]
            (or (not (next s))
                (and  (not (= (inc (first s)) (second s)))
                      (primary? (rest s)))))]
    (memoize
     (fn [n-splits]
       (filter primary? (c/permutations (range n-splits)))))))

(defn grade-permutations
  ([s n]
   (grade-permutations s n (default-split-sizes s)))
  ([s n split-sizes]
   (let [split-perms (split-permutations (inc n))]
     (mapcat (fn [split]
               (map (fn [sp] (mapcat split sp))
                    split-perms))
             (splits s (inc n) split-sizes)))))

(defn gradual-permutations
  "take a sequence s to permute,
   returns a grade->permutations map."
  ([s]
   (gradual-permutations s (default-split-sizes s)))
  ([s split-sizes]
   (reduce
    (fn [ret n]
      (assoc ret n (grade-permutations s n split-sizes)))
    {}
    (permutation-grades s))))

(defn permutation
  ([s]
   (shuffle s))
  ([s idx]
   (if (u/random-kw? idx)
     (shuffle s)
     (permutation s idx {})))
  ([s idx {:keys [grade split-bounds split-sizes min-split max-split]}]
   (let [cnt (count s)
         split-sizes
         (cond split-sizes split-sizes
               split-bounds (range (split-bounds 0) (inc (min cnt (split-bounds 1))))
               min-split (range min-split (inc cnt))
               max-split (range 1 (inc (min cnt max-split)))
               :else (range 1 (inc cnt)))]
     (if grade
       (let [grade (member (permutation-grades s) grade)]
         (member (grade-permutations s grade split-sizes)
                 idx))
       (member (reduce concat (vals (gradual-permutations s split-sizes)))
               idx)))))

(comment :tries

  (do :misc
      (idx-permutations 4)
      (take 10 (simple-permutations (range 8)))
      (partitions '(1 2 3 4 5 6))
      (simple-subseqs '(0 1 2 3 4 5))
      (splits (range 10) 3)
      (u/sums 10 3 (range 1 10)))

  (split-permutations 1)
  (grade-permutations [0 1 2 3 4 5] 3 [2 4])

  (count (c/permutations (range 6)))
  (count (gradual-permutations (range 6)))
  (member (range 6) 0)
  (default-split-sizes (range 5))
  (permutation (range 6) 0)
  (permutation (range 6) :rand {:grade 2}))
