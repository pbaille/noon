(ns noon.utils.contour
  "an experiment around simple int sequence development"
  (:require [clojure.math.combinatorics :as c]
            [noon.utils.misc :as u]
            [noon.utils.sequences :as s]))

(defn bounds [s]
  [(apply min s)
   (apply max s)])

(defn size [s]
  (let [[min max] (bounds s)]
    (- max min)))

(defn contour [s]
  (let [sorted-set (sort (set s))
        mappings (map vector sorted-set (range))]
    (mapv (into {} mappings)
          s)))

(defn contour-inversions [x]
  (let [size (apply max x)]
    (map (fn [i]
           (mapv (fn [v] (mod (+ i v) (inc size))) x))
         (range (inc size)))))

(defn contour-mirror [x]
  (let [max (apply max x)]
    (mapv (fn [x] (- (- x max))) x)))

(defn- contour-growths
  [contour size]
  (let [contour-size (apply max contour)
        space (- size contour-size)
        updates (mapcat c/permutations
                        (u/sums space contour-size (range 0 (inc space))))
        indexed-contour (map-indexed vector contour)
        idx->shift-idxs (mapv (fn [v]
                                (keep (fn [[i v2]] (if (>= v2 v) i))
                                      indexed-contour))
                              (sort (set contour)))]
    (map (fn [u]
           (reduce (fn [ret [idx shift]]
                     (reduce (fn [ret i] (update ret i + shift))
                             ret (idx->shift-idxs (inc idx))))
                   contour
                   (map-indexed vector u)))
         updates)))

(defn lines
  "given a contour returns a seq of increasingly wide lines following it"
  [contour growth]
  (let [base-size (apply max contour)]
    (cond (int? growth)
          (mapcat (partial contour-growths contour)
                  (range base-size (inc (+ base-size growth))))
          (vector? growth)
          (mapcat (partial contour-growths contour)
                  (map (partial + base-size) (range (growth 0) (inc (growth 1)))))
          )))

(defn similars [s extent]
  (cond
    (int? extent)
    (let [contour (contour s)
          size (size contour)]
      (contour-growths contour (+ size extent)))
    (vector? extent)
    (let [[emin emax] extent
          contour (contour s)
          contour-size (size contour)
          size (size s)
          max-size (+ emax size)
          min-size (+ emin size)]
      (mapcat (partial contour-growths contour)
              (range (max contour-size min-size) (inc max-size))))
    :else (u/throw* `similars
                    "unexpected 2nd argument: "
                    extent)))

(defn gen-contour
  "produce a contour vector of length 'x and height 'y"
  ([x]
   (cond (int? x) (gen-contour x x)
         (vector? x) (gen-contour (x 0) (x 1))))
  ([x y]
   (let [base (range y)]
     (->> (concat base (cycle base))
          (take x)
          s/shuffle-no-rep
          vec))))

(defn gen-line
  "generate a line"
  [{:keys [contour grow pick]}]
  (-> (gen-contour contour)
      (lines grow)
      (s/member pick)))

(comment :gen-line
         (gen-line {:contour [6 4] :grow [0 3]})

         (lines (vec (gen-contour [6 4])) 6))

(comment :tries
         (defn summary [s]
           (let [bounds (bounds s)
                 size (size s)
                 contour (contour s)]
             {:self s
              :bounds bounds
              :size size
              :contour contour
              :mirror-contour (contour-mirror contour)
              :contour-inversions (contour-inversions contour)
              :similars (similars s [10 10])}))
         (contour [1 5 3 1 5 9 -1])
         (contour-growths (contour [1 3 2 3]) 10)

         (let [total 4
               size 6
               steps [-2 -1 1 2 3]
               sums (u/sums total size steps)]
           (assert (and (every? (fn [x] (= total (apply + x))) sums)
                        (every? (fn [x] (= size (count x))) sums)
                        (every? (set steps) (flatten sums))
                        (= (map sort sums) sums)
                        (= (count sums) (count (set sums)))))
           sums)

         (let [ret (lines [0 2 1 2 3] 6)]
           (assert (and (= (count ret) (count (set ret)))
                        (apply = (map contour ret))))
           ret)

         (contour-mirror [0 2 1 2 4 3])

         (contour-inversions [0 2 1 2 4 3])

         (similars [1 5 3 1 5 9 -1] [-2 2])
         (summary [1 5 3 1 5 9 -1])

         (similars [0 2 4 3] [-1 2])
         (expansions [0 2 3 1] 2)
         (expansions [0 2 3 1] [2 4])
         (shrinks [0 2 5 3] 2)
         (shrinks [0 2 5 3] [1 2]))
