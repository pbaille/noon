(ns noon.score
  "A score is a collection of events, represented using a clojure set."
  (:refer-clojure :exclude [iter])
  (:require [clojure.core :as c]
            [noon.harmony :as h]
            [noon.events :as events]
            [noon.utils.misc :as u :refer [t t?]]
            [noon.utils.chance :as g]
            [noon.numbers :as numbers])
  #?(:cljs (:require-macros [noon.score :refer [sfn sf_ e->s]])))

(def score0 #{events/DEFAULT_EVENT})

(defn score
  "Build a score from `x`.
   `x` can be either:
    - a set of event maps.
    - a single event map.
    - nil (empty score)
    - a generator that will be realized and fed into this function again."
  [x]
  (cond (set? x) x
        (map? x) #{x}
        (g/gen? x) (score (x))
        (nil? x) #{}
        :else (u/throw* "noon.score/score :: bad argument : " x)))

(defn score?
  "Test if `x` is a score."
  [x]
  (if (set? x)
    (every? map? x)))

(do :views

    "Basic things we may want to know about a score."

    (defn score-duration
      [score]
      (or (->> (map (fn [e]
                      (c/+ (:duration e)
                           (:position e)))
                    score)
               sort
               last)
          0))

    (defn score-track-count
      "How many tracks has this score ?"
      [score]
      (inc (apply max (map #(:track % 0) score))))

    (defn score-bounds
      "Based on a given note dimension k,
           returns the min and max notes of a score."
      [score k]
      (let [sorted (sort-by k (filter k score))]
        [(first sorted) (last sorted)]))

    (defn score-origin
      "Get the position of the first event of a score."
      [score]
      (-> (map :position score)
          sort first))

    (defn pitch-value-bounds
      "Return a vector of min and max chromatic pitch values of the given score."
      [x]
      (let [ps (map events/pitch-value x)]
        [(apply min ps) (apply max ps)])))

(do :transformations

    "Some score transformation helpers, low level building blocks used in score-updates definitions."

    (defn map-event-update
      "Apply `event-update` to each event of `score`. If `event-update` returns nil for an event, it is removed from the resulting score."
      [score event-update]
      (set (keep event-update score)))

    (defn scale-score
      "Scale score timing by given ratio."
      [score ratio]
      (map-event-update score
                        (events/map->efn {:duration (numbers/mul ratio)
                                          :position (numbers/mul ratio)})))

    (defn shift-score
      "Shift all position by the given offset."
      [score offset]
      (map-event-update score
                        (events/map->efn {:position (numbers/add offset)})))

    (defn fit-score
      "Fit a score into a note scaling and shifting it
       to match the position and length of the given note."
      [score {:keys [duration position]}]
      (let [current-duration (score-duration score)
            scaled (if duration
                     (scale-score score
                                  (c// duration current-duration))
                     score)]
        (if position
          (shift-score scaled position)
          scaled)))

    (defn normalise-score
      "Normalise score to {:position 0 :duration 1}"
      [score]
      (fit-score score
                 {:position 0 :duration 1}))

    (defn reverse-score
      "Reverse score temporally."
      [score]
      (let [total-duration (score-duration score)]
        (map-event-update score
                          (fn [e]
                            (assoc e :position
                                   (c/- total-duration
                                        (:position e)
                                        (:duration e)))))))

    (defn filter-score [score f]
      (set (filter (events/->event-matcher f) score)))

    (defn trim-score
      "Removes everything before `beg` and after `end` from `score`.
       (triming overlapping durations)."
      [score beg end]
      (map-event-update score
                        (fn [{:as evt :keys [position duration]}]
                          (let [end-pos (+ position duration)]
                            (cond (or (>= position end)
                                      (<= end-pos beg)) nil
                                  (and (>= position beg) (<= end-pos end)) evt
                                  :else (cond-> evt
                                          (> end-pos end)
                                          (-> (update :duration - (- end-pos end))
                                              (assoc :trimed-fw true))
                                          (< position beg)
                                          (-> (update :position + (- beg position))
                                              (update :duration - (- beg position))
                                              (assoc :trimed-bw true))))))))

    (do :midi-prepare

        (defn numerify-pitches
          "Replace the pitch entry value of each event by its MIDI pitch value (7bits natural)."
          [score]
          (map-event-update score (fn [e] (update e :pitch h/hc->chromatic-value))))

        (defn dedupe-patches-and-control-changes
          "Remove redondant :patch and :cc event entries from `score`"
          [score]
          (->> (group-by (juxt :track :channel) score)
               (map (fn [[_ xs]]
                      (loop [ret #{}
                             current-patch nil
                             current-control-changes nil
                             todo (sort-by :position xs)]
                        (if-let [[x & todo] (seq todo)]
                          (let [same-patch (= current-patch (:patch x))
                                same-control-changes (= current-control-changes (:cc x))]
                            (cond (and same-patch same-control-changes)
                                  (recur (conj ret (dissoc x :cc :patch))
                                         current-patch
                                         current-control-changes
                                         todo)
                                  same-patch
                                  (recur (conj ret (dissoc x :patch))
                                         current-patch
                                         (:cc x)
                                         todo)
                                  same-control-changes
                                  (recur (conj ret (dissoc x :cc))
                                         (:patch x)
                                         current-control-changes
                                         todo)
                                  :else
                                  (recur (conj ret x)
                                         (:patch x)
                                         (:cc x)
                                         todo)))
                          ret))))
               (reduce into #{})))

        (defn dedup-tempo-messages
          "NOT USED YET, TO VERIFY"
          [score]
          (->> (group-by :position score)
               (sort-by key)
               (map (fn [[_ xs]]
                      (assert (apply = (map :bpm xs))
                              "all events at a given position must have same bpm")
                      (let [[x & xs] (sort-by (juxt :track :channel) xs)]
                        (cons x (map (fn [e] (dissoc e :bpm)) xs)))))
               (reduce into #{})))))

(do :sorting-grouping

    (defn sort-score
      "Sort `score` events.
       arity 1: by :position.
       arity 2: by `f`
       arity 3: by `f` using `comp` as compare fn."
      ([score] (sort-score :position score))
      ([f score] (sort-by f score))
      ([f comp score] (sort-by f comp score)))

    (defn chunk-score
      "Chunk `score` using the `by` function, return a sorted (accordingly to `by`) sequence of subscores."
      [score by]
      (map (comp set val)
           (sort (group-by by score)))))

(do :composition

    (defn merge-scores
      "Merge several scores together."
      [scores]
      (reduce into #{} scores))

    (defn concat-score
      "Concat 2 scores temporally."
      [a b]
      (->> (score-duration a)
           (shift-score b)
           (into a)))

    (defn concat-scores
      "Concat several scores temporally."
      [xs]
      (case (count xs)
        0 #{}
        1 (first xs)
        (reduce concat-score xs))))

(do :show

    (defn show
      "Look at a score using hierarchical grouping."
      ([score]
       (show [:track :channel :position] score))
      ([path score]
       (letfn [(assoc' [x k v]
                 (if (and v (not-empty v)) (assoc x k v) x))
               (extract-common-top-level [xs]
                 (let [common (into {}
                                    (filter (fn [[k v]]
                                              (every? (partial = v) (map k xs)))
                                            (first xs)))
                       common-keys (keys common)]
                   [common (mapv (fn [x] (apply dissoc x common-keys)) xs)]))
               (extract-pitch-common [[common xs]]
                 (let [[pcommon ps] (extract-common-top-level (map :pitch xs))]
                   [(assoc' common :pitch pcommon)
                    (map (fn [x p]
                           (assoc' x :pitch p))
                         xs ps)]))
               (extract-common [xs]
                 (-> xs extract-common-top-level extract-pitch-common))
               (by [k xs]
                 (let [[common xs] (extract-common xs)]
                   (->> (group-by k xs)
                        (map (fn [[v xs]] [v (mapv #(dissoc % k) xs)]))
                        (into (sorted-map))
                        (vector common k))))
               ($vals [f [common k xs]]
                 [common k (->> (mapv (fn [[k v]] [k (f v)]) xs) (into (sorted-map)))])
               (go [path xs]
                   (if (seq path)
                     ($vals (partial go (next path))
                            (by (first path) xs))
                     xs))]
         (let [[common xs] (extract-common score)]
           [common
            (go (remove (set (keys common)) path)
                xs)]))))

    (comment

      (show
       (mk (chans [(patch :piano) (tup s0 s1 s2)]
                  [(patch :vibraphone) (lin s0 s1 (par d0 d1 d2))]))))

    (defn qshow
      "Raw scores are hard to parse for a human,
       this is intended to help a bit."
      [score]
      (mapv (fn [{:as event :keys [position duration]}]
              (into [(events/pitch-value event) duration position]))
            (sort-score :position score))))

(do :score-update

    "A score-update is a function that takes a score and return a score."

    (defmacro sfn
      "Just a tagged lambda that represents a score update function."
      {:tags [:syntax :score-update]}
      [arg & body]
      `(t :score-update
          (fn [~arg] ~@body)))

    (defmacro sf_
      {:tags [:syntax :score-update]}
      [& body]
      `(sfn ~'_ ~@body))

    (def score-update? (t? :score-update))

    (declare chain-score-updates)

    (defn ->score-update
      "Turn 'x into a score-update if possible."
      [x]
      (if-let [event-update (events/->event-update x)]
        (sf_ (map-event-update _ event-update))
        (cond (score-update? x) x
              (vector? x) (chain-score-updates x)
              (g/gen? x) (if (->score-update (g/realise x))
                           (sf_ ((->score-update (g/realise x)) _))))))

    (defn ->score-update!
      "Strict version of `noon.score/->score-update`, it throws if `x` is not convertible."
      [x]
      (or (->score-update x)
          (u/throw* `->score-update! " not convertible: " x)))

    (defn ->score-checker
      "Convert `x` to a score-checker if possible.
       A score-checker is a score-update that can return the score unchanged or nil indicating failure."
      [x]
      (if (or (events/event-matcher? x)
              (events/event-update? x)
              (map? x))

        (sfn s
             (if (every? (events/->event-matcher x) s) s))

        (if-let [update (->score-update x)]
          (sfn s (let [s' (update s)]
                   (if (and s' (not= #{} s'))
                     s)))
          (if (fn? x)
            (sfn s (if (x s) s))))))

    (defn ->score-checker!
      "Strict version of `noon.score/->score-checker`"
      [x]
      (or (->score-checker x)
          (u/throw* `->score-checker! " not convertible: " x)))

    (defn chain-score-updates [updates]
      (if-let [updates (u/?keep ->score-update updates)]
        (sf_ (u/?reduce #(%2 %1) _ updates))
        (u/throw* `chain-score-updates " bad argument: " updates)))

    (defn map-score-update
      "map `score-update` over `score`.
       - each event of `score` will be converted to a single event score
       - this single event score will be repositioned to zero and updated using `score-update`.
       - all resulting scores will be concatenated into one."
      [score score-update]
      (->> (map (fn [e]
                  (-> (score-update #{(assoc e :position 0)})
                      (shift-score (:position e))))
                score)
           (reduce into #{})))

    (defn wrap-event->score-fn
      "Build a score-update from an event -> score function."
      [f]
      (sfn score (->> (map (fn [e]
                             (-> (f (assoc e :position 0))
                                 (shift-score (:position e))))
                           score)
                      (reduce into #{}))))

    (defmacro e->s
      {:tags [:syntax :score-update]}
      [pat & body]
      `(wrap-event->score-fn (fn [~pat] ~@body)))

    (defn update-score
      "Updates `score` with `update`."
      [score update]
      (if-let [score-update (->score-update update)]
        (score-update score)
        (u/throw* `update-score " bad argument: " update)))

    (defn partial-update
      "Use `event-matcher` to match some events of `score`, apply `update` to the resulting subscore,
       then merge unselected events into the updated subscore.
       see `noon.score/->event-matcher` for exact semantics of event matching."
      [score event-matcher update]
      (let [grouped (group-by (events/->event-matcher event-matcher) score)
            common (set (get grouped false))
            updated (update-score (get grouped true) update)]
        (into common updated)))

    (defn map-update [score update]
      (if-let [event-update (events/->event-update update)]
        (map-event-update score event-update)
        (if-let [score-update (->score-update update)]
          (map-score-update score score-update)
          (u/throw* `map-update " bad argument: " update)))))

(do :creation

    "Main entry point to create a score."

    (defn mk*
      "Feed score0 into given updates."
      {:tags [:base :score-builder]}
      [updates]
      ((chain-score-updates updates) score0))

    (defn mk
      {:tags [:base :score-builder]}
      [& updates]
      (mk* updates)))
