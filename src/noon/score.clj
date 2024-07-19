(ns noon.score
  "build, transform, play and write midi scores"
  (:refer-clojure :exclude [cat while struct])
  (:require [clojure.core :as c]
            [clojure.pprint :refer [pprint]]
            [clojure.java.shell :as shell]
            [noon.midi :as midi]
            [noon.harmony :as h]
            [noon.vst.index :as vst]
            [noon.constants :as constants]
            [noon.utils.misc :as u :refer [t t? f_ defn*]]
            [noon.utils.mapsets :as ms]
            [noon.utils.maps :as m]
            [noon.utils.chance :as g]
            [noon.utils.pseudo-random :as pr]))

(do :help

    (defn pp [& xs]
      (mapv pprint xs)
      (last xs))

    (defmacro dbg [& xs]
      `(do (println '------)
           (println '~&form)
           (pp ~@xs)))

    (defn sub [x] (f_ (- _ x)))
    (defn add [x] (f_ (+ _ x)))
    (defn mul [x] (f_ (* _ x)))
    (defn div [x] (f_ (/ _ x)))

    (defn eq [x] (f_ (= _ x)))
    (defn gt [x] (f_ (> _ x)))
    (defn lt [x] (f_ (< _ x)))
    (defn gte [x] (f_ (>= _ x)))
    (defn lte [x] (f_ (<= _ x)))

    (def hm* (partial apply hash-map))

    (defn ?reduce
      "like reduce but short-circuits (returns nil) on first falsy result"
      [f init xs]
      (reduce (fn [a e]
                (or (f a e) (reduced nil)))
              init xs))

    (defn ->int
      "Turn `x` into an integer, rounding it if needed, returning 0 if not a number."
      [x]
      (cond (integer? x) x
            (number? x) (int (Math/round (float x)))
            :else 0))

    (defn ->7bits-natural
      "MIDI often deals with natural between 0 and 127,
       this function coerce its input to this range."
      [x]
      (-> (->int x)
          (max 0)
          (min 127)))

    (defn ->16bits-natural
      "MIDI sometimes deals with 16 bits between values,
       this function coerce its input to this range."
      [x]
      (-> (->int x)
          (max 0)
          (min 65535)))

    (defn ->4bits-natural
      "MIDI sometimes deals with natural between 0 and 16,
       this function coerce its input to this range."
      [x]
      (-> (->int x)
          (max 0)
          (min 15))))

(do :event

    "The 'event is the smallest brick we are dealing with, it represents some kind of MIDI event using a clojure map."

    (def DEFAULT_EVENT
      (assoc midi/DEFAULT_NOTE
             :pitch h/DEFAULT_HARMONIC_CONTEXT
             :voice 0
             :patch [0 4]))

    (defn normalise-event
      "Puts time related dimensions of a note into their identity values.
       Useful in many time streching transformations"
      [x]
      (assoc x :position 0 :duration 1))

    (do :views

        (defn pitch-value
          [e]
          (h/hc->chromatic-value (:pitch e)))

        (defn pitch-class-value
          [e]
          (mod (pitch-value e) 12)))

    (do :event-update

        "An event-update is a function that takes an event and return an event."

        (defmacro efn
          "just a tagged lambda that represents an event update function"
          [arg & body]
          `(t :event-update
              (fn [~arg] ~@body)))

        (defmacro ef_ [& body]
          `(efn ~'_ ~@body))

        (def event-update?
          (t? :event-update))

        (defn map->efn [x]
          (t :event-update (m/->upd x))))

    (do :instances

        (def e0 (t :event-update identity))

        (defn dur
          "Builds a :duration event-update based on `x`."
          {:tags [:event-update :temporal]}
          [x]
          (map->efn {:duration x}))

        (do :velocity
            (defn vel
              "Builds a :velocity event-update based on `x`."
              {:tags [:event-update]}
              [x]
              (ef_ (update _ :velocity
                           (fn [v] (->7bits-natural (m/value-merge v x))))))

            (defn vel+
              "Builds an event update that adds `n` to :velocity value"
              {:tags [:event-update]}
              [n] (vel (add n)))

            (defn vel-
              "Builds an event update that substract `n` to :velocity value"
              {:tags [:event-update]}
              [n] (vel (sub n)))

            (def vel0 (vel 0)))

        (do :channel

            (defn chan
              "Builds a :velocity event-update based on `x`."
              {:tags [:event-update]}
              [x]
              (ef_ (update _ :channel
                           (fn [v] (->4bits-natural (m/value-merge v x))))))

            (defn chan+ [x] (chan (add x)))
            (defn chan- [x] (chan (sub x))))

        (do :tracks
            (defn track [x]
              (ef_ (update _ :track
                           (fn [t] (->16bits-natural (m/value-merge t x))))))

            (defn track+ [x] (track (add x)))
            (defn track- [x] (track (sub x))))

        (do :voice

            " incubation "

            (defn voice [x]
              (ef_ (update _ :voice
                           (fn [v] (->4bits-natural (m/value-merge v x))))))

            (defn voice+ [x] (voice (add x)))
            (defn voice- [x] (voice (sub x))))

        (defn cc [key val]
          (if-let [code (midi/cc-code key)]
            (map->efn {:cc {code (fn [v] (->7bits-natural (m/value-merge v val)))}})
            (u/throw* "Unrecognised control change code: " key)))

        (defn patch
          ([x]
           (cond (keyword? x) (patch (vst/pick x))
                 (vector? x) (ef_ (assoc _ :patch x))
                 (number? x) (patch nil x)
                 :else (u/throw* "noon.score/patch :: bad argument " x)))
          ([bank program]
           (patch [bank program])))

        (do :var-definitions

            "defines some vars shorthands for duration, velocity, channel and track updates."

            ;; defines some duration update vars
            ;; d2 ... d11 to multiply it
            ;; d:2 ... d:11 to divide it
            (defmacro -def-durations []
              (cons 'do
                    (concat (for [i (range 2 12)]
                              (list 'do
                                    (list 'def (with-meta (symbol (str "dur" i))
                                                 {:doc (str "Multiply event duration by " i)
                                                  :tags [:event-update :alias :temporal]})
                                          `(dur (mul ~i)))
                                    (list 'def (with-meta (symbol (str "dur:" i))
                                                 {:doc (str "Divide event duration by " i)
                                                  :tags [:event-update :alias :temporal]})
                                          `(dur (div ~i)))))
                            (for [n (range 2 12)
                                  d (range 2 12)]
                              (list 'def (with-meta (symbol (str "dur" n ":" d))
                                           {:doc (str "Multiply event duration by " n "/" d)
                                            :tags [:event-update :alias :temporal]})
                                    `(dur (mul (/ ~n ~d))))))))
            (-def-durations)

            ;; defines 12 levels of velocity from 10 to 127
            ;; as v1 ... v12
            (defmacro -def-velocities []
              (cons 'do
                    (for [i (range 1 13)]
                      (let [v (int (* i (/ 127 12)))]
                        (list 'def (with-meta (symbol (str "vel" i))
                                     {:doc (str "Set event velocity to " v)
                                      :tags [:event-update :alias]})
                              `(vel ~v))))))
            (-def-velocities)

            (defmacro -def-channels []
              (cons 'do
                    (for [i (range 0 16)]
                      (list 'def (with-meta (symbol (str "chan" i))
                                   {:doc (str "Set event midi channel to " i)
                                    :tags [:event-update :alias]})
                            `(chan ~i)))))
            (-def-channels)

            (defmacro -def-tracks []
              (cons 'do
                    (for [i (range 0 16)]
                      (list 'def (with-meta (symbol (str "track" i))
                                   {:doc (str "Set event midi channel to " i)
                                    :tags [:event-update :alias]})
                            `(track ~i)))))
            (-def-tracks))

        (do :pitch

            "Wraps noon.harmony functionality under the :pitch key of events."

            (defmacro import-wrap-harmony-update-constructors [& xs]
              `(do ~@(map (fn [x]
                            (let [original-sym (symbol "noon.harmony" (name x))]
                              `(defn ~x
                                 ~(str "Build an harmonic event-update using " original-sym
                                       "\n  The resulting transformation will be used to update the :pitch value of the received event.\n\n"
                                       "  " original-sym
                                       "\n\n  arglists:\n\n  "
                                       (:arglists (meta (resolve original-sym)))
                                       "\n\n  doc:\n\n  "
                                       (:doc (meta (resolve original-sym))
                                             "undocumented"))
                                 {:tags [:event-update :alias :harmonic]}
                                 [~'& xs#]
                                 (let [u# (apply ~(symbol "noon.harmony" (name x)) xs#)]
                                   #_(println '~x xs#)
                                   (map->efn
                                    {:pitch
                                     (fn [ctx#]
                                       #_(println ctx#)
                                       (h/upd ctx# u#))})))))
                          xs)))

            (defmacro import-wrap-harmony-updates [& xs]
              `(do ~@(map (fn [x]
                            (let [original-sym (symbol "noon.harmony" (name x))]
                              (list 'def (with-meta x
                                           {:doc (str "Updates the :pitch value of the received event using "
                                                      original-sym
                                                      "\n\ndoc:\n\n"
                                                      (:doc (meta (resolve original-sym))
                                                            "undocumented"))
                                            :tags [:event-update :alias :harmmonic]})
                                    `(map->efn
                                      {:pitch
                                       (fn [ctx#]
                                         #_(println ctx#)
                                         (h/upd ctx# ~(symbol "noon.harmony" (name x))))}))))
                          xs)))

            (import-wrap-harmony-update-constructors
             ;; positions
             position s-position d-position c-position

             ;; intervals
             t-step s-step d-step c-step
             t-shift s-shift d-shift c-shift
             layer-step layer-shift

             ;; context tweaks
             origin scale struct degree root inversion
             repitch rescale restruct reorigin reroot redegree)

            (import-wrap-harmony-updates
             t-round t-ceil t-floor
             s-round s-ceil s-floor
             d-round d-ceil d-floor
             s+ s-)

            (defn transpose
              "Transpose the pitch origin of all events by the given update."
              {:tags [:event-update :harmonic]}
              [f]
              (assert (event-update? f) "transpose only takes event-update")
              (ef_ (let [new-origin (h/hc->pitch (:pitch (f ((position 0) _))))]
                     (assoc-in _ [:pitch :origin] new-origin))))

            (defn rebase
              "Applies the given transformations while preserving pitch."
              {:tags [:event-update :harmonic]}
              [& fs]
              (ef_
               (reduce #(%2 %1) _
                       (conj (vec fs)
                             (repitch (h/hc->pitch (:pitch _)))))))

            (do :defs

                (defmacro -def-wrapped [wrapper m]
                  (cons 'do (for [[k v] (eval m)]
                              (list 'def
                                    (with-meta (symbol (name k))
                                      {:tags [:event-update :alias :harmonic]
                                       :doc (str "Alias for " (list (symbol "noon.score" (name wrapper)) v))})
                                    (list wrapper v)))))

                (-def-wrapped struct noon.constants/structs)

                (-def-wrapped scale noon.constants/modes)

                (-def-wrapped repitch noon.constants/pitches)

                (do :intervals

                    {:chromatic 'c
                     :diatonic 'd
                     :structural 's
                     :tonic 't
                     :octave 'o}

                    (def c0 (c-step 0))
                    (def d0 (d-step 0))
                    (def s0 (s-step 0))
                    (def t0 (t-step 0))

                    (defmacro -def-steps [name prefix max f]
                      (cons 'do
                            (mapcat
                             (fn [n]
                               [(list 'def (with-meta (symbol (str prefix n))
                                             {:doc (str "Step up "
                                                        n " " name " " (if (> n 1) "steps" "step") ".")
                                              :tags [:event-update :harmonic]})
                                      (list f n))
                                (list 'def (with-meta (symbol (str prefix n "-"))
                                             {:doc (str "Step down " n " " name " " (if (> n 1) "steps" "step") ".")
                                              :tags [:event-update :harmonic]})
                                      (list f (list `- n)))])
                             (range 1 max))))

                    (-def-steps "chromatic" "c" 37 c-step)
                    (-def-steps "diatonic" "d" 22 d-step)
                    (-def-steps "structural" "s" 13 s-step)
                    (-def-steps "tonic" "t" 13 t-step)

                    (defmacro -def-shifts [name prefix max f]
                      (cons 'do
                            (mapcat
                             (fn [n]
                               [(list 'def (with-meta (symbol (str prefix n))
                                             {:doc (str "Shift up "
                                                        n " " name (when (> n 1) "s") ".")
                                              :tags [:event-update :harmonic]})
                                      (list f n))
                                (list 'def (with-meta (symbol (str prefix n "-"))
                                             {:doc (str "Shift down " n " " name (when (> n 1) "s") ".")
                                              :tags [:event-update :harmonic]})
                                      (list f (list `- n)))])
                             (range 1 max))))

                    (-def-shifts "octave" "o" 9 (fn [i] (t-shift i :forced))))

                (defmacro -def-degrees []
                  (cons 'do
                        (concat (for [[n v] (map vector '[I II III IV V VI VII] (range))]
                                  (list 'def (with-meta n
                                               {:doc (str "Go to degree " n)
                                                :tags [:event-update :harmonic]})
                                        (degree v)))
                                (for [[degree-sym degree-val] (map vector '[I II III IV V VI VII] (range))
                                      [alteration-sym alteration-val] [["#" c1] ["b" c1-]]]
                                  (let [[dn dv an av] [degree-sym degree-val alteration-sym alteration-val]]
                                    (list 'def (with-meta (symbol (str dn an))
                                                 {:doc (str "Go to degree " an dn)
                                                  :tags [:event-update :harmonic]})
                                          `[(transpose ~av) (degree ~dv)]))))))

                (-def-degrees)))))

(do :score

    "A score is a collection of events, represented using a clojure set."

    (def score0 #{DEFAULT_EVENT})

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
          (let [ps (map pitch-value x)]
            [(apply min ps) (apply max ps)])))

    (do :transformations

        "Some score transformation helpers, low level building blocks used in score-updates definitions."

        (defn scale-score
          "Scale score timing by given ratio."
          [score ratio]
          (ms/$ score
                {:duration (mul ratio)
                 :position (mul ratio)}))

        (defn shift-score
          "Shift all position by the given offset."
          [score offset]
          (ms/$ score
                {:position (add offset)}))

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
          [score]
          (fit-score score
                     {:position 0 :duration 1}))

        (defn concat-score [a b]
          (->> (score-duration a)
               (shift-score b)
               (into a)))

        (defn concat-scores [xs]
          (case (count xs)
            0 #{}
            1 (first xs)
            (reduce concat-score xs)))

        (defn reverse-score [score]
          (let [total-duration (score-duration score)]
            (ms/$ score
                  (fn [e]
                    (assoc e :position
                           (c/- total-duration
                                (:position e)
                                (:duration e)))))))

        (defn sort-score
          ([score] (sort-score :position score))
          ([f score] (sort-by f score))
          ([f comp score] (sort-by f comp score)))

        (defn numerify-pitches [score]
          (ms/$ score (fn [e] (update e :pitch h/hc->chromatic-value))))

        (defn dedupe-patches-and-program-changes [score]
          (->> (group-by (juxt :track :channel) score)
               (map (fn [[_ xs]]
                      (loop [ret #{}
                             current-patch nil
                             current-program-changes nil
                             todo (sort-by :position xs)]
                        (if-let [[x & todo] (seq todo)]
                          (let [same-patch (= current-patch (:patch x))
                                same-program-changes (= current-program-changes (:pc x))]
                            (cond (and same-patch same-program-changes)
                                  (recur (conj ret (dissoc x :pc :patch))
                                         current-patch
                                         current-program-changes
                                         todo)
                                  same-patch
                                  (recur (conj ret (dissoc x :patch))
                                         current-patch
                                         (:pc x)
                                         todo)
                                  same-program-changes
                                  (recur (conj ret (dissoc x :pc))
                                         (:patch x)
                                         current-program-changes
                                         todo)
                                  :else
                                  (recur (conj ret x)
                                         (:patch x)
                                         (:pc x)
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
               (reduce into #{}))))

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
                      [(patch :vibraphone) (cat s0 s1 (par d0 d1 d2))]))))

        (defn qshow
          "Raw scores are hard to parse for a human,
           this is intended to help a bit."
          [score]
          (mapv (fn [{:as event :keys [position duration]}]
                  (into [(pitch-value event) duration position]))
                (sort-score :position score))))

    (do :score-update

        "A score-update is a function that takes a score and return a score."

        (declare ->upd)

        (defmacro sfn
          "Just a tagged lambda that represents a score update function."
          [arg & body]
          `(t :score-update
              (fn [~arg] ~@body)))

        (defmacro sf_ [& body]
          `(sfn ~'_ ~@body))

        (def score-update? (t? :score-update))

        (defn upd
          "Updates score 's with update 'x."
          [s x] ((->upd x) s))

        (defn partial-upd
          "Use 'filt to match some events of the score 's, apply 'x to the resulting subscore,
           then merge unselected events into the updated subscore."
          [s filt x]
          (ms/split-upd s filt (->upd x)))

        (defn partial-upd2
          "Use 'filt to match some events of the score 's, apply 'x to the resulting subscore,
           then merge unselected events into the updated subscore.
           This second version allows you to provide an event update as a filter.
           If the result of the update is equal to the original event, it is considered a match."
          [s filt x]
          (ms/split-upd s
                        (if (event-update? filt)
                          (fn [evt] (= evt (filt evt)))
                          filt)
                        (->upd x)))

        (do :casting

            "Casting various clojure's values to score-updates or event-updates."

            (declare lin* par*)

            (defn ->upd
              "Turn 'x into a score-function."
              [x]
              (cond (score-update? x) x
                    (g/gen? x) (sf_ ((->upd (g/realise x)) _))
                    (event-update? x) (sf_ (ms/$ _ x))
                    (map? x) (->upd (map->efn x))
                    (vector? x) (lin* x)
                    (set? x) (par* x)
                    :else (u/throw* "->upd/bad-argument: " x)))

            (defn ->event-upd
              "Turn 'x into an event-upd (used in '$)."
              [x]
              (cond (event-update? x) x

                    (map? x) (map->efn x)

                    (g/gen? x)
                    (ef_ ((->event-upd (g/realise x)) _))

                    (score-update? x)
                    (ef_ (-> ((->upd x) #{(assoc _ :position 0)})
                             (shift-score (:position _))))

                    (or (vector? x)
                        (set? x)) (->event-upd (->upd x))

                    :else (u/throw* "->event-upd/bad-argument: " x))))))

(do :creation

    "Main entry point to create a score."

    (defn mk*
      "Feed score0 into given updates."
      [xs]
      (upd score0 (lin* xs)))

    (defn mk [& xs]
      (mk* xs)))

(do :updates

    "The main forms you will use inside 'mk or 'play."

    (def ^{:doc "Identity transformation"
           :tags [:base]}
      same
      (sf_ _))

    (def ^{:doc "Identity transformation"
           :tags [:base]}
      _ same)

    (defn* k
      "Act like 'mk, ignoring current score."
      {:tags [:base]}
      [xs]
      (sf_ (mk* xs)))

    (def ^{:doc "Returns the empty score regardless of input."
           :tags [:base]}
      void
      (sf_ #{}))

    (defn* lin
      "Compose several updates together linearly."
      {:tags [:base]}
      [xs]
      (sf_ (?reduce upd _ xs)))

    (defn* par
      "Apply several update on a score merging the results."
      {:tags [:base :parallel]}
      [xs]
      (sf_ (ms/mk (map #(upd _ %) xs))))

    (defn* par>
      "Accumulative 'par."
      {:tags [:accumulative :parallel]}
      [xs]
      (sf_ (loop [segments [_] xs xs]
             (if-let [[x & xs] xs]
               (recur (conj segments (upd (peek segments) x)) xs)
               (reduce into #{} (next segments))))))

    (defn* $
      "Apply an update to each events of a score."
      {:tags [:base :iterative]}
      [xs]
      (sf_ (?reduce (fn [s x] (ms/$ s (->event-upd x)))
                    _ xs)))

    (defn* cat
      "Feed each transformations with the current score and concatenate the results."
      {:tags [:base :linear]}
      [xs]
      (sfn score
           (concat-scores
            (map (f_ (upd score _)) xs))))

    (defn* cat>
      "Accumulative 'cat."
      {:tags [:base :linear :accumulative]}
      [xs]
      (sf_ (loop [segments [_] xs xs]
             (if-let [[x & xs] xs]
               (recur (conj segments (upd (peek segments) x)) xs)
               (concat-scores (next segments))))))

    (defn* fit
      "Wraps the given transformation 'x, stretching its output to the input score duration.
       In other words, turn any transformation into another one that do not change the duration of its input score."
      {:tags [:base]}
      [xs]
      (sf_ (fit-score (upd _ (lin* xs))
                      {:duration (score-duration _)})))

    (defn* tup
      "Like 'cat but preserve the length of the input score"
      {:tags [:base :linear]}
      [xs] (fit (cat* xs)))

    (defn* tup>
      "Accumulative 'tup."
      {:tags [:accumulative :linear]}
      [xs] (fit (cat>* xs)))

    (defn* append
      "Like 'cat but insert the current score before."
      {:tags [:base :linear]}
      [xs]
      (cat* (cons same xs)))

    (defn* superpose
      "Like 'par but keep the current score."
      {:tags [:base :parallel]}
      [xs]
      (par* (cons same xs)))

    (defn rep
      "Iterates the given update n times over the input score and cat the results."
      {:tags [:base :linear :accumulative]}
      ([n x]
       (rep n x false))
      ([n x skip-first]
       (sf_ (->> (if skip-first (upd _ x) _)
                 (iterate (->upd x))
                 (take n)
                 (concat-scores)))))

    (defn rup
      "Iterates the given update n times over the input score and tup the results."
      {:tags [:base :linear :accumulative]}
      ([n x]
       (rup n x false))
      ([n x skip-first]
       (fit (rep n x skip-first))))

    (defn dup
      "Duplicate n times and concat the duplicates."
      {:tags [:base :linear :multiplicative]}
      [n]
      (sf_ (concat-scores (repeat n _))))

    (defn dupt
      "Duplicate n times and tup the duplicates."
      {:tags [:base :linear :multiplicative]}
      [n]
      (fit (dup n)))

    (defn tupn
      "Creates a tup of size n using the 'f update."
      {:tags [:base :linear :multiplicative]}
      [n f] (tup* (repeat n f)))

    (defn catn
      "Duplicate n times the score resulting from applying 'f on the current score."
      {:tags [:base :linear :multiplicative]}
      [n f] (cat* (repeat n f)))

    (defn* parts
      "Apply updates to subscores
       (parts sel1 upd1 sel2 upd2 ...)"
      {:tags [:base :partial]}
      [xs]
      (sf_ (reduce (fn [s [filt upd]]
                     (partial-upd2 s filt upd))
                   _ (partition 2 xs))))

    (defn while
      "Iterate the given transformation 'f while 'test is passing."
      {:tags [:base :iterative]}
      ([test f] (while test f same))
      ([test f after]
       (sf_ (let [nxt (upd _ f)]
              (if (not-empty (upd nxt test))
                (recur nxt)
                (upd nxt after))))))

    (defn* fst
      "Tries given transformations in order until the first success (non empty score)."
      {:tags [:base :selective]}
      [xs]
      (sf_ (loop [xs xs]
             (if-let [[x & xs] (seq xs)]
               (or (not-empty (upd _ x))
                   (recur xs))))))

    (defn* fst-that
      "Tries given transformations in order until one passes the given test."
      {:tags [:base :selective]}
      [test fs]
      (fst* (map (f_ (lin _ test))
                 fs)))

    (defn shrink
      "Shrink a score using 'f on each events to determine if it is kept or not."
      {:tags [:base :temporal]}
      [f]
      (sf_ (ms/shrink _ f)))

    (defn adjust
      "Time stretching/shifting operation
       syntax sugar over 'fit-score."
      {:tags [:base :temporal]}
      [x]
      (let [opts (cond (map? x) x
                       (number? x) {:duration x}
                       (vector? x) {:duration (x 1) :position (x 0)})]
        (sf_ (fit-score _ opts))))

    (defn* fork-with
      "Like 'par
       but let you the opportunity to do something
       on the score based on the index of the branch
       before applying corresponding update."
      [f xs]
      (par* (map-indexed (fn [i x] (lin (f i) x))
                         xs)))

    (defn* chans
      "Apply each update in parallel on subsequent midi channels."
      {:tags [:base :parallel]}
      [xs] (fork-with* chan+ xs))

    (defn* tracks
      "Apply each update in parallel on subsequent midi tracks."
      {:tags [:base :parallel]}
      [xs] (fork-with* track+ xs))

    (defn mirror
      "Mirrors all pitches around 'p."
      {:tags [:harmonic]}
      [p] ($ {:pitch (h/mirror p)}))

    (def ^{:doc "Reverse the given score."
           :tags [:temporal]}
      rev
      (sf_ (reverse-score _)))

    (defn event-scale
      "Restrains and scale one event dimension to the given bounds over the whole score."
      [dim x]
      (let [[min-out max-out]
            (cond (number? x) [0 x]
                  (vector? x) x)]
        (sf_ (let [[min-in max-in] (mapv dim (score-bounds _ dim))
                   f #(u/scale-range % min-in max-in min-out max-out)]
               (upd _ ($ (f_ (update _ dim f))))))))

    (do :selection

        (defn min-by [f]
          (sf_ #{(first (sort-by f _))}))

        (defn max-by [f]
          (sf_ #{(last (sort-by f _))}))

        (def min-pitch (min-by pitch-value))

        (def max-pitch (max-by pitch-value))

        (do :time

            "Updates to select time sections of a score."

            (defn from
              "Removes the elements anterior to the given position from the score."
              [x]
              (shrink {:position (gte x)}))

            (defn until
              "Removes the elements posterior to the given position from the score."
              [x]
              (shrink {:position (lt x)}))

            (defn between
              "Keep only events that are positioned between x and y positions."
              [x y]
              (lin (from x) (until y)))

            (defn start-from
              "Shift the score to the given position, removing all anterior events."
              [x]
              (lin (from x) {:position (sub x)}))

            (def start-from-last
              "Shifting the score to last position erasing all anterior events."
              (sf_ (-> (group-by :position _)
                       sort last val set
                       (upd {:position 0}))))

            (defn trim
              "Removes everything before 'beg and after 'end from the score
               (triming overlapping durations)."
              [beg end]
              ($ (efn {:as evt :keys [position duration]}
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
                                          (assoc :trimed-bw true))))))))))

    (do :checks

        (defn within-bounds?
          "Returns score unchanged if 'ef applied to each event is between 'min and 'max."
          [ef min max]
          (sf_ (if (every? (fn [e] (<= min (ef e) max)) _)
                 _)))

        (defn within-time-bounds?
          "Returns the score unchanged if all its events are between 'start and 'end."
          [start end]
          (sf_ (if (and (>= (score-origin _) start)
                        (<= (score-duration _) end))
                 _)))

        (defn within-pitch-bounds?
          "Returns the score unchanged if all pitches are between 'min and 'max.
          'min and 'max should be 'pitchable' (pitch map | pitch keyword | int)."
          [min max]
          (within-bounds? (comp h/hc->chromatic-value :pitch)
                          (:c (constants/get-pitch min))
                          (:c (constants/get-pitch max))))

        (def within-midi-pitch-bounds?
          (within-pitch-bounds? 0 127)))

    (do :non-determinism

        (defmacro !
          "Takes a non deterministic expression resulting in a score update.
           Returns a score update that wraps the expression so that it is evaluated each time the update is called."
          [expr]
          `(vary-meta (sfn score# (upd score# ~expr))
                      assoc :non-deterministic true))

        (defn* one-of
          "Returns an update that choose randomly one of the given updates before applying it."
          [xs]
          (! (pr/rand-nth xs)))

        (defn* maybe
          "Like 'one-of, return an update that choose randomly one of the given updates, but can also do nothing."
          [xs]
          (one-of* (cons same xs)))

        (defn probs
          "Takes a map of type {update number}
           where each key is an update and each value is its probability of occurence."
          [m]
          (let [pm (g/weighted m)]
            (! (pm))))

        (defn* any-that
          "Tries given transformations in random order until one passes the given test."
          [test fs]
          (! (fst-that* test (pr/shuffle fs))))

        (defn* mixtup
          "A tup that mix its elements."
          [xs]
          (tup* (pr/shuffle xs)))

        (defn* shuftup
          "A tup that shuffles its elements everytime it is used."
          [xs]
          (! (mixtup* xs)))

        (defn* mixcat
          "A cat that mix its elements."
          [xs]
          (cat* (pr/shuffle xs)))

        (defn* shufcat
          "A cat that shuffles its elements everytime it is used."
          [xs]
          (! (mixcat* xs)))

        (defn* shuf
          "Shuffles the values of the given dimensions."
          [dims]
          (sf_ (let [size (count _)
                     idxs (range size)
                     mappings (zipmap idxs (pr/shuffle idxs))
                     events (vec _)]
                 (reduce (fn [s i] (conj s (merge (events i) (select-keys (events (mappings i)) dims))))
                         #{}
                         idxs)))))

    (do :lines

        "Updates related to scores representing single melodic lines"

        (def line?
          "Returns the given score if it is a 1 voice line with no holes or superpositions."
          (sf_ (if (and (= (score-duration _)
                           (reduce + (map :duration _)))
                        (let [xs (sort-by :position _)
                              [p1 :as ps] (map :position xs)]
                          (= ps (reductions + p1 (butlast (map :duration xs))))))
                 _)))

        (def legato
          (sf_ (->> (conj _ {:position (score-duration _)})
                    (group-by :position)
                    (sort-by key)
                    (partition 2 1)
                    (reduce (fn [score [[p1 xs] [p2 _]]]
                              (into score (map #(assoc % :duration (- p2 p1)) xs)))
                            #{}))))

        (def shuffle-line
          "If the given score is a line, shuffle the notes."
          (sf_ (if (line? _)
                 (concat-scores
                  (pr/shuffle (map (fn [e] #{(assoc e :position 0)})
                                   _))))))

        (defn permute-line
          [idxs]
          (let [length (count idxs)]
            (sf_ (if (and (line? _) (= length (count _)))
                   (let [notes (vec (sort-by :position _))]
                     (concat-scores
                      (map (fn [e] #{(assoc e :position 0)})
                           (map notes idxs))))))))

        (defn* $cat
          "'mapcat for score, works only on lines."
          [xs]
          (sf_ (if (line? _)
                 (concat-scores
                  (map (fn [e]
                         ((cat* xs) #{(assoc e :position 0)}))
                       (sort-score :position _)))))))

    (do :incubator

        (defn* voices
          "Like 'par but keep track of voice number."
          [xs]
          (fork-with* voice+ xs))

        (defn* voices>
          "Like 'par> but keep track of voice number."
          [xs]
          (par>* (map (fn [i x] (lin (voice+ i) x))
                      (range)
                      xs)))

        (defn catn>
          "Creates a 'cat> of size n using the 'f update."
          [n f] (cat>* (repeat n f)))

        (defn tupn>
          "Creates a 'tup> of size n using the 'f update."
          [n f] (tup>* (repeat n f)))

        (defn $by
          "Splits the score according to the return of 'f applied to each event,
           apply 'g on each subscore and merge all the results together."
          [f g]
          (sf_ (->> (group-by f _)
                    (map (fn [[_ group]]
                           (let [s (set group)
                                 o (score-origin s)]
                             (-> (shift-score s (- o))
                                 (upd g)
                                 (shift-score o)))))
                    (reduce into #{}))))

        (defn zip
          "Zips the current score with the result of updating it with the given update 'x.
           the zipping is done by :position with the given function 'f that takes two scores and produce one.
           All the scores returned by 'f are merged into a final one which is returned."
          [f x]
          (sf_ (let [updated (upd _ x)]
                 (->> (map (fn [[position xs]]
                             (assert (apply = (map :duration xs))
                                     "each position group should have events of the same duration.")
                             (let [duration (:duration (first xs))
                                   chunk (upd updated (between position (+ position duration)))]
                               (f (set xs) chunk)))
                           (group-by :position _))
                      (reduce into #{})))))

        (defn try-until
          "Given the undeterministic update 'u,
           tries it on the score until the result of it passes 'test"
          [test u & {:keys [max] :or {max 100}}]
          (sf_ (loop [n 0]
                 (or (upd _ (lin u test))
                     (if (>= max n)
                       (recur (inc n)))))))))

(do :midi

    (def MIDI_DEFAULT_OPTIONS
      {:bpm 60})

    (def MIDI_DIRECTORIES
      {:default "generated"
       :history "generated/history"})

    (def MUSESCORE_BIN
      "/Applications/MuseScore 4.app/Contents/MacOS/mscore")

    (def options* (atom MIDI_DEFAULT_OPTIONS))

    (def sequencer* (atom nil))

    (def history* (atom ()))

    (defn gen-filename [& [dir]]
      (let [name (System/currentTimeMillis)]
        (if dir
          (str dir "/" name)
          name)))

    (defn midifiable-score [score]
      (vec (-> score numerify-pitches dedupe-patches-and-program-changes)))

    (defn options [& {:as options}]
      (sf_ (vary-meta _ assoc ::options options)))

    (defn score->midi-bytes [bpm score]
      (-> (midi/new-sequence (score-track-count score) bpm)
          (midi/add-events (midifiable-score score))
          (midi/get-midi-bytes)))

    (defn output-files [{:keys [filename midi pdf xml]}]
      (let [{:keys [directory file-barename]
             :or {directory (MIDI_DIRECTORIES :default)
                  file-barename (gen-filename)}} (u/parse-file-path filename)
            base (str directory "/" file-barename)]
        (u/ensure-directory directory)
        (merge
         {:source-file (str base ".mut")
          :seed-file (str base ".seed")}
         (when (or midi pdf xml) {:midi-file (str base ".mid")})
         (when (or pdf xml) {:xml-file (str base ".xml")})
         (when pdf {:pdf-file (str base ".pdf")}))))

    (defn noon
      ([score]
       (noon {} score))
      ([opts score]
       (let [{:as options
              :keys [tracks bpm play source]} (merge @options* opts (-> score meta ::options))

             {:as files
              :keys [midi-file source-file seed-file xml-file pdf-file]} (output-files options)

             multi-sequencer (midi/midi :bpm bpm
                                        :track-idx->sequencer (or tracks (constantly :default)) :data (midifiable-score score))]

         (when @sequencer*
           ((:stop @sequencer*))
           ((:close @sequencer*)))

         (reset! sequencer* multi-sequencer)

         (if play
           ((:play @sequencer*)))

         (if midi-file
           ((:write @sequencer*) midi-file))

         (when (zero? (:exit (shell/sh "which" MUSESCORE_BIN)))
           (if xml-file
             (shell/sh MUSESCORE_BIN "--export-to" xml-file midi-file))

           (when pdf-file
             (shell/sh MUSESCORE_BIN xml-file "-o" pdf-file)
             (u/copy-file pdf-file "last.pdf")))

         (when source
           (spit source-file source)
           (spit seed-file (u/serialize-to-base64 pr/*rnd*)))

         (swap! history* conj files)

         (with-meta files {:score score}))))

    (defmacro write [opts & xs]
      `(noon (merge {:midi true
                     :source '~&form}
                    ~opts)
             (mk ~@xs)))

    (defmacro play [& xs]
      `(noon {:filename ~(gen-filename (MIDI_DIRECTORIES :history))
              :source '~&form
              :midi true
              :play true}
             (mk ~@xs)))

    (defmacro stop []
      `(if-let [sq# @sequencer*]
         ((:close sq#))))

    (comment
      (let [s (-> (midi/new-state :bpm 60 :n-tracks 1 :sequencer (midi/init-device-sequencer midi/iac-bus-1-output-device))
                  (midi/add-events (midifiable-score (mk (tup s0 s1 s2))))
                  :sequencer)]
        (midi/show-sequencer s)
        (midi/show-sequence s))
      (noon {;:midi true
             :play true
             :sequencer (midi/init-device-sequencer midi/iac-bus-1-output-device)}
            (mk (mixtup s0 s2 s4) (mixtup d0 d1 d2 d3)))
      (noon {:sequencer (midi/init-soundfont-sequencer (midi/SOUNDFONTS :chorium))}
            (mk (mixtup s0 s2 s4) (mixtup d0 d1 d2 d3)))
      (.open @sequencer*)
      (midi/restart-sequencer @sequencer*)
      (midi/show-sequencer @sequencer*)
      (midi/display-sequence-details @sequencer*)
      (.start @sequencer*)
      (play (tupn> 7 d2))
      (show
       (mk (patch :vibraphone)
           (tup d0 d1 d2)
           (tup same (patch :flute))))
      (write {} (tup d0 d1))))
