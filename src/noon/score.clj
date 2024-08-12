(ns noon.score
  "build, transform, play and write midi scores"
  (:require [clojure.core :as c]
            [noon.midi :as midi]
            [noon.harmony :as h]
            [noon.vst.index :as vst]
            [noon.constants :as constants]
            [noon.utils.misc :as u :refer [t t? f_ defn*]]
            [noon.utils.maps :as m]
            [noon.utils.chance :as g]
            [noon.utils.pseudo-random :as pr]
            [noon.externals :as externals]
            [clojure.string :as str]))

(do :help

    (defn sub [x] (f_ (- _ x)))
    (defn add [x] (f_ (+ _ x)))
    (defn mul [x] (f_ (* _ x)))
    (defn div [x] (f_ (/ _ x)))

    (defn eq [x] (f_ (= _ x)))
    (defn gt [x] (f_ (> _ x)))
    (defn lt [x] (f_ (< _ x)))
    (defn gte [x] (f_ (>= _ x)))
    (defn lte [x] (f_ (<= _ x)))

    (defn ?reduce
      "like `clojure.core/reduce` but short-circuits (returns nil) on first falsy result"
      [f init xs]
      (reduce (fn [a e]
                (or (f a e) (reduced nil)))
              init xs))

    (defn ?keep
      "like `clojure.core/map`, but if `f` returns nil for one element, the whole form returns nil."
      [f xs]
      (reduce (fn [ret e]
                (if-let [v (f e)]
                  (conj ret v)
                  (reduced nil)))
              [] xs))

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
      "MIDI sometimes deals with 16 bits values,
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
          (t :event-update (m/->upd x)))

        (defn chain-event-updates
          "Chain several `event-updates` one after another."
          [event-updates]
          (ef_ (?reduce #(%2 %1) _ event-updates)))

        (defn ->event-update [x]
          (cond (event-update? x) x
                (map? x) (map->efn x)
                (vector? x) (if-let [updates (?keep ->event-update x)]
                              (chain-event-updates updates))
                (and (g/gen? x)
                     (->event-update (g/realise x))) (ef_ ((->event-update (g/realise x)) _))))

        (defn event-matcher [f] (t :event-matcher f))

        (def event-matcher? (t? :event-matcher))

        (defn event-update->event-matcher [event-update]
          (event-matcher (fn [e] (= e (event-update e)))))

        (defn ->event-matcher
          {:doc (str/join "\n"
                          ["turn `x` into an event-matcher."
                           "an event-matcher is a function from event to boolean."
                           "If `x` is an event-update, the result of applying it to the received event should be "
                           "equal to the received event in order for it to indicate a match."
                           "In other cases `x` is passed as second argument to `noon.utils.maps/match`:"
                           (:doc (meta #'m/match))])}
          [x]
          (cond (event-matcher? x) x
                (event-update? x) (event-update->event-matcher x)
                :else (event-matcher (fn [e] (m/match e x))))))

    (do :midi-val

        (defn midi-val
          "Build a midi value (int between 0 and 127) from `x`.
           `x` can be either:
           - an integer, that will be constrained to 0-127 range.
           - a float or a rational, that will be scaled to the 0-127 range.
           - :min or :max keywords that will map to 0 and 127 respectively."
          [x]
          (cond
            (int? x) (-> x (min 127) (max 0))
            (number? x) (midi-val (int (Math/round (* (float x) 127))))
            :else (case x
                    :min 0
                    :max 127
                    (u/throw* `midi-val "bad argument:" x))))

        (defn humanize
          "Build a function that humanize a midi value (int between 0 and 127).
           Options are:
           - :max-step is the maximum step that can occur in either directions.
             its value can be:
             - a natural number, which represent the maximum step value in either direction
             - a rational or float, which indicates the size of maximun step relatively to the allowed value range (see :bounds option).
             - nil, indicating that any step can be made within specified :bounds (default to the whole midi value range 0-127)
           - :bounds is a vector of the form [min-value max-value] that constrain input and output of the created update.
             the two value it contains can be any valid `noon.score/midi-val` argument (natural, rational,float or :min and :max keywords)"
          [& {:keys [max-step bounds]}]
          (let [[min-val max-val] (mapv midi-val (or bounds [0 127]))
                bound (fn [x] (-> x (min max-val) (max min-val)))
                in-bounds? (fn [x] (<= min-val x max-val))
                max-step (cond (not max-step) (- max-val min-val)
                               (int? max-step) max-step
                               (number? max-step) (int (Math/round (* (float max-step) (- max-val min-val)))))
                steps (remove zero? (range (- max-step) (inc max-step)))]
            (fn [v]
              (let [v (bound v)]
                (+ v (pr/rand-nth (filter (fn [s] (in-bounds? (+ s v))) steps))))))))

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

            (defn vel-humanize
              "Build an event update that humanize the :velocity value.
               please refer to the `noon.score/humanize` doc."
              [max-step & [bounds]]
              (let [f (humanize {:bounds bounds :max-step max-step})]
                (ef_ (update _ :velocity f))))

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

            (defn voice [x]
              (ef_ (update _ :voice
                           (fn [v] (->4bits-natural (m/value-merge v x))))))

            (defn voice+ [x] (voice (add x)))
            (defn voice- [x] (voice (sub x))))

        (defn cc [key val]
          (if-let [code (midi/cc-code key)]
            (map->efn {:cc {code (fn [v]
                                   (let [v (m/value-merge v val)]
                                     (cond
                                       (number? v) (->7bits-natural v)
                                       (sequential? v) (mapv ->7bits-natural v)
                                       :else (u/throw* "Bad value for event's :cc entry: " v))))}})
            (u/throw* "Unrecognised control change code: " key)))

        (defn pc [& xs]
          (map->efn {:pc (vec xs)}))

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
                                   (map->efn
                                    {:pitch
                                     (fn [ctx#]
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
             origin scale structure degree root inversion
             repitch rescale restructure reorigin reroot redegree)

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

                (-def-wrapped structure noon.constants/structures)

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

        (defn map-event-update
          {:doc (str "Apply `event-update` to each event of `score`. "
                     "if `event-update` returns nil for an event, it is removed from the resulting score.")}
          [score event-update]
          (set (keep event-update score)))

        (defn scale-score
          "Scale score timing by given ratio."
          [score ratio]
          (map-event-update score
                            (map->efn {:duration (mul ratio)
                                       :position (mul ratio)})))

        (defn shift-score
          "Shift all position by the given offset."
          [score offset]
          (map-event-update score
                            (map->efn {:position (add offset)})))

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

        (defn sort-score
          "Sort `score` events.
           arity 1: by :position.
           arity 2: by `f`
           arity 3: by `f` using `comp` as compare fn."
          ([score] (sort-score :position score))
          ([f score] (sort-by f score))
          ([f comp score] (sort-by f comp score)))

        (defn filter-score [score f]
          (set (filter (->event-matcher f) score)))

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
                  (into [(pitch-value event) duration position]))
                (sort-score :position score))))

    (do :score-update

        "A score-update is a function that takes a score and return a score."

        (defmacro sfn
          "Just a tagged lambda that represents a score update function."
          [arg & body]
          `(t :score-update
              (fn [~arg] ~@body)))

        (defmacro sf_ [& body]
          `(sfn ~'_ ~@body))

        (def score-update? (t? :score-update))

        (declare chain-score-updates)

        (defn ->score-update
          "Turn 'x into a score-update if possible."
          [x]
          (if-let [event-update (->event-update x)]
            (sf_ (map-event-update _ event-update))
            (cond (score-update? x) x
                  (vector? x) (chain-score-updates x)
                  (g/gen? x) (if (->score-update (g/realise x))
                               (sf_ ((->score-update (g/realise x)) _))))))

        (defn ->score-update!
          "Strict version of `noon.score/->score-update`, it throws if `x` is not convertible."
          [x]
          (or (->score-update x)
              (u/throw* `->score-update! "not convertible: " x)))

        (defn ->score-checker
          "Convert `x` to a score-checker if possible.
           A score-checker is a score-update that can return the score unchanged or nil indicating failure."
          [x]
          (if (or (event-matcher? x)
                  (event-update? x)
                  (map? x))

            (sfn s
                 (if (every? (->event-matcher x) s) s))

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
              (u/throw* `->score-checker! "not convertible: " x)))

        (defn chain-score-updates [updates]
          (if-let [updates (?keep ->score-update updates)]
            (sf_ (?reduce #(%2 %1) _ updates))
            (u/throw* `chain-score-updates "bad argument: " updates)))

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

        (defn update-score
          "Updates `score` with `update`."
          [score update]
          (if-let [score-update (->score-update update)]
            (score-update score)
            (u/throw* `update-score "bad argument: " update)))

        (defn partial-update
          "Use `event-matcher` to match some events of `score`, apply `update` to the resulting subscore,
           then merge unselected events into the updated subscore.
           see `noon.score/->event-matcher` for exact semantics of event matching."
          [score event-matcher update]
          (let [grouped (group-by (->event-matcher event-matcher) score)
                common (set (get grouped false))
                updated (update-score (get grouped true) update)]
            (into common updated)))

        (defn map-update [score update]
          (if-let [event-update (->event-update update)]
            (map-event-update score event-update)
            (if-let [score-update (->score-update update)]
              (map-score-update score score-update)
              (u/throw* `map-update "bad argument: " update))))))

(do :creation

    "Main entry point to create a score."

    (defn mk*
      "Feed score0 into given updates."
      [updates]
      ((chain-score-updates updates) score0))

    (defn mk [& updates]
      (mk* updates)))

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
      {:doc "Act like 'mk, ignoring current score."
       :tags [:base]}
      [updates]
      (sf_ (mk* updates)))

    (def ^{:doc "Returns the empty score regardless of input."
           :tags [:base]}
      void
      (sf_ #{}))

    (defn* chain
      {:doc "Compose several updates together linearly."
       :tags [:base]}
      [updates]
      (chain-score-updates updates))

    (defn* par
      {:doc "Apply several update on a score merging the results."
       :tags [:base :parallel]}
      [updates]
      (sf_ (merge-scores (map #(update-score _ %) updates))))

    (defn* par>
      {:doc "Accumulative 'par."
       :tags [:accumulative :parallel]}
      [updates]
      (sf_ (loop [segments [_] updates updates]
             (if-let [[update & updates] updates]
               (recur (conj segments (update-score (peek segments) update)) updates)
               (merge-scores (next segments))))))

    (defn* each
      {:doc "Apply an update to each events of a score."
       :tags [:base :iterative]}
      [updates]
      (sf_ (reduce map-update _ updates)))

    (defn* lin
      {:doc "Feed each transformations with the current score and concatenate the results."
       :tags [:base :linear]}
      [updates]
      (sf_ (concat-scores
            (map #(update-score _ %) updates))))

    (defn* lin>
      {:doc "Accumulative 'lin."
       :tags [:base :linear :accumulative]}
      [updates]
      (sf_ (loop [segments [_] updates updates]
             (if-let [[update & updates] updates]
               (recur (conj segments (update-score (peek segments) update)) updates)
               (concat-scores (next segments))))))

    (defn* fit
      {:doc (str "Wraps the given transformation 'x, stretching its output to the input score duration. "
                 "In other words, turn any transformation into another one that do not change the duration of its input score.")
       :tags [:base]}
      [updates]
      (sf_ (fit-score (update-score _ (chain* updates))
                      {:duration (score-duration _)})))

    (defn* tup
      {:doc "Like 'lin but preserve the length of the input score"
       :tags [:base :linear]}
      [updates] (fit (lin* updates)))

    (defn* tup>
      {:doc "Accumulative 'tup."
       :tags [:accumulative :linear]}
      [updates] (fit (lin>* updates)))

    (defn* append
      {:doc "Like 'lin but insert the current score before."
       :tags [:base :linear]}
      [updates]
      (lin* (cons same updates)))

    (defn* append>
      {:doc "Accumulative 'append."
       :tags [:linear :accumulative]}
      [updates]
      (chain* (map append updates)))

    (defn* superpose
      {:doc "Like 'par but keep the current score."
       :tags [:base :parallel]}
      [updates]
      (par* (cons same updates)))

    (defn* superpose>
      {:doc "Accumulative 'superpose."
       :tags [:parallel :accumulative]}
      [updates]
      (chain* (map superpose updates)))

    (defn rep
      {:doc "Iterates the given `update` `n-times` over the input score and concat the results."
       :tags [:base :linear :accumulative]}
      ([n-times update]
       (rep n-times update false))
      ([n-times update skip-first]
       (let [update (->score-update update)]
         (sf_ (->> (if skip-first (update _) _)
                   (iterate update)
                   (take n-times)
                   (concat-scores))))))

    (defn rup
      {:doc "Iterates the given `update` `n-times` over the input score and tup the results."
       :tags [:base :linear :accumulative]}
      ([n-times update]
       (rup n-times update false))
      ([n-times update skip-first]
       (fit (rep n-times update skip-first))))

    (defn dup
      {:doc "Duplicate the received score `n-times` and concat the duplicates."
       :tags [:base :linear :multiplicative]}
      [n-times]
      (sf_ (concat-scores (repeat n-times _))))

    (defn dupt
      {:doc "Duplicate received score `n-times` and tup the duplicates."
       :tags [:base :linear :multiplicative]}
      [n-times]
      (fit (dup n-times)))

    (defn ntup
      {:doc "Creates a tup of size `n` using `update`."
       :tags [:base :linear :multiplicative]}
      [n update] (tup* (repeat n update)))

    (defn nlin
      {:doc "Creates a lin of size `n` using `update`."
       :tags [:base :linear :multiplicative]}
      [n update] (lin* (repeat n update)))

    (defn* parts
      {:doc "Apply updates to subscores: (parts sel1 upd1 sel2 upd2 ...)"
       :tags [:base :partial]}
      [xs]
      (sf_ (reduce (fn [s [filt upd]]
                     (partial-update s filt upd))
                   _ (partition 2 xs))))

    (defn repeat-while
      {:doc "Iterate the given `update` while `test` is passing."
       :tags [:base :iterative]}
      ([test update] (repeat-while test update same))
      ([test update after]
       (let [update (->score-update! update)
             test (->score-checker! test)
             after (->score-update! after)]
         (sf_ (let [nxt (update _)]
                (if (test nxt)
                  (recur nxt)
                  (after nxt)))))))

    (defn* fst
      {:doc "Tries given `updates` in order until the first success (non empty score)."
       :tags [:base :selective]}
      [updates]
      (sf_ (loop [updates updates]
             (if-let [[update & updates] (seq updates)]
               (or (not-empty (update-score _ update))
                   (recur updates))))))

    (defn* fst-that
      {:doc "Tries given `updates` in order until one passes `test`."
       :tags [:base :selective]}
      [test updates]
      (let [test (->score-checker! test)]
        (fst* (map (f_ (chain _ test))
                   updates))))

    (defn shrink
      {:doc "Shrink a score using `event-check` on each events to determine if it is kept or not."
       :tags [:base :temporal]}
      [event-check]
      (sf_ (filter-score _ event-check)))

    (defn adjust
      {:doc "Time stretching/shifting operation syntax sugar over `noon.score/fit-score`."
       :tags [:base :temporal]}
      [x]
      (let [opts (cond (map? x) x
                       (number? x) {:duration x}
                       (vector? x) {:duration (x 1) :position (x 0)})]
        (sf_ (fit-score _ opts))))

    (defn* fork-with
      {:doc (str "Like `noon.score/par` but let you the opportunity to do something on the score "
                 "based on the index of the branch before applying corresponding update.")}
      [branch-idx->update branch-updates]
      (par* (map-indexed (fn [i update] (chain (branch-idx->update i) update))
                         branch-updates)))

    (defn* voices
      {:doc "Apply `updates` in parallel on subsequent voices."
       :tags [:base :parallel]}
      [updates] (fork-with* voice+ updates))

    (defn* chans
      {:doc "Apply `updates` in parallel on subsequent midi channels."
       :tags [:base :parallel]}
      [updates] (fork-with* chan+ updates))

    (defn* tracks
      {:doc "Apply `updates` in parallel on subsequent midi tracks."
       :tags [:base :parallel]}
      [updates] (fork-with* track+ updates))

    (defn mirror
      {:doc "Mirrors all pitches around `pitch`."
       :tags [:harmonic]}
      [pitch] (each {:pitch (h/mirror pitch)}))

    (def ^{:doc "Reverse the given score."
           :tags [:temporal]}
      rev
      (sf_ (reverse-score _)))

    (defn event-scale
      {:doc "Restrains and scale one event `dimension` to the given bounds over the whole score."
       :tags [:scaling :bounding]}
      [dimension x]
      (let [[min-out max-out]
            (cond (number? x) [0 x]
                  (vector? x) x)]
        (sf_ (let [[min-in max-in] (mapv dimension (score-bounds _ dimension))
                   f #(u/scale-range % min-in max-in min-out max-out)]
               (update-score _ (each (ef_ (update _ dimension f))))))))

    (do :selection

        (defn min-by
          {:doc "Build an update that returns a one element score,
           applying `f` to each event and selecting the event for which `f` returns the lowest value."
           :tags [:selective]}
          [f]
          (sf_ #{(first (sort-by f _))}))

        (defn max-by
          {:doc "Build an update that returns a one element score,
           applying `f` to each event and selecting the event for which `f` returns the greatest value."
           :tags [:selective]}
          [f]
          (sf_ #{(last (sort-by f _))}))

        (def ^{:doc "Return a one event score, holding the lowest pitch of the received score."
               :tags [:selective :harmonic]}
          min-pitch (min-by pitch-value))

        (def ^{:doc "Return a one event score, holding the highest pitch of the received score."
               :tags [:selective :harmonic]}
          max-pitch (max-by pitch-value))

        (do :time

            "Updates to select time sections of a score."

            (defn from
              {:doc "Build an update that removes the elements anterior to the given position from the received score."
               :tags [:temporal :selective]}
              [x]
              (shrink {:position (gte x)}))

            (defn until
              {:doc "Build an update that removes the elements posterior to the given position from the received score."
               :tags [:temporal :selective]}
              [x]
              (shrink {:position (lt x)}))

            (defn between
              {:doc "Build an update that keeps only events that are positioned between x and y positions."
               :tags [:temporal :selective]}
              [x y]
              (chain (from x) (until y)))

            (defn start-from
              {:doc "Build an update that shifts the score to the given position, removing all anterior events."
               :tags [:temporal :selective]}
              [x]
              (chain (from x) {:position (sub x)}))

            (def ^{:doc "Shifting the score to last position erasing all anterior events."
                   :tags [:temporal :selective]}
              start-from-last
              (sf_ (-> (group-by :position _)
                       sort last val set
                       (update-score {:position 0}))))

            (defn start-from-nth-last
              {:doc "Shifting the score to `nth` last position erasing all anterior events."
               :tags [:temporal :selective]}
              [nth]
              (sf_ (let [sorted (sort (group-by :position _))]
                     (if (>= (count sorted) nth)
                       (let [taken (map (comp set val) (take-last nth sorted))]
                         (update-score (merge-scores taken)
                                       {:position (sub (:position (ffirst taken)))}))))))

            (defn trim
              {:doc (str "Build and update that removes everything before `beg` and after `end` from the received score. "
                         "(triming overlapping durations).")
               :tags [:temporal :selective]}
              [beg end]
              (sf_ (trim-score _ beg end)))))

    (do :checks

        (defn within-bounds?
          {:doc (str "Build a check update (one that can return nil or the score unchanged) "
                     "succeed if `event-fn` applied to each event is between `min` and `max`.")
           :tags [:check :bounding]}
          [event-fn min max]
          (->score-checker
           (fn [s] (every? (fn [e] (<= min (event-fn e) max))
                           s))))

        (defn within-time-bounds?
          {:doc (str "Build a check update (one that can return nil or the score unchanged) "
                     "Succeed if all its events are between `start` and `end`.")
           :tags [:check :temporal]}
          [start end]
          (->score-checker
           (fn [s]
             (and (>= (score-origin s) start)
                  (<= (score-duration s) end)))))

        (defn within-pitch-bounds?
          {:doc (str "Build a check update (one that can return nil or the score unchanged)"
                     "Succeed if all pitches are between `min` and `max`."
                     "`min` and `max` should be 'pitchable' (pitch map | pitch keyword | int).")
           :tags [:check :harmonic]}
          [min max]
          (within-bounds? (comp h/hc->chromatic-value :pitch)
                          (:c (constants/get-pitch min))
                          (:c (constants/get-pitch max))))

        (def ^{:doc "Returns the score unchanged if every pitch within it are in the 0-127 MIDI range."
               :tags [:check :harmonic]}
          within-midi-pitch-bounds?
          (within-pitch-bounds? 0 127)))

    (do :non-determinism

        (defmacro !
          {:doc (str "Takes a non deterministic `expression` resulting in a score update."
                     "Returns a score update that wraps the `expression` so that it is evaluated each time the update is called.")
           :tags [:non-deterministic]}
          [expression]
          `(vary-meta (sfn score# (update-score score# ~expression))
                      assoc :non-deterministic true))

        (defn* one-of
          {:doc "Returns an update that choose randomly one of the given `updates` before applying it."
           :tags [:non-deterministic]}
          [updates]
          (! (pr/rand-nth updates)))

        (defn* maybe
          {:doc "Like `noon.score/one-of`, return an update that choose randomly one of the given `updates`, but can also do nothing."
           :tags [:non-deterministic]}
          [updates]
          (one-of* (cons same updates)))

        (defn probs
          {:doc "Takes a map of type {update number} where each key is an update and each value is its probability of occurence."
           :tags [:non-deterministic]}
          [m]
          (let [pm (g/weighted m)]
            (! (pm))))

        (defn* any-that
          {:doc "Tries `updates` in random order until one passes `test`."
           :tags [:non-deterministic :check]}
          [test updates]
          (! (fst-that* test (pr/shuffle updates))))

        (defn* mixtup
          {:doc "A tup that mix its elements."
           :tags [:linear :non-deterministic]}
          [updates]
          (tup* (pr/shuffle updates)))

        (defn* shuftup
          {:doc "A tup that shuffles its elements everytime it is used."
           :tags [:linear :non-deterministic]}
          [updates]
          (! (mixtup* updates)))

        (defn* mixlin
          {:doc "A lin that mix its elements."
           :tags [:linear :non-deterministic]}
          [updates]
          (lin* (pr/shuffle updates)))

        (defn* shuflin
          {:doc "A lin that shuffles its elements everytime it is used."
           :tags [:linear :non-deterministic]}
          [updates]
          (! (mixlin* updates)))

        #_(defn* shuffle-dimensions
            "Shuffles the values of the given dimensions."
            {:tags [:non-deterministic]}
            [dims]
            (sf_ (let [size (count _)
                       idxs (range size)
                       mappings (zipmap idxs (pr/shuffle idxs))
                       events (vec _)]
                   (reduce (fn [s i] (conj s (merge (events i) (select-keys (events (mappings i)) dims))))
                           #{}
                           idxs)))))

    (do :incubator

        (defn* voices>
          {:doc "Like `noon.score/par>` but keep track of voice number."}
          [updates]
          (par>* (map (fn [i update] (chain (voice+ i) update))
                      (range)
                      updates)))

        (defn nlin>
          {:doc "Creates a `noon.score/lin>` of size `n` using `update`."}
          [n update] (lin>* (repeat n update)))

        (defn ntup>
          {:doc "Creates a `noon.score/tup>` of size `n` using `update`."}
          [n update] (tup>* (repeat n update)))

        (defn fill
          {:doc (str "Fill the score using a `tup` of `update` of size (score-duration / `resolution`)"
                     "`resolution` should be an exact multiple of received score's duration.")
           :tags [:temporal :multiplicative]}
          [resolution update]
          (sf_ (let [sdur (score-duration _)
                     n (quot sdur resolution)]
                 (if-not (zero? (rem sdur resolution))
                   (u/throw* `fill " resolution should be a multiple of score length "))
                 (update-score _ (ntup n update)))))

        (defn fill>
          {:doc (str "Fill the score using an accumulative `tup>` of `update` of size (score-duration / `resolution`)"
                     "`resolution` should be an exact multiple of received score's duration.")
           :tags [:temporal :multiplicative]}
          [resolution update]
          (sf_ (let [sdur (score-duration _)
                     n (quot sdur resolution)]
                 (if-not (zero? (rem sdur resolution))
                   (u/throw* `fill> " resolution should be a multiple of score length "))
                 (update-score _ (ntup> n update)))))

        (defn $by
          {:doc (str "Splits the score according to the return of `event->group` applied to each event."
                     "apply `update` on each subscore and merge all the results together."
                     "Before being updated, each subscore is repositioned to zero, and shifted back to its original position after.")}
          [event->group update]
          (sf_ (->> (group-by event->group _)
                    (map (fn [[_ group]]
                           (let [s (set group)
                                 o (score-origin s)]
                             (-> (shift-score s (- o))
                                 (update-score update)
                                 (shift-score o)))))
                    merge-scores)))

        (defn zip
          {:doc (str "Zips the current score with the result of updating it with `update`."
                     "the zipping is done by :position with `zip-fn` that takes two scores and produce one."
                     "All the scores returned by `zip-fn` are merged into a final one which is returned.")}
          [zip-fn update]
          (sf_ (let [updated (update-score _ update)]
                 (->> (group-by :position _)
                      (map (fn [[position xs]]
                             (assert (apply = (map :duration xs))
                                     "each position group should have events of the same duration.")
                             (let [duration (:duration (first xs))
                                   chunk (update-score updated (between position (+ position duration)))]
                               (zip-fn (set xs) chunk))))
                      merge-scores))))

        (defn try-until
          {:doc "Given an undeterministic `update`, tries it on the score until the result of it passes `test`"}
          [test update & {:keys [max] :or {max 100}}]
          (sf_ (loop [n 0]
                 (or (update-score _ (chain update test))
                     (if (>= max n)
                       (recur (inc n)))))))

        (defn newrep
          {:doc "INCUB: simple rep"}
          ([n] (newrep n same))
          ([n & xs]
           (let [[update flags] (if (keyword? (first xs)) [same xs] [(first xs) (rest xs)])
                 flags (zipmap flags (repeat true))
                 updates (repeat n update)]
             (cond (:par flags) (par* updates)
                   (:fit flags) (tup* updates)
                   :else (lin* updates)))))

        (defn iter
          {:doc "INCUB: accumulative rep"}
          [x & xs]
          (let [[n [f & {:as options}]] (if (number? x) [x xs] [nil (cons x xs)])]
            (println n f options)
            (sf_ (let [u (->score-update f)
                       seed (if (:next options) (update-score _ u) _)
                       scores (->> (iterate u seed)
                                   (drop (:drop options 0))
                                   (take (:take options n)))]
                   (cond (:par options) (merge-scores scores)
                         (:fit options) (fit-score (concat-scores scores) {:duration (score-duration _)})
                         :else (concat-scores scores))))))))

(do :midi

    (def MIDI_DEFAULT_OPTIONS
      {:bpm 60
       :tracks {0 :chorium}})

    (def MIDI_DIRECTORIES
      {:default "generated"
       :history "generated/history"})

    (def options* (atom MIDI_DEFAULT_OPTIONS))

    (def sequencer* (atom nil))

    (def history* (atom ()))

    (defn gen-filename [& [dir]]
      (let [name (System/currentTimeMillis)]
        (if dir
          (str dir "/" name)
          name)))

    (defn midifiable-score [score]
      (vec (-> score numerify-pitches dedupe-patches-and-control-changes)))

    (defn options [& {:as options}]
      (sf_ (vary-meta _ assoc ::options options)))

    (defn score->midi-bytes [bpm score]
      (-> (midi/new-sequence (score-track-count score) bpm)
          (midi/add-events (midifiable-score score))
          (midi/get-midi-bytes)))

    (defn output-files [{:keys [filename midi pdf xml mp3]}]
      (let [{:keys [directory file-barename]
             :or {directory (MIDI_DIRECTORIES :default)
                  file-barename (gen-filename)}} (u/parse-file-path filename)
            base (str directory "/" file-barename)]
        (u/ensure-directory directory)
        (merge
         {:source-file (str base ".noon")
          :seed-file (str base ".seed")}
         (when (or midi mp3 pdf xml) {:midi-file (str base ".mid")})
         (when (or pdf xml) {:xml-file (str base ".xml")})
         (when pdf {:pdf-file (str base ".pdf")})
         (when mp3 {:mp3-file (str base ".mp3")}))))

    (defn noon
      ([score]
       (noon {} score))
      ([opts score]
       (let [{:as options
              :keys [tracks bpm play source]} (merge @options* opts (-> score meta ::options))

             {:as files
              :keys [midi-file source-file seed-file]} (output-files options)

             multi-sequencer (midi/midi :bpm bpm
                                        :track-idx->sequencer (or tracks (constantly :default))
                                        :data (midifiable-score score))]

         (when @sequencer*
           ((:stop @sequencer*))
           ((:close @sequencer*)))

         (reset! sequencer* multi-sequencer)

         (if play
           ((:play @sequencer*)))

         (if midi-file
           ((:write @sequencer*) midi-file))

         (externals/handle-externals files)

         (when source
           (spit source-file source)
           (spit seed-file (u/serialize-to-base64 @pr/random*)))

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
      (play (ntup> 7 d2))
      (show
       (mk (patch :vibraphone)
           (tup d0 d1 d2)
           (tup same (patch :flute))))
      (write {} (tup d0 d1))))
