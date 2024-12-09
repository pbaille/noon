(ns noon.events
  "Noon events are maps that represent a MIDI event.
   This ns is help to deal with such maps and defines some useful event updates (functions from event to event)"
  (:require [noon.harmony :as h]
            [noon.vst.index :as vst]
            [noon.utils.misc :as u :refer [t t?]]
            [noon.utils.maps :as m]
            [noon.utils.chance :as g]
            [noon.utils.pseudo-random :as pr]
            [noon.midi :as midi]
            [noon.numbers :as nums :refer [add sub mul div]])
  #?(:cljs (:require-macros [noon.events :refer [efn ef_ -def-durations -def-velocities -def-channels -def-tracks
                                                 import-wrap-harmony-update-constructors import-wrap-harmony-updates
                                                 -def-wrapped -def-steps -def-shifts -def-degrees]])))

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

    (defn event->pitch
      [e]
      (h/hc->pitch (:pitch e)))

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
      {:tags [:syntax :event-update]}
      [arg & body]
      `(t :event-update
          (fn [~arg] ~@body)))

    (defmacro ef_
      {:tags [:syntax :event-update]}
      [& body]
      `(efn ~'_ ~@body))

    (def event-update?
      (t? :event-update))

    (defn map->efn [x]
      (t :event-update (m/->upd x)))

    (defn chain-event-updates
      "Chain several `event-updates` one after another."
      [event-updates]
      (ef_ (u/?reduce #(%2 %1) _ event-updates)))

    (defn ->event-update [x]
      (cond (event-update? x) x
            (map? x) (map->efn x)
            (vector? x) (if-let [updates (u/?keep ->event-update x)]
                          (chain-event-updates updates))
            (and (g/gen? x)
                 (->event-update (g/realise x))) (ef_ ((->event-update (g/realise x)) _))))

    (defn event-matcher [f] (t :event-matcher f))

    (def event-matcher? (t? :event-matcher))

    (defn event-update->event-matcher [event-update]
      (event-matcher (fn [e] (= e (event-update e)))))

    (defn ->event-matcher
      "turn `x` into an event-matcher.
           An event-matcher is a function from event to boolean.
           If `x` is an event-update, the result of applying it to the received event should be
           equal to the received event in order for it to indicate a match.
           In other cases `x` is passed as second argument to `noon.utils.maps/match`"
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

    (def ^{:tags [:event-update]} e0
      (t :event-update identity))

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
                       (fn [v] (nums/->7bits-natural (m/value-merge v x))))))

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

        (def ^{:tags [:event-update]} vel0
          (vel 0)))

    (do :channel

        (defn chan
          "Builds a :velocity event-update based on `x`."
          {:tags [:event-update]}
          [x]
          (ef_ (update _ :channel
                       (fn [v] (nums/->4bits-natural (m/value-merge v x))))))

        (defn chan+
          "Builds an event-update that increase channel number by `x`"
          {:tags [:event-update]}
          [x] (chan (add x)))
        (defn chan-
          "Builds an event-update that decrease channel number by `x`"
          {:tags [:event-update]}
          [x] (chan (sub x))))

    (do :tracks

        (defn track
          "Builds an event-update that set the track number number to `x`"
          {:tags [:event-update]}
          [x]
          (ef_ (update _ :track
                       (fn [t] (nums/->16bits-natural (m/value-merge t x))))))

        (defn track+
          "Builds an event-update that increase track number number by `x`"
          {:tags [:event-update]}
          [x] (track (add x)))

        (defn track-
          "Builds an event-update that increase track number number by `x`"
          {:tags [:event-update]}
          [x] (track (sub x))))

    (do :voice

        (defn voice
          "Builds an event-update that set the voice number number to `x`"
          {:tags [:event-update]}
          [x]
          (ef_ (update _ :voice
                       (fn [v] (nums/->4bits-natural (m/value-merge v x))))))

        (defn voice+
          "Builds an event-update that increase track number number by `x`"
          {:tags [:event-update]}
          [x] (voice (add x)))

        (defn voice-
          "Builds an event-update that decrease track number number by `x`"
          {:tags [:event-update]}
          [x] (voice (sub x))))

    (defn cc
      "Build an event-update that adds the given control change to the received event.
           `key` is the control-change code, and `val` is the value for it."
      {:tags [:event-update]}
      [key val]
      (if-let [code (midi/cc-code key)]
        (map->efn {:cc {code (fn [v]
                               (let [v (m/value-merge v val)]
                                 (cond
                                   (number? v) (nums/->7bits-natural v)
                                   (sequential? v) (mapv nums/->7bits-natural v)
                                   :else (u/throw* "Bad value for event's :cc entry: " v))))}})
        (u/throw* "Unrecognised control change code: " key)))

    (defn pc
      "Builds an update that sets programs changes on the received event"
      {:tags [:event-update]}
      [& xs]
      (map->efn {:pc (vec xs)}))

    (defn patch
      "Build an event-update that change the patch of the received event."
      {:tags [:event-update]}
      ([x]
       (cond (keyword? x) (patch (vst/pick x))
             (vector? x) (ef_ (assoc _ :patch x))
             (number? x) (patch nil x)
             :else (u/throw* "noon.score/patch :: bad argument " x)))
      ([bank program]
       (patch [bank program]))))

(do :var-definitions

    "defines some vars shorthands for duration, velocity, channel and track updates."

    (defmacro -def-durations
      "Defines some duration update vars
           d2 ... d11 to multiply it
           d:2 ... d:11 to divide it"
      []
      (cons 'do
            (concat (for [i (range 2 12)]
                      (list 'do
                            (list 'def (with-meta (symbol (str "dur" i))
                                         {:doc (str "Multiply event duration by " i)
                                          :tags [:event-update :alias :temporal]
                                          :no-doc true})
                                  `(dur (mul ~i)))
                            (list 'def (with-meta (symbol (str "dur:" i))
                                         {:doc (str "Divide event duration by " i)
                                          :tags [:event-update :alias :temporal]
                                          :no-doc true})
                                  `(dur (div ~i)))))
                    (for [n (range 2 12)
                          d (range 2 12)]
                      (list 'def (with-meta (symbol (str "dur" n ":" d))
                                   {:doc (str "Multiply event duration by " n "/" d)
                                    :tags [:event-update :alias :temporal]
                                    :no-doc true})
                            `(dur (mul (/ ~n ~d))))))))
    (-def-durations)

    (defmacro -def-velocities
      "Defines 12 levels of velocity from 10 to 127 as v1 ... v12"
      []
      (cons 'do
            (for [i (range 1 13)]
              (let [v (int (* i (/ 127 12)))]
                (list 'def (with-meta (symbol (str "vel" i))
                             {:doc (str "Set event velocity to " v)
                              :tags [:event-update :alias]
                              :no-doc true})
                      `(vel ~v))))))
    (-def-velocities)

    (defmacro -def-channels []
      (cons 'do
            (for [i (range 0 16)]
              (list 'def (with-meta (symbol (str "chan" i))
                           {:doc (str "Set event midi channel to " i)
                            :tags [:event-update :alias]
                            :no-doc true})
                    `(chan ~i)))))
    (-def-channels)

    (defmacro -def-tracks []
      (cons 'do
            (for [i (range 0 16)]
              (list 'def (with-meta (symbol (str "track" i))
                           {:doc (str "Set event midi channel to " i)
                            :tags [:event-update :alias]
                            :no-doc true})
                    `(track ~i)))))
    (-def-tracks))

(do :pitch

    "Wraps noon.harmony functionality under the :pitch key of events."

    #?(:clj (defmacro import-wrap-harmony-update-constructors [& xs]
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
                                 {:tags [:event-update :harmonic]}
                                 [~'& xs#]
                                 (let [u# (apply ~(symbol "noon.harmony" (name x)) xs#)]
                                   (map->efn
                                    {:pitch
                                     (fn [ctx#]
                                       (h/upd ctx# u#))})))))
                          xs))))

    #?(:clj (defmacro import-wrap-harmony-updates [& xs]
              `(do ~@(map (fn [x]
                            (let [original-sym (symbol "noon.harmony" (name x))]
                              (list 'def (with-meta x
                                           {:doc (str "Updates the :pitch value of the received event using "
                                                      original-sym
                                                      "\n\ndoc:\n\n"
                                                      (:doc (meta (resolve original-sym))
                                                            "undocumented"))
                                            :tags [:event-update :harmonic]})
                                    `(map->efn
                                      {:pitch
                                       (fn [ctx#]
                                         #_(println ctx#)
                                         (h/upd ctx# ~(symbol "noon.harmony" (name x))))}))))
                          xs))))

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
                               :doc (str "Alias for " (list (symbol "noon.score" (name wrapper)) v))
                               :no-doc true})
                            (list wrapper v)))))

        (-def-wrapped structure noon.constants/structures)

        (-def-wrapped scale noon.constants/modes)

        (-def-wrapped repitch noon.constants/pitches))

    (do :intervals

        {:chromatic 'c
         :diatonic 'd
         :structural 's
         :tonic 't
         :octave 'o}

        (def ^{:tags [:alias :event-update]} c0 (c-step 0))
        (def ^{:tags [:alias :event-update]} d0 (d-step 0))
        (def ^{:tags [:alias :event-update]} s0 (s-step 0))
        (def ^{:tags [:alias :event-update]} t0 (t-step 0))

        (defmacro -def-steps [name prefix max f]
          (cons 'do
                (mapcat
                 (fn [n]
                   [(list 'def (with-meta (symbol (str prefix n))
                                 {:doc (str "Step up "
                                            n " " name " " (if (> n 1) "steps" "step") ".")
                                  :no-doc true
                                  :tags [:event-update :alias :harmonic]})
                          (list f n))
                    (list 'def (with-meta (symbol (str prefix n "-"))
                                 {:doc (str "Step down " n " " name " " (if (> n 1) "steps" "step") ".")
                                  :no-doc true
                                  :tags [:event-update :alias :harmonic]})
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
                                  :no-doc true
                                  :tags [:event-update :alias :harmonic]})
                          (list f n))
                    (list 'def (with-meta (symbol (str prefix n "-"))
                                 {:doc (str "Shift down " n " " name (when (> n 1) "s") ".")
                                  :tags [:event-update :alias :harmonic]
                                  :no-doc true})
                          (list f (list `- n)))])
                 (range 1 max))))

        (-def-shifts "octave" "o" 9 (fn [i] (t-shift i :forced))))

    (defmacro -def-degrees []
      (cons 'do
            (concat (for [[n v] (map vector '[I II III IV V VI VII] (range))]
                      (list 'def (with-meta n
                                   {:doc (str "Go to degree " n)
                                    :no-doc true
                                    :tags [:event-update :alias :harmonic]})
                            `(degree ~v)))
                    (for [[degree-sym degree-val] (map vector '[I II III IV V VI VII] (range))
                          [alteration-sym alteration-val] [["#" `c1] ["b" `c1-]]]
                      (let [[dn dv an av] [degree-sym degree-val alteration-sym alteration-val]]
                        (list 'def (with-meta (symbol (str dn an))
                                     {:doc (str "Go to degree " an dn)
                                      :no-doc true
                                      :tags [:event-update :alias :harmonic]})
                              `[(transpose ~av) (degree ~dv)]))))))

    (-def-degrees))
