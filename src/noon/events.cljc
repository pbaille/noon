(ns noon.events
  "Noon events are maps that represent a MIDI event.
   This ns is help to deal with such maps and defines some useful event updates (functions from event to event)"
  (:require [noon.harmony :as h]
            [noon.utils.misc :as u :refer [t t?]]
            [noon.utils.maps :as m]
            [noon.utils.chance :as g]
            [noon.constants :as constants])
  #?(:cljs (:require-macros [noon.events :refer [efn ef_ -def-durations -def-velocities -def-channels -def-tracks
                                                 import-wrap-harmony-update-constructors import-wrap-harmony-updates
                                                 -def-wrapped -def-steps -def-shifts -def-degrees]])))

(def DEFAULT_EVENT
  {:position 0
   :duration 1
   :channel 0
   :track 0
   :velocity 80
   :pitch h/DEFAULT_HARMONIC_CONTEXT
   :voice 0
   :patch [0 4]})

(defn normalise-event
  "Puts time related dimensions of a note into their default values.
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
            (g/gen? x) (ef_ (let [v (g/realise x)]
                              (if-let [f (->event-update v)]
                                (f _)
                                (u/throw* "The non deterministic value was expected to realise to something that can cast to event-update: "
                                          v))))))

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
                                  `(noon.updates/dur (noon.numbers/mul ~i)))
                            (list 'def (with-meta (symbol (str "dur:" i))
                                         {:doc (str "Divide event duration by " i)
                                          :tags [:event-update :alias :temporal]
                                          :no-doc true})
                                  `(noon.updates/dur (noon.numbers/div ~i)))))
                    (for [n (range 2 12)
                          d (range 2 12)]
                      (list 'def (with-meta (symbol (str "dur" n ":" d))
                                   {:doc (str "Multiply event duration by " n "/" d)
                                    :tags [:event-update :alias :temporal]
                                    :no-doc true})
                            `(noon.updates/dur (noon.numbers/mul (/ ~n ~d))))))))

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
                      `(noon.updates/vel ~v))))))

    (defmacro -def-channels []
      (cons 'do
            (for [i (range 0 16)]
              (list 'def (with-meta (symbol (str "chan" i))
                           {:doc (str "Set event midi channel to " i)
                            :tags [:event-update :alias]
                            :no-doc true})
                    `(noon.updates/chan ~i)))))

    (defmacro -def-tracks []
      (cons 'do
            (for [i (range 0 16)]
              (list 'def (with-meta (symbol (str "track" i))
                           {:doc (str "Set event midi channel to " i)
                            :tags [:event-update :alias]
                            :no-doc true})
                    `(noon.updates/track ~i))))))

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

    (do :defs

        (defmacro -def-wrapped [type wrapper]
          (let [entries (case type
                          :modes constants/modes
                          :structures constants/structures
                          :pitches constants/pitches)]
            (cons 'do (for [[k v] entries]
                        (list 'def
                              (with-meta (symbol (name k))
                                {:tags [:event-update :alias :harmonic]
                                 :doc (str "Alias for " (list (symbol "noon.updates" (name wrapper)) v))
                                 :no-doc true})
                              (list wrapper v)))))))

    (do :intervals

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
                 (range 1 max)))))

    (defmacro -def-degrees []
      (cons 'do
            (concat (for [[n v] (map vector '[I II III IV V VI VII] (range))]
                      (list 'def (with-meta n
                                   {:doc (str "Go to degree " n)
                                    :no-doc true
                                    :tags [:event-update :alias :harmonic]})
                            `(noon.updates/degree ~v)))
                    (for [[degree-sym degree-val] (map vector '[I II III IV V VI VII] (range))
                          [alteration-sym alteration-val] [["#" 'noon.updates/c1] ["b" 'noon.updates/c1-]]]
                      (let [[dn dv an av] [degree-sym degree-val alteration-sym alteration-val]]
                        (list 'def (with-meta (symbol (str dn an))
                                     {:doc (str "Go to degree " an dn)
                                      :no-doc true
                                      :tags [:event-update :alias :harmonic]})
                              `[(noon.updates/transpose ~av) (noon.updates/degree ~dv)])))))))
