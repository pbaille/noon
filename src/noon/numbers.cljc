(ns noon.numbers
  "Noon events are maps that represent a MIDI event.
   This ns is help to deal with such maps and defines some useful event updates (functions from event to event)"
  (:require [noon.utils.misc :as u :refer [f_]]
            [noon.utils.pseudo-random :as pr]))

(defn sub {:tags [:arythmetic]} [x] (f_ (- _ x)))
(defn add {:tags [:arythmetic]} [x] (f_ (+ _ x)))
(defn mul {:tags [:arythmetic]} [x] (f_ (* _ x)))
(defn div {:tags [:arythmetic]} [x] (f_ (/ _ x)))

(defn eq {:tags [:arythmetic]} [x] (f_ (= _ x)))
(defn gt {:tags [:arythmetic]} [x] (f_ (> _ x)))
(defn lt {:tags [:arythmetic]} [x] (f_ (< _ x)))
(defn gte {:tags [:arythmetic]} [x] (f_ (>= _ x)))
(defn lte {:tags [:arythmetic]} [x] (f_ (<= _ x)))

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
      (min 15)))

(defn float-equal?
  [a b & {:keys [epsilon] :or {epsilon 1e-5}}]
  (< (abs (- a b)) epsilon))

(defn is-multiple? [a b & [epsilon]]
  (let [epsilon (or epsilon 1e-5)
        r (rem a b)]
    (or (< (abs r) epsilon)
        (< (- b r) epsilon))))

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
