(ns noon.numbers
  "Noon events are maps that represent a MIDI event.
   This ns is help to deal with such maps and defines some useful event updates (functions from event to event)"
  (:require [noon.utils.misc :as u :refer [f_]]))

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
  (let [epsilon (or epsilon 1e-5)]
    (or (< (abs (rem a b)) epsilon)
        (< (- b  (rem a b)) epsilon))))
