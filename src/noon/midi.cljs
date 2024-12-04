(ns noon.midi
  (:require [clojure.string :as str]))

;; https://github.com/alda-lang/alda-sound-engine-clj/blob/master/src/alda/sound/midi.clj

(def DEFAULT_EVENT
  {:position 0
   :channel 0
   :track 0})

(def DEFAULT_NOTE
  (merge DEFAULT_EVENT
         {:duration 1
          :pitch 60
          :velocity 80}))

(def MIDI_RESOLUTION 2048)

(do :control-changes

    ;; https://anotherproducer.com/online-tools-for-musicians/midi-cc-list/
    ;; most commons control changes message codes
    (def MIDI-CC
      {0 "Bank Select 1"
       1 "Modulation Wheel"
       2 "Breath controller"
       4 "Foot Pedal"
       5 "Portamento Time"
       6 "Data Entry"
       7 "Volume"
       8 "Balance"
       10 "Pan position"
       11 "Expression"
       12 "Effect Control 1"
       13 "Effect Control 2"
       32 "Bank Select 2"
       64 "Hold Pedal"
       65 "Portamento"
       66 "Sostenuto Pedal"
       67 "Soft Pedal"
       68 "Legato Pedal"
       69 "Hold 2 Pedal"
       70 "Sound Variation"
       71 "Resonance"
       72 "Sound Release Time"
       73 "Sound Attack Time"
       74 "Frequency Cutoff"})

    (defn cc-name->keyword [name]
      (-> (str/lower-case name)
          (str/replace #" " "-")
          (keyword)))

    (def CONTROL_CHANGES
      (map (fn [[code name]]
             {:code code
              :name name
              :key (cc-name->keyword name)})
           MIDI-CC))

    (defn cc-code [x]
      (if (int? x)
        x
        (-> (filter (fn [{:keys [name key]}]
                      (or (= x key)
                          (= (str/lower-case x) (str/lower-case name))))
                    CONTROL_CHANGES)
            first :code)))

    (comment :tries

             (cc-code :bank-select)
             (cc-code :expression)
             (cc-code "resonance")
             (cc-code "Resonance")))
