(ns noon.midi
  (:require [noon.utils.misc :as u]
            [clojure.string :as str])
  (:import (java.io File)
           (java.nio ByteBuffer)
           (java.util Arrays)
           (javax.sound.midi MetaMessage MidiEvent MidiSystem ShortMessage Sequence)))

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


    (cc-code :bank-select)
    (cc-code :expression)
    (cc-code "resonance")
    (cc-code "Resonance"))

#_(defn note [x]
  (if (map? x)
    (merge DEFAULT_NOTE x)))

(defn new-midi-sequencer
  [& [connected]]
  (doto (MidiSystem/getSequencer (boolean connected)) .open))

(def MIDI-SET-TEMPO 0x51)
(defn set-tempo-message
  [bpm]
  (assert (>= bpm 4))
  (let [uspq (quot 60000000 bpm)
        data (-> (ByteBuffer/allocate 4)
                 (.putInt uspq)
                 .array
                 (Arrays/copyOfRange 1 4))]
    (MetaMessage. MIDI-SET-TEMPO data 3)))

(def MIDI-SET-KEY-SIGNATURE 0x59)
(defn set-key-signature-message [sharps majmin]
  (MetaMessage. MIDI-SET-KEY-SIGNATURE (byte-array [sharps majmin]) 2))


(defn new-state
  [& {:keys [bpm n-tracks connected]
      :or {bpm 60 n-tracks 1}}]
  (let [sequencer (new-midi-sequencer connected)
        sq (Sequence. Sequence/PPQ MIDI_RESOLUTION)]

    (doto sequencer
      (.setSequence sq)
      (.setTickPosition 0))

    (dotimes [_ n-tracks]
      (let [track (.createTrack sq)]

        (.add track (MidiEvent. (set-tempo-message bpm) 0))
        (.add track (MidiEvent. (set-key-signature-message 0 0) 0))))

    {:tempo bpm
     :sequencer sequencer}))

(defn short-message
  [type channel data1 & [data2]]
  (doto (ShortMessage.)
    (.setMessage type channel data1 (or data2 127))))

(defn position->tick
  [{:as _state :keys [sequencer]}
   position]
  (* (.getResolution (.getSequence sequencer))
     position))

(defn add-event
  [{:as state :keys [sequencer]}
   message
   position
   track
   & [type]]
  (let [sequence (.getSequence sequencer)
        track (aget (.getTracks sequence) track)
        tick-position (position->tick state position)
        tick-delta (case type :control-change 0 (:note-off :patch-change) 1 2)]
    (.add track (MidiEvent. message (+ tick-position tick-delta)))
    state))

(defn add-note
  [state note]
  (let [{:keys [position channel duration velocity pitch track]}
        (merge DEFAULT_NOTE note)]
    (add-event state
               (short-message ShortMessage/NOTE_ON channel pitch velocity)
               position
               track)
    (add-event state
               (short-message ShortMessage/NOTE_OFF channel pitch)
               (+ position duration)
               track
               :note-off)
    state))

(defn add-control-change-over-time
  [state event key [start-val end-val & more? :as values]]
  (let [{:as event :keys [channel track position duration]}
        (merge DEFAULT_EVENT event)]

    (if more?

      (let [pairs (partition 2 1 values)
            duration-step (/ duration (count pairs))
            event (assoc event :duration duration-step)]
        (mapv (fn [idx pair]
                (add-control-change-over-time
                 state
                 (update event :position + (* idx duration-step))
                 key pair))
              (range)
              pairs)
        state)

      (doseq [[delta val]
              (map vector
                   (u/linear-interpolation 0 duration (u/dist start-val end-val))
                   (if (> end-val start-val)
                     (range start-val (inc end-val))
                     (reverse (range end-val (inc start-val)))))]
        (add-event state
                   (short-message ShortMessage/CONTROL_CHANGE channel (cc-code key) val)
                   (+ position delta)
                   track
                   :control-change)))
    state))

(defn add-control-change
  [state event key val]
  (let [{:keys [channel track position]}
        (merge DEFAULT_EVENT event)]
    (cond
      (number? val)
      (add-event state
                 (short-message ShortMessage/CONTROL_CHANGE channel (cc-code key) val)
                 position
                 track
                 :control-change)

      (vector? val)
      (add-control-change-over-time state event key val))
    state))

(defn add-patch-change
  [state event]
  (let [{:keys [channel patch track position]}
        (merge DEFAULT_EVENT event)]
    (let [[bank prog] (if (number? patch) [nil patch] patch)]
      (if bank
        (add-control-change state event :bank-select-2 bank))
      (add-event state
                 (short-message ShortMessage/PROGRAM_CHANGE channel prog)
                 position
                 track
                 :patch-change))
    state))

(defn add-events
  [state events]
  (doseq [{:as e :keys [pitch cc patch]} events]
    (if pitch (add-note state e))
    (if patch (add-patch-change state e))
    (doseq [[k v] cc] (add-control-change state e k v)))
  state)

(defn add-notes [state notes]
  (reduce add-note state notes))

(defn write-midi-file
  [{:as _state :keys [sequencer]}
   filename]
  (u/ensure-file filename)
  (MidiSystem/write (.getSequence sequencer)
                    1 (File. filename)))

(defn- filepath->buffered-input-stream [p]
      (java.io.BufferedInputStream. (java.io.FileInputStream. (File. p))))

(do :play1

    (defonce play-sequencer (new-midi-sequencer :connected))
    (defn stop [] (.stop play-sequencer))
    (defn play [] (stop) (.start play-sequencer))

    (defn play-file [filename]
      (stop)
      (let [stream (filepath->buffered-input-stream filename)]
        (.setSequence play-sequencer stream)
        (play))))

(do :play2

    (def SOUNDFONTS
      {:chorium "midi/soundfonts/choriumreva.sf2"
       :squid "midi/soundfonts/squid.sf2"})

    (defn reset-filestream []
      (filepath->buffered-input-stream "midi/reset.mid"))

    (defn init-sequencer2 [sf2-path]
      (let [bank (MidiSystem/getSoundbank (filepath->buffered-input-stream sf2-path))
            sq (MidiSystem/getSequencer false)
            sy (MidiSystem/getSynthesizer)]
        (.open sq) (.open sy)
        (.loadAllInstruments sy bank)
        (.setReceiver
         (.getTransmitter sq)
         (.getReceiver sy))
        sq))

    (defonce play-sequencer2
      (init-sequencer2 (SOUNDFONTS :chorium)))

    (comment
      (def play-sequencer2
        (init-sequencer2
         (SOUNDFONTS ([:chorium :yami :squid] 0)))))

    (defn stop2 [] (.stop play-sequencer2))
    (defn reset2 [] (stop2) (.setSequence play-sequencer2 (reset-filestream)) (.start play-sequencer2) (Thread/sleep 100))
    (defn play2 [] (.start play-sequencer2))

    (defn play-file2 [filename]
      (reset2)
      (let [stream (filepath->buffered-input-stream filename)]
        (.setSequence play-sequencer2 stream)
        (play2))))

(defn write-and-play
  [state filename]
  (write-midi-file state filename)
  (play-file filename))

(defn write-pitch-line
  [pitches filename & [opts]]
  (let [{:as opts :keys [duration tempo bpm]} (merge DEFAULT_NOTE opts)]
    (write-midi-file
     (reduce (fn [state [idx pitch]]
               (add-note state (assoc opts :pitch pitch :position (* idx duration))))
             (new-state :bpm (or tempo bpm 60))
             (map vector (range) pitches))
     filename)))





(comment :first-xp

         (ShortMessage/CONTROL_CHANGE)

         (defn export-midi-file!
           [sq filename]
           (MidiSystem/write sq 0 (File. filename)))

         (defn hello-midi
           "just try to emit a midi file with one note"
           []
           (let [sequencer (new-midi-sequencer)
                 sq (Sequence. Sequence/PPQ 128)
                 track (.createTrack sq)
                 tempo 60]
             (doto sequencer
               (.setSequence sq)
               (.setTickPosition 0))

             (.add track (MidiEvent. (set-tempo-message tempo) 0))

             (let [note-on (doto (ShortMessage.)
                             (.setMessage ShortMessage/NOTE_ON
                                          0 60 80))
                   note-off (doto (ShortMessage.)
                              (.setMessage ShortMessage/NOTE_OFF
                                           0 60 80))]

               (.add track (MidiEvent. note-on 0))
               (.add track (MidiEvent. note-off 512)))

             (export-midi-file! sq "test.mid"))))

(comment
  (-> (new-state)
      (add-note {:channel 0 :position 0 :pitch 60 :duration 4 :velocity 80})
      (add-note {:position 4 :pitch 62 :duration 2})
      (add-note {:position 6 :pitch 64 :duration 3/2})
      (add-control-change {:duration 5} :expression [10 20 80])
      (add-patch-change {:channel 1 :patch [7 9]})
      (add-note {:position 7.5 :pitch 66 :duration 3/2})
      (add-note {:position 7.5 :pitch 68 :duration 3/2})
      (write-midi-file "hello.mid")
      )

  (-> (new-state)
      (add-note {:channel 0 :position 0 :pitch 60 :duration 4 :velocity 80})
      (add-note {:position 4 :pitch 62 :duration 2})
      (add-note {:position 6 :pitch 64 :duration 3/2})
      (add-note {:position 7.5 :pitch 66 :duration 3/2})
      (add-note {:position 7.5 :pitch 68 :duration 3/2})
      (write-and-play "generated/test-play.mid"))

  (stop)
  (play)

  (let [s (new-midi-sequencer :connected)]
    (future (.setSequence s (java.io.BufferedInputStream. (java.io.FileInputStream. (File. "data/bach1.mid"))))
            (.start s)
            (Thread/sleep 5000)
            (.stop s)))

  (let [s (new-midi-sequencer :connected)]
    (future (.setSequence s (java.io.BufferedInputStream. (java.io.FileInputStream. (File. "generated/I.mid"))))
            (.start s)
            #_(Thread/sleep 5000)
            #_(.stop s)))

  (-> (new-state 60 2)
      (add-note {:channel 0 :position 0 :pitch 60 :duration 4 :velocity 80})
      (add-note {:pitch 68 :duration 8 :track 1})
      (add-control-change-over-time {:cc :expression :duration 1 :start-value 50 :end-value 70})
      (write-midi-file "hello-multi-track.mid")
      ))

(comment
  ((short-message ShortMessage/PROGRAM_CHANGE 1 2))
  (write-pitch-line [60 64 68 71]
                    "generated/write-pitch-line_test.mid"
                    {:duration 1/2}))

;; https://stackoverflow.com/questions/53922543/javax-midi-play-midi-file-with-custom-soundfont
(comment :play-with-custom-soundfont

         (let [bank (MidiSystem/getSoundbank (filepath->buffered-input-stream "midi/choriumreva.sf2"))
               sq (MidiSystem/getSequencer false)
               sy (MidiSystem/getSynthesizer)]
           (.open sq) (.open sy)
           (.loadAllInstruments sy bank)
           (.setReceiver
            (.getTransmitter sq)
            (.getReceiver sy))
           (.setSequence sq (filepath->buffered-input-stream "midi/bach1.mid"))
           (.start sq)
           (Thread/sleep 10000)
           (.stop sq)))
