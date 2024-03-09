(ns noon.midi
  (:require [noon.utils.misc :as u]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import (java.io File)
           (java.nio ByteBuffer)
           (java.util Arrays)
           (javax.sound.midi MetaMessage SysexMessage MidiEvent MidiSystem ShortMessage Sequence)))

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

(do :meta-messages

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

      (def MIDI-SET-TIME-SIGNATURE 0x58)
      (defn set-time-signature-message
        [n-remaining-beats numerator denominator-pow2 & [metronome-beat-midi-clock-length n-32th-by-beat]]
        (MetaMessage. MIDI-SET-TIME-SIGNATURE
                      (byte-array [n-remaining-beats
                                   numerator
                                   denominator-pow2
                                   (or metronome-beat-midi-clock-length 24)
                                   (or n-32th-by-beat 32)])
                      5)))

(do :events-and-notes

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
            tick-delta (case type :control-change 0 (:note-off :program-change :patch-change) 1 2)]
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

    (do :control|patch-changes

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

        (defn add-program-changes
          [state event]
          (let [{:keys [channel pc track position]}
                (merge DEFAULT_EVENT event)]
            (doseq [prog pc]
              (add-event state
                         (short-message ShortMessage/PROGRAM_CHANGE channel prog)
                         position
                         track
                         :program-change))
            state)))

    (defn add-events
      [state events]
      (doseq [{:as e :keys [pitch cc patch pc]} events]
        (if pitch (add-note state e))
        (if patch (add-patch-change state e))
        (if pc (add-program-changes state e))
        (doseq [[k v] cc] (add-control-change state e k v)))
      state)

    (defn add-notes [state notes]
      (reduce add-note state notes)))

(do :write-files

    (defn write-midi-file
      [{:as _state :keys [sequencer]}
       filename]
      (u/ensure-file filename)
      (let [file (File. filename)]
        (MidiSystem/write (.getSequence sequencer)
                          1 file)
        file))

    (defn get-midi-bytes
      [{:as _state :keys [sequencer]}]
      (let [baos (java.io.ByteArrayOutputStream.)
            sequence (.getSequence sequencer)]
        (MidiSystem/write sequence 1 baos)
        (.toByteArray baos)))

    (defn write-midi-file2
      [state filename]
      (u/ensure-file filename)
      (with-open [fos (java.io.FileOutputStream. filename)]
        (.write fos (get-midi-bytes state)))))

(do :buffered-input-streams

    (defn- filepath->buffered-input-stream [p]
      (java.io.BufferedInputStream. (java.io.FileInputStream. (File. p))))

    (defn- resource->buffered-input-stream [url]
      (-> url
          .openStream
          java.io.BufferedInputStream.))

    (defn reset-filestream []
      (resource->buffered-input-stream (io/resource "midi/reset.mid"))))

(do :sequencers

    (do :operations

        (defn start-sequencer [sq] (if-not (.isOpen sq) (.open sq)) (.start sq) sq)

        (defn stop-sequencer [sq] (if (.isOpen sq) (.stop sq)) sq)

        (defn close-sequencer [sq] (if (.isOpen sq) (.close sq)) sq)

        (defn restart-sequencer [sq]
           (stop-sequencer sq)
           (.setTickPosition sq 0)
           (start-sequencer sq)
           sq)

        (defn set-sequence [sq s]
           (.setSequence sq s))

        (defn reset-sequencer
           "Stop and close the given sequencer,
       Run the midi reset file in order to clean residual state."
           [sq]
           (stop-sequencer sq)
           #_(close-sequencer sq)
           (set-sequence sq (reset-filestream))
           (start-sequencer sq)
           (Thread/sleep 100)
           #_(close-sequencer sq)
           sq)

        (defn load-sequencer [sq filename]
           (reset-sequencer sq)
           (set-sequence sq (filepath->buffered-input-stream filename))
           sq)

        (defn play-file-with [sq filename]
           (load-sequencer sq filename)
           (start-sequencer sq))

        (do :printing

            (defn track->events [track]
              (for [i (range (.size track))]
                 (.get track i)))

            (defn event->string [event tick]
              (let [msg (.getMessage event)]
                 (cond
                   (instance? ShortMessage msg)
                   (let [command (.getCommand ^ShortMessage msg)
                         channel (.getChannel ^ShortMessage msg)
                         data1 (.getData1 ^ShortMessage msg)
                         data2 (.getData2 ^ShortMessage msg)]
                     (format "t %-6d ShortMessage: command=%-4d channel=%-2d data1=%-4d data2=%-4d" tick command channel data1 data2))

                   (instance? SysexMessage msg)
                   (let [data (.getData ^SysexMessage msg)]
                     (format "t %-6d SysexMessage: data=%s" tick (java.util.Arrays/toString data)))

                   (instance? MetaMessage msg)
                   (let [type (.getType ^MetaMessage msg)
                         data (.getData ^MetaMessage msg)]
                     (format "t %-6d MetaMessage: type=%-3d data=%s" tick type (java.util.Arrays/toString data)))

                   :else
                   (str "Message is of unknown type"))))

            (defn show-sequence [sequencer]
              (let [sequence (.getSequence sequencer)]                               ; Get the sequence
                 (println "Sequence details:")
                 (println "------------------")
                 (doseq [track (.getTracks sequence)
                         :let [events (track->events track)]]
                   (println "Track contains" (.size track) "events:")
                   (doseq [ev events]
                     (println (event->string ev (.getTick ev)))))))

            (defn show-sequencer [sequencer]
              (do (println)
                  (println "Sequencer information:")
                  (println "----------------------")
                  (println "Sequencer class: " (.getClass sequencer))
                  (println "Is sequencer open?: " (.isOpen sequencer))
                  (println "Microsecond position: " (.getMicrosecondPosition sequencer))
                  (println "Tempo In beats per minute: " (.getTempoInBPM sequencer))
                  (println "Tempo In microseconds per quarter note: " (.getTempoInMPQ sequencer))
                  (println "Tracks: ")
                  (show-sequence sequencer)
                  (println "----------------------")
                  (println)))))

    (do :default

        (defn new-midi-sequencer
           [& [connected]]
           (MidiSystem/getSequencer (boolean connected))))

    (do :soundfont

        (def SOUNDFONTS
           {:chorium "midi/soundfonts/choriumreva.sf2"
            :squid "midi/soundfonts/squid.sf2"})

        (defn init-synth [sf2-path]
           (let [bank (MidiSystem/getSoundbank (resource->buffered-input-stream (io/resource sf2-path)))
                 sy (MidiSystem/getSynthesizer)]
             (.open sy)
             (.loadAllInstruments sy bank)
             sy))

        (def chorium-synth (init-synth (SOUNDFONTS :chorium)))

        (defn init-soundfont-sequencer [synth]
           (let [sq (MidiSystem/getSequencer false)]
             (.setReceiver
             (.getTransmitter sq)
             (.getReceiver synth))
             sq))

        (defn new-chorium-sequencer []
            (init-soundfont-sequencer chorium-synth)))

    (do :external-device

        (defn get-output-device [name]
       (first (keep (fn [info]
                      (let [device (MidiSystem/getMidiDevice info)]
                        (if (and (= (.getName info) name)
                                (not (zero? (.getMaxReceivers device))))
                          device)))
                   (MidiSystem/getMidiDeviceInfo))))

        (defn init-device-sequencer [device]
       (let [sq (MidiSystem/getSequencer false)]
         (.open device)
         (.setReceiver
         (.getTransmitter sq)
         (.getReceiver device))
         sq))

        (def iac-bus-1-output-device
       (get-output-device "Bus 1"))

        (defn new-bus-1-sequencer []
       (init-device-sequencer iac-bus-1-output-device))

        (comment
       (show-sequencer bus-1-sequencer)
       (play-file-with bus-1-sequencer "generated/history/1709837113419.mid"))))

(defn new-sequencer [x]
  (case x
    :default (new-midi-sequencer true)
    :chorium (new-chorium-sequencer)
    :bus1 (new-bus-1-sequencer)
    (if (instance? javax.sound.midi.Sequencer x)
      x
      (new-midi-sequencer true))))

(defn new-state
  [& {:keys [sequencer bpm n-tracks connected]
      :or {bpm 60 n-tracks 1}}]
  (let [sequencer (new-sequencer sequencer)
        sq (Sequence. Sequence/PPQ MIDI_RESOLUTION)]

    (doto sequencer
      (.setTickPosition 0)
      (.setSequence sq))

    (dotimes [_ n-tracks]
      (let [track (.createTrack sq)]

        (.add track (MidiEvent. (set-tempo-message bpm) 0))
        (.add track (MidiEvent. (set-key-signature-message 0 0) 0))))

    {:tempo bpm
     :sequencer sequencer}))


(comment :scratch

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
                   (write-midi-file "hello-multi-track.mid"))))

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
                    (.stop sq))))

(comment :gpt

         (defn send-midi [bus-name note velocity]
           (let [info  (->> (MidiSystem/getMidiDeviceInfo)
                            (map (fn [info] {:name (.getName info) :device (MidiSystem/getMidiDevice info)}))
                            (filter #(.contains (:name %) bus-name))
                            first)
                 device (:device info)
                 sq (MidiSystem/getSequencer false)]
             (.open device)
             (.setReceiver (.getTransmitter sq) (.getReceiver device))
             (let [sequence (Sequence. Sequence/PPQ 1 1)
                   _ (.setSequence sq sequence)
                   track (.createTrack sequence)
                   message (doto (ShortMessage.) (.setMessage ShortMessage/NOTE_ON 0 note velocity))
                   message-off (doto (ShortMessage.) (.setMessage ShortMessage/NOTE_OFF 0 note 0))
                   note-on-event (javax.sound.midi.MidiEvent. message 0)
                   note-off-event (javax.sound.midi.MidiEvent. message-off 1)]
               (.add track note-on-event)
               (.add track note-off-event)
               sq)))

         (let [sq (send-midi "Bus 1" 60 93)]
           (.open sq)
           (.start sq))

         (let [s (-> (new-state :sequencer chorium-sequencer)
                     (add-note DEFAULT_NOTE)
                     (:sequencer))]
           (.open s)
           (.start s)))
