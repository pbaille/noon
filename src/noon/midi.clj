(ns noon.midi
  (:require [noon.utils.misc :as u]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.codec.base64 :as b64]
            [clj-http.client :as http])
  (:import (java.io File)
           (java.nio ByteBuffer)
           (java.util Arrays)
           (javax.sound.midi MetaMessage SysexMessage MidiEvent MidiSystem ShortMessage Sequence Sequencer)))

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
    (def MIDI-SET-KEY-SIGNATURE 0x59)
    (def MIDI-SET-TIME-SIGNATURE 0x58)

    (defn set-tempo-message
      [bpm]
      (assert (>= bpm 4))
      (let [uspq (quot 60000000 bpm)
            data (-> (ByteBuffer/allocate 4)
                     (.putInt uspq)
                     .array
                     (Arrays/copyOfRange 1 4))]
        (MetaMessage. MIDI-SET-TEMPO data 3)))

    (defn set-key-signature-message [sharps majmin]
      (MetaMessage. MIDI-SET-KEY-SIGNATURE (byte-array [sharps majmin]) 2))

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
      [sequence position]
      (* (.getResolution sequence)
         position))

    (defn add-event
      [sequence
       message
       position
       track
       & [type]]
      (let [track (aget (.getTracks sequence) track)
            tick-position (position->tick sequence position)
            tick-delta (case type :control-change 0 (:note-off :program-change :patch-change) 1 2)]
        (.add track (MidiEvent. message (+ tick-position tick-delta)))))

    (defn add-note
      [sequence note]
      (let [{:keys [position channel duration velocity pitch track]}
            (merge DEFAULT_NOTE note)]
        (add-event sequence
                   (short-message ShortMessage/NOTE_ON channel pitch velocity)
                   position
                   track)
        (add-event sequence
                   (short-message ShortMessage/NOTE_OFF channel pitch)
                   (+ position duration)
                   track
                   :note-off)
        sequence))

    (do :control|program-changes

        (defn add-control-change-over-time
          [sequence event key [start-val end-val & more? :as values]]
          (let [{:as event :keys [channel track position duration]}
                (merge DEFAULT_EVENT event)]

            (if more?

              (let [pairs (partition 2 1 values)
                    duration-step (/ duration (count pairs))
                    event (assoc event :duration duration-step)]
                (mapv (fn [idx pair]
                        (add-control-change-over-time
                         sequence
                         (update event :position + (* idx duration-step))
                         key pair))
                      (range)
                      pairs)
                sequence)

              (doseq [[delta val]
                      (map vector
                           (u/linear-interpolation 0 duration (u/dist start-val end-val))
                           (if (> end-val start-val)
                             (range start-val (inc end-val))
                             (reverse (range end-val (inc start-val)))))]
                (add-event sequence
                           (short-message ShortMessage/CONTROL_CHANGE channel (cc-code key) val)
                           (+ position delta)
                           track
                           :control-change)))
            sequence))

        (defn add-control-change
          [sequence event key val]
          (let [{:keys [channel track position]}
                (merge DEFAULT_EVENT event)]
            (cond
              (number? val)
              (add-event sequence
                         (short-message ShortMessage/CONTROL_CHANGE channel (cc-code key) val)
                         position
                         track
                         :control-change)

              (vector? val)
              (add-control-change-over-time sequence event key val))
            sequence))

        (defn add-patch-change
          [sequence event]
          (let [{:keys [channel patch track position]}
                (merge DEFAULT_EVENT event)]
            (let [[bank prog] (if (number? patch) [nil patch] patch)]
              (if bank
                (add-control-change sequence event :bank-select-2 bank))
              (add-event sequence
                         (short-message ShortMessage/PROGRAM_CHANGE channel prog)
                         position
                         track
                         :patch-change))
            sequence))

        (defn add-program-changes
          [sequence event]
          (let [{:keys [channel pc track position]}
                (merge DEFAULT_EVENT event)]
            (doseq [prog pc]
              (add-event sequence
                         (if (vector? prog)
                           (short-message ShortMessage/PROGRAM_CHANGE channel (prog 0) (prog 1))
                           (short-message ShortMessage/PROGRAM_CHANGE channel prog))
                         position
                         track
                         :program-change))
            sequence)))

    (defn add-events
      [sequence events]
      (doseq [{:as e :keys [pitch cc patch pc]} events]
        (if pitch (add-note sequence e))
        (if patch (add-patch-change sequence e))
        (if pc (add-program-changes sequence e))
        (doseq [[k v] cc] (add-control-change sequence e k v)))
      sequence))

(do :write-files

    (defn write-midi-file
      [sequence filename]
      (u/ensure-file filename)
      (let [file (File. filename)]
        (MidiSystem/write sequence 1 file)
        file))

    (defn get-midi-bytes
      [sequence]
      (let [baos (java.io.ByteArrayOutputStream.)]
        (MidiSystem/write sequence 1 baos)
        (.toByteArray baos)))

    (defn sequence->midi-string
      [sequence]
      (str (b64/encode (get-midi-bytes sequence))))

    (defn write-midi-file2
      [sequence filename]
      (u/ensure-file filename)
      (with-open [fos (java.io.FileOutputStream. filename)]
        (.write fos (get-midi-bytes sequence)))))

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

        (defn close-sequencer [sq] (when (.isOpen sq) (.stop sq) (.close sq)) sq)

        (defn restart-sequencer [sq]
          (stop-sequencer sq)
          (.setTickPosition sq 0)
          (start-sequencer sq)
          sq)

        (defn set-sequence [sq s]
          (.setSequence sq s))

        (defn play-file-with [sq filename]
          (set-sequence sq (filepath->buffered-input-stream filename))
          (start-sequencer sq))

        (defn do-midi-reset-file [sq]
          (stop-sequencer sq)
          (.setTickPosition sq 0)
          (set-sequence sq (reset-filestream))
          (start-sequencer sq)
          (Thread/sleep 30)
          (stop-sequencer sq)
          (.setTickPosition sq 0))

        (defn reset-sequencer [sq]
          (let [sequence (.getSequence sq)]
            (do-midi-reset-file sq)
            (set-sequence sq sequence))
          sq)

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
          (atom {:s3-base-url "https://pbaille-noon.s3.eu-west-3.amazonaws.com/soundfonts/"
                 :resource-path "midi/soundfonts/"
                 :local-path "resources/midi/soundfonts/"
                 :files {:chorium "choriumreva.sf2"
                         :squid "squid.sf2"
                         :airfont "airfont_340.sf2"
                         :fluid "FluidR3_GM.sf2"}}))

        (defn download-soundfont [soundfont-kw]
          (let [file (get-in @SOUNDFONTS [:files soundfont-kw])
                url (str (get @SOUNDFONTS :s3-base-url) file)
                target-file (io/file (str (get @SOUNDFONTS :local-path) file))
                response (http/get url {:as :stream})]
            (when-not (.exists (.getParentFile target-file))
              (.mkdirs (.getParentFile target-file)))
            (with-open [in (:body response)
                        out (io/output-stream target-file)]
              (io/copy in out))
            (println (str soundfont-kw " soundfont succesfully saved:"))
            (.toURL target-file)))

        (defn soundfont-resource [name-kw]
          (if-let [filename (get-in @SOUNDFONTS [:files name-kw])]
            (or (io/resource (str (get @SOUNDFONTS :resource-path) filename))
                (let [path (str (get @SOUNDFONTS :local-path) filename)]
                  (when (.exists (io/file path)) (.toURL (io/file path))))
                (do (println "Soundfont is not locally available, downloading it...")
                    (download-soundfont name-kw)))
            (u/throw* "Unknown soundfont: " name-kw
                      "\nshould be one of:\n" (keys (get @SOUNDFONTS :files)))))

        (defn init-synth [sf2-resource]
          (let [bank (MidiSystem/getSoundbank (resource->buffered-input-stream sf2-resource))
                sy (MidiSystem/getSynthesizer)]
            (.open sy)
            (.loadAllInstruments sy bank)
            sy))

        (defonce synths* (atom {}))

        (defn get-soundfont-synth [name-kw]
          (or (get @synths* name-kw)
              (if-let [soundfont-resource (soundfont-resource name-kw)]
                (let [synth (init-synth soundfont-resource)]
                  (swap! synths* assoc name-kw synth)
                  synth)
                (u/throw* "Unknown soundbank: " name-kw))))

        (defn init-soundfont-sequencer [synth]
          (let [sq (MidiSystem/getSequencer false)]
            (.setReceiver
             (.getTransmitter sq)
             (.getReceiver synth))
            sq))

        (defn new-soundfont-sequencer [name-kw]
          (init-soundfont-sequencer (get-soundfont-synth name-kw))))

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

        (def get-iac-bus-1-output-device
          (memoize (fn [] (get-output-device "Bus 1"))))

        (defn new-bus-1-sequencer []
          (init-device-sequencer (get-iac-bus-1-output-device)))

        (def virtual-output-devices
          {:bus1 (get-output-device "Bus 1")
           :bus2 (get-output-device "Bus 2")
           :bus3 (get-output-device "Bus 3")
           :bus4 (get-output-device "Bus 4")})

        (defn new-virtual-output-sequencer [kw]
          (init-device-sequencer (virtual-output-devices kw)))

        (comment
          (show-sequencer bus-1-sequencer)
          (play-file-with bus-1-sequencer "generated/history/1709837113419.mid"))))

(defn new-sequencer [x]
  (case x
    :default (new-midi-sequencer true)
    (:chorium :airfont :squid :fluid) (new-soundfont-sequencer x)
    (:bus1 :bus2 :bus3 :bus4) (new-virtual-output-sequencer x)
    (if (instance? Sequencer x)
      x
      (new-midi-sequencer true))))

(defn new-sequence
  [n-tracks bpm]
  (let [sequence (Sequence. Sequence/PPQ MIDI_RESOLUTION)]

    (dotimes [_ n-tracks]
      (let [track (.createTrack sequence)]

        (.add track (MidiEvent. (set-tempo-message bpm) 0))
        (.add track (MidiEvent. (set-key-signature-message 0 0) 0))))

    sequence))

(defn copy-track [sequence track]
  (let [target-track (.createTrack sequence)]
    (doseq [i (range (.size track))]
      (.add target-track (.get track i)))
    sequence))

(defn midi [& {:keys [track-idx->sequencer bpm data]
               :or {track-idx->sequencer (constantly :default)}}]
  (let [track->data (group-by :track data)
        n-tracks (inc (apply max (keys track->data)))
        sequence (add-events (new-sequence n-tracks bpm)
                             data)
        sequencers (map (fn [track-idx]
                          (let [sequencer (new-sequencer (track-idx->sequencer track-idx))]
                            (.setSequence sequencer (copy-track (new-sequence 1 bpm)
                                                                (aget (.getTracks sequence) track-idx)))
                            sequencer))
                        (range n-tracks))]
    {:play (fn []
             (doseq [s sequencers] (reset-sequencer s))
             (doseq [s sequencers] (restart-sequencer s)))
     :write (fn [filename] (write-midi-file sequence filename))
     :midi-string (fn [] (sequence->midi-string sequence))
     :stop (fn [] (doseq [s sequencers] (stop-sequencer s)))
     :close (fn [] (doseq [s sequencers] (close-sequencer s)))}))



(comment :scratch

         (clojure.pprint/pprint
          (.getInstruments (.getDefaultSoundbank (MidiSystem/getSynthesizer))))

         (defn write-pitch-line
           [pitches filename & [opts]]
           (let [{:as opts :keys [duration tempo bpm]} (merge DEFAULT_NOTE opts)]
             (write-midi-file
              (reduce (fn [state [idx pitch]]
                        (add-note state (assoc opts :pitch pitch :position (* idx duration))))
                      (new-state :bpm (or tempo bpm 60))
                      (map vector (range) pitches))
              filename)))

         (comment
           (-> (new-state)
               (add-note {:channel 0 :position 0 :pitch 60 :duration 4 :velocity 80})
               (add-note {:position 4 :pitch 62 :duration 2})
               (add-note {:position 6 :pitch 64 :duration 3/2})
               (add-control-change {:duration 5} :expression [10 20 80])
               (add-patch-change {:channel 1 :patch [7 9]})
               (add-note {:position 7.5 :pitch 66 :duration 3/2})
               (add-note {:position 7.5 :pitch 68 :duration 3/2})
               (write-midi-file "hello.mid"))
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
               (write-midi-file "hello-multi-track.mid")))

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
