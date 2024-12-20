(ns noon.output
  (:require [noon.score :as score]
            [noon.midi :as midi]
            #?@(:clj [[noon.externals :as externals]
                      [noon.utils.misc :as u]
                      [noon.utils.pseudo-random :as pr]])))

#?(:clj (do (def MIDI_DEFAULT_OPTIONS
               {:bpm 60
                :tracks {0 :default}})

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
               (vec (-> score score/numerify-pitches score/dedupe-patches-and-control-changes)))

             (defn options [& {:as options}]
               (score/sf_ (vary-meta _ assoc ::options options)))

             (defn score->midi-bytes [bpm score]
               (-> (midi/new-sequence (score/score-track-count score) bpm)
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

                  (try (when @sequencer*
                         ((:stop @sequencer*))
                         ((:close @sequencer*)))
                       (catch Exception _ nil))

                  (reset! sequencer* multi-sequencer)

                  (if play
                    ((:play @sequencer*)))

                  (if midi-file
                    ((:write @sequencer*) midi-file))

                  (externals/handle-externals files)

                  (when source
                    (spit source-file source)
                  ;; TODO this is incorrect, random state should be captured before the score being computed.
                    (spit seed-file (u/serialize-to-base64 @pr/random*)))

                  (swap! history* conj files)

                  (with-meta files {:score score}))))

             (defmacro write [opts & xs]
               `(noon (merge {:midi true
                              :source '~&form}
                             ~opts)
                      (score/score ~@xs)))

             (defmacro play [& xs]
               `(noon {:filename ~(gen-filename (MIDI_DIRECTORIES :history))
                       :source '~&form
                       :midi true
                       :play true}
                      (score/score ~@xs)))

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
               (write {} (tup d0 d1)))))

(defn play-score
  {:tags [:base :playing]}
  [score]
  #?(:clj (noon {:play true} score)
     :cljs (midi/play (score/numerify-pitches score))))

(defn stop-playback
  {:tags [:base :playing]}
  []
  #?(:clj (stop) :cljs (midi/stop-midi)))
