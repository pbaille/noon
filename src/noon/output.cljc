(ns noon.output
  (:require [noon.score :as score]
            [noon.midi :as midi]
            #?@(:clj [[noon.externals :as externals]
                      [noon.utils.misc :as u]]))
  #?(:cljs (:require-macros [noon.output :refer [play]])))


(def OUTPUT_DIRECTORIES
  {:default "generated"
   :history "generated/history"})

(def DEFAULT_OPTIONS
  {:bpm 60
   :tracks {0 :default}
   :output-dir (OUTPUT_DIRECTORIES :default)})

(def options* (atom DEFAULT_OPTIONS))

(def sequencer* (atom nil))

(def history* (atom ()))

(defn midifiable-score [score]
  (-> score
      score/numerify-pitches
      #?(:clj score/dedupe-patches-and-control-changes)
      vec))

(defn options [& {:as options}]
  (score/sf_ (vary-meta _ assoc ::options options)))

#?(:clj (defn score->midi-bytes [bpm score]
          (-> (midi/new-sequence (score/score-track-count score) bpm)
              (midi/add-events (midifiable-score score))
              (midi/get-midi-bytes))))

(defn output-files [{:keys [output-dir id source midi pdf xml mp3]} score]
  (let [base (str output-dir "/" id)]
    #?(:clj (u/ensure-directory output-dir))
    (merge
     {:id id}
     (when source
       {:source-file (str base ".noon")
        :seed-file (str base ".seed")})
     (when (or midi mp3 pdf xml) {:midi-file (str base ".mid")})
     (when (or pdf xml) {:xml-file (str base ".xml")})
     (when pdf {:pdf-file (str base ".pdf")})
     (when mp3 {:mp3-file (str base ".mp3")}))))

(defn noon
  ([score]
   (noon {} score))
  ([opts score]
   (let [midifiable-score (midifiable-score score)
         id (or (:id opts) (hash midifiable-score))
         {:as options
          :keys [tracks bpm play source mute]} (merge @options* {:id id} opts (-> score meta ::options))

         {:as files
          :keys [midi-file]} (output-files options score)

         multi-sequencer (midi/midi :bpm bpm
                                    :track-idx->sequencer (or tracks (constantly :default))
                                    :data midifiable-score
                                    :mute mute)]

     (try (when @sequencer*
            ((:close @sequencer*)))
          (catch #?(:clj Exception
                    :cljs js/Error) _ nil))

     (reset! sequencer* multi-sequencer)

     (if (and play (not mute))
       ((:play @sequencer* (fn []))))

     (if midi-file
       ((:write @sequencer* identity) midi-file))

     #?(:clj (when (not mute) (externals/handle-externals files))
        :cljs '(throw (js/Error. (str [:noon.output/unsupported-cljs-options options]))))

     (when source
       #?(:clj (spit (:source-file files) source)
          :cljs (throw (js/Error. "writing files is not supported from cljs")))

       ;; TODO this is incorrect, random state should be captured before the score being computed.
       #_(spit (:seed-file files) (u/serialize-to-base64 @pr/random*)))

     (swap! history* conj files)

     (with-meta files {:score score}))))

(defmacro write [opts & xs]
  `(noon (merge {:midi true
                 :source '~&form}
                ~opts)
         (score/score ~@xs)))

(defmacro play [& xs]
  `(let [score# (score/score ~@xs)]
     (noon {:output-dir (OUTPUT_DIRECTORIES :history)
            :id (hash score#)
            :source '~&form
            :midi true
            :play true}
           score#)))

(defn stop []
  (if-let [sq @sequencer*]
    ((:close sq))))

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
  (write {} (tup d0 d1)))
