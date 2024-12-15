(ns noon.midi
  "WIP, many features are missing compared to clj version"
  (:require ["tone" :as Tone]
            ["smplr" :refer [Soundfont]]
            [noon.data.GM :as gm]))

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

(do :midi

    (def playing* (atom false))

    ;; when playing some midi we get back a stop function,
    ;; all those functions are kept here in order to be able to stop all playing instruments.
    (def stop-fns* (atom []))

    (def soundfonts* (atom {}))

    ;; user can register callbacks to be triggered when playing is done
    ;; see `on-done-playing`
    (def on-done-callbacks* (atom {}))

    (defn on-done-playing
      "Register a callback to execute when corresponding play ends."
      [playing-id cb]
      (swap! on-done-callbacks* assoc playing-id cb))

    (defn done-playing!
      "This function is called with the corresponding playing-id when playing ends."
      [playing-id]
      (when (= playing-id @playing*)
        #_(println "stop playing " playing-id)
        (reset! playing* false)
        #_(println "will execute callback: " (get @on-done-callbacks* playing-id))
        (when-let [cb (get @on-done-callbacks* playing-id)]
          (cb))
        {:noon.midi/done playing-id}))

    (defn get-instrument [instrument-name]
      (or (get @soundfonts* instrument-name)
          (when-let [sf (.-load (Soundfont. (.-_context (.-context Tone))
                                            #js {:instrument instrument-name
                                                 #_:kit #_"FluidR3_GM"}))]
            (swap! soundfonts* assoc instrument-name sf)
            sf)))

    (defn stop-midi
      "Call all recorded stop-fns* and set playing* to false"
      []
      (println "stopping midi")
      #_(.stop Tone/Transport (.now Tone))
      (doseq [f @stop-fns*] (f))
      (reset! playing* false))

    (defn play
      "Play a noon score using tone and smplr."
      [noon-data]
      (stop-midi)
      (println "loading instruments...")
      #_(js/console.log Tone)
      (let [playing-id (reset! playing* (gensym))
            patch->events (group-by :patch noon-data)]

        (-> (js/Promise.all
             (mapv (fn [[[_bank instrument] events]]
                     (let [instrument-name (gm/instrument-val->instrument-name instrument)]
                       #_(log "loading: " instrument-name)
                       (.then (get-instrument instrument-name)
                              (fn [inst] [inst events]))))
                   patch->events))

            (.then (fn [xs]
                     (js/console.log "scheduling events...")
                     (let [t0 (.now Tone)
                           time-until-end
                           (reduce (fn [end-pos [instrument events]]
                                     (reduce (fn [end-pos {:as _note :keys [channel pitch velocity duration position]}]

                                               (.start instrument
                                                       #js {:channel channel
                                                            :note pitch
                                                            :velocity velocity
                                                            :duration duration
                                                            :time (+ t0 position)})
                                               (max (+ position duration) end-pos))
                                             end-pos
                                             events))
                                   0 xs)]

                       (println time-until-end)

                       (js/setTimeout (fn []
                                        (println "done playing.")
                                        (done-playing! playing-id))
                                      (+ 1000 (* 1000 time-until-end)))

                       {:id playing-id
                        :stop (fn [] (doseq [[i _] xs] (.stop i)))
                        :started (.start Tone/Transport)})))

            (.then (fn [{:keys [id stop]}]
                     (println "playing...")
                     (swap! stop-fns* conj stop)
                     {:noon.midi/playing id})))))

    (comment (def simple-tup
               [{:patch [0 4] :channel 0, :pitch 67, :voice 0, :duration (/ 1 3)  , :position (/ 2 3) , :velocity 80, :track 0}
                {:patch [0 6] :channel 0, :pitch 64, :voice 0, :duration (/ 1 3)  , :position (/ 1 3)  , :velocity 80, :track 0}
                {:patch [0 4], :channel 0, :pitch 60, :voice 0, :duration (/ 1 3), :position 0N, :velocity 80, :track 0}])

             (play-noon simple-tup)))
