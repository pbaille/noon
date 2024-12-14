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

    ;; when playing some midi we get back a stop function,
    ;; all those functions are kept here in order to be able to stop all playing instruments.
    (def stop-fns* (atom []))
    (def playing* (atom false))

    (defn get-instrument [instrument-name]
      (.-load (Soundfont. (.-_context (.-context Tone))
                          #js {:instrument instrument-name
                               #_:kit #_"FluidR3_GM"})))

    (defn stop-midi []
      (println "stopping midi")
      #_(.stop Tone/Transport (.now Tone))
      (doseq [f @stop-fns*] (f))
      (reset! playing* false))

    (defn play [noon-data]
      (stop-midi)
      #_(println "playing js " noon-data)
      #_(js/console.log Tone)
      (reset! playing* true)
      (let [patch->events (group-by :patch noon-data)]
        (-> (js/Promise.all
             (mapv (fn [[[_bank instrument] events]]
                     (let [instrument-name (gm/instrument-val->instrument-name instrument)]
                       #_(log "loading: " instrument-name)
                       (.then (get-instrument instrument-name)
                              (fn [inst] [inst events]))))
                   patch->events))
            (.then (fn [xs]
                     #_(js/console.log "instruments " xs)
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

                       (js/setTimeout (fn [] #_(println "done playing") (reset! playing* false))
                                      (* 1000 time-until-end))
                       {:stop (fn [] (doseq [[i _] xs] (.stop i)))
                        :started (.start Tone/Transport)})))
            (.then (fn [{:keys [stop]}]
                     (swap! stop-fns* conj stop))))))

    (comment (def simple-tup
               [{:patch [0 4] :channel 0, :pitch 67, :voice 0, :duration (/ 1 3)  , :position (/ 2 3) , :velocity 80, :track 0}
                {:patch [0 6] :channel 0, :pitch 64, :voice 0, :duration (/ 1 3)  , :position (/ 1 3)  , :velocity 80, :track 0}
                {:patch [0 4], :channel 0, :pitch 60, :voice 0, :duration (/ 1 3), :position 0N, :velocity 80, :track 0}])

             (play-noon simple-tup)))
