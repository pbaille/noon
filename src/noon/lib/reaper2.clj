(ns noon.lib.reaper2
  (:require [noon.score :as noon]
            [noon.harmony :as harmony]
            [noon.utils.reaper :as reaper :refer [<<]]
            [backtick :refer [template]]
            [noon.utils.misc :as u]
            [noon.lib.melody :as m]
            [noon.utils.cider-keybindings :as kbs]))

(def REAPER_MIDI_RESOLUTION 2048)
(def REAPER_SYNC_MIDI_FILE "/Users/pierrebaille/Code/WIP/noon/generated/reaper-sync.mid")

(def reaper*
  (atom {:time-selection [0 0]
         :focus [0 60]}))

(def score*
  (atom noon/score0))

(do :watching

    (defn position->ppq [q]
      (Math/round (double (* q REAPER_MIDI_RESOLUTION))))

    (defn noon-note->reaper-note
      [{:as e :keys [position duration pitch]}]
      (let [start (+ 2 (position->ppq position))
            end (+ start (position->ppq duration))]
        (-> (dissoc e :position :duration)
            (assoc :start-position start
                   :end-position (- end 1)
                   :pitch (harmony/hc->chromatic-value pitch)))))

    (defn sync-selection! []
      (<< (let [seq u.seq
                tbl u.tbl
                t ru.take
                T (t.get-active)
                selected-notes ~(mapv noon-note->reaper-note (filter :selected @score*))]
            (each [_ n (ipairs (t.notes.get T))]
                  (let [matching-note (seq.find selected-notes
                                                (fn [sn]
                                                  (and n
                                                       (= sn.channel n.channel)
                                                       (= sn.pitch n.pitch)
                                                       (= sn.start-position n.start-position)
                                                       (= sn.end-position n.end-position)
                                                       (= sn.velocity n.velocity))))]
                    (if matching-note
                      (t.set-note T (tbl.put matching-note :idx n.idx))))))))

    (add-watch score* :score-sync
               (fn [_ _ _ score]
                 (noon/write-score score :filename REAPER_SYNC_MIDI_FILE)
                 (<< (let [t ru.take
                           item (reaper.GetSelectedMediaItem 0 0)]
                       (if item
                         (do (t.cursor.set (t.get-active) 0)
                             (reaper.DeleteTrackMediaItem (reaper.GetMediaItemTrack item) item)))
                       (reaper.InsertMedia ~REAPER_SYNC_MIDI_FILE 0)))
                 (sync-selection!)))

    (add-watch reaper* :reaper-sync
               (fn [_ _ _ {:keys [time-selection focus]}]
                 (<< (let [t ru.take
                           T (t.get-active)]
                       (t.focus.set T {:x ~(position->ppq (focus 0)) :y ~(focus 1)})
                       (t.time-selection.set T
                                             ~(position->ppq (time-selection 0))
                                             ~(position->ppq (time-selection 1)))
                       :ok)))))

(do :updates

    (defn upd-score! [& xs]
      (swap! score* (noon/lin* xs)))

    (defn set-score! [& xs]
      (reset! score* (noon/mk* xs)))

    (defn upd-focus! [f]
      (swap! reaper* update :focus f))

    (defn upd-time-selection! [f]
      (swap! reaper* update :time-selection f))

    (defn set-focus! [x]
      (swap! reaper* assoc :focus x))

    (defn set-time-selection! [x]
      (swap! reaper* assoc :time-selection x)))

(defn framed-upd! [& xs]
  (let [[start end] (:time-selection @reaper*)]
    (if-not (= start end)
      (upd-score! (parts (fn [{:keys [duration position]}]
                           (and (<= start position)
                                (<= (+ position duration) end)))
                         (noon/lin (noon/start-from start)
                                   (noon/until (- end start))
                                   (noon/lin* xs)
                                   (noon/$ {:position (partial + start)})))))))

(comment
  (set-score! {:channel 1}
              (noon/cat s0 s1 s2))
  (upd-score! (parts {:channel 1} {:selected true}))
  (set-focus! [5 76])
  (set-time-selection! [0 2])
  (framed-upd! ($ d3)))

(comment
  (set-score!
   (chans

    [(patch :vibraphone)
     vel3
     (tupn 4 [(one-of IV II VI) tetrad (par [t2- vel5] s0 s1 s2 s3)])]

    [(patch :ocarina)
     vel5
     (shuftup d1 d2 d3 d4 d5 d6)
     ($ (maybe (par d0 d3)))
     (rup 16
          (probs {(m/permutation :rand) 1
                  (m/rotation :rand) 3
                  (one-of* (map d-step (range -3 4))) 5}))])

   (adjust 10)
   (append [d2- (transpose c3)]
           [d2 (transpose c3-)]
           same)))
