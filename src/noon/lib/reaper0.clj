(ns noon.lib.reaper0
  (:use noon.score)
  (:require [noon.harmony :as harmony]
            [noon.utils.reaper :as reaper :refer [<<]]
            [backtick :refer [template]]
            [noon.utils.misc :as u]
            [noon.midi :as midi]
            [noon.score :as noon]
            [noon.utils.cider-keybindings :as kbs]))

(def MIDI_RESOLUTION midi/MIDI_RESOLUTION)

(def REAPER_SYNC_MIDI_FILE
  "/Users/pierrebaille/Code/WIP/noon/generated/reaper-sync.mid")

(def score* (atom score0))

(defn upd-score! [& xs]
  (swap! score*
         (lin* xs)))

(do :convertions

    (defn convert-rationals [noon-event]
      (-> noon-event
          (update :position float)
          (update :duration float)))

    (defn round [n]
      (Math/round (double n) ))

    (defn ppq->qpos [ppq]
      (/ ppq MIDI_RESOLUTION))

    (defn qpos->ppq [q]
      (round (* q MIDI_RESOLUTION)))

    (defn noon-note->reaper-note
      [{:as e :keys [position duration]}]
      (let [start (qpos->ppq position)
            end (+ start (qpos->ppq duration))]
        (-> (dissoc e :position :duration)
            (assoc :start-position start
                   :end-position end))))

    (defn score->notes [score]
      (mapv noon-note->reaper-note (numerify-pitches score))))

(do :get-reaper-selection

    (def equivalent-notes?
      (memoize
       (fn [reaper-note noon-event]
         (and (= (:channel noon-event) (:channel reaper-note))
              (= (:velocity noon-event) (:velocity reaper-note))
              (let [start-position (int (:start-position reaper-note))
                    end-position (int (:end-position reaper-note))]
                (and (= start-position (qpos->ppq (:position noon-event)))
                     (= (- end-position start-position) (qpos->ppq (:duration noon-event)))
                     (= (harmony/hc->chromatic-value (:pitch noon-event)) (:pitch reaper-note))))))))

    (defn retrieve-reaper-note [reaper-note score]
      (first (filter (partial equivalent-notes? reaper-note)
                     score)))

    (defn split-selection [score reaper-selected-notes]
      (reduce (fn [[selected remaining] n]
                (if-let [picked (retrieve-reaper-note n remaining)]
                  [(conj selected picked) (disj remaining picked)]
                  (throw (Exception. (str "not found note: " n)))))
              [#{} score] reaper-selected-notes)))

(do :updates

    (defn time-framed-upd [time-selection upd]
      (let [start (ppq->qpos (:start time-selection))]
        (lin {:position #(- % start)}
             upd
             {:position #(+ % start)})))

    (defn upd-selection! [& xs]
      (let [[time-selection reaper-notes] (<< [(ru.take.time-selection.get (ru.take.get-active))
                                               (ru.take.note-selection.get (ru.take.get-active))])
            [selected remaining] (split-selection @score* reaper-notes)
            updated (upd selected (if time-selection
                                    (time-framed-upd time-selection (lin* xs))
                                    (lin* xs)))]
        (reset! score*
                (into updated remaining))
        (<< (global T (ru.take.get-active))
            (ru.take.note-selection.delete-all T))
        (doseq [notes (partition-all 32 (score->notes updated))]
          (reaper/>> (ru.take.insert-notes T ~(vec notes))))))

    (defn sync-score! []
      (let [notes (score->notes @score*)]
        (<< (global T (ru.take.get-active))
            (ru.take.notes.clear T))
        (doseq [notes (partition-all 32 notes)]
          (reaper/>> (ru.take.insert-notes T ~(vec notes)))))))

(defn reset-score! [score]
  (noon/write-score score :filename REAPER_SYNC_MIDI_FILE)
  (reaper/>> (let [t ru.take
                   item (reaper.GetSelectedMediaItem 0 0)]
               (if item
                 (do (t.cursor.set (t.get-active) 0)
                     (reaper.DeleteTrackMediaItem (reaper.GetMediaItemTrack item) item)))
               (reaper.InsertMedia ~REAPER_SYNC_MIDI_FILE 0))))

(kbs/emit-bindings
 "emacs/cider-bindings.el"
 'noon-mode-map
 '{:score {:upd ["H-C-u" (upd-selection! *expr*)]}}
 'reaper-mode-map
 '{:score {:upd ["U" (reset-score! *expr*)]}
   :selection {:upd ["u" (upd-selection! *expr*)]}})

(comment (do :score

             (<< (global ru (u.reload :ruteal)))
             (<< (let [take ru.take
                       seq u.seq
                       t (take.get-active)]
                   (take.note-selection.delete-all t)))

             (do (reset! score* score0)
                 (sync-score!))

             (upd-score! (cat d1 d2 d3)
                         ($ (tup d0 d3 d6)))
             (upd-score! (cat s0 s1 s2 s3))

             (cat s0 d2 d4)
             (tup s0 s1-)
             (cat s0 s3)

             (upd-selection! ($ {:selected false}))
             (upd-selection! ($ (tup d1- d1 d3 d0)))
             (upd-score! ($ (tup d1 d3)))

             (require '[noon.lib.melody :as m])
             (upd-score!
              (lin (chans

                    [(patch :vibraphone)
                     vel3
                     (tupn 4 [(one-of IV II VI) tetrad (par [t2- vel5] s0 s1 s2 s3)])]

                    [(patch :ocarina)
                     vel5
                     (shuftup d1 d2 d3 d4 d5)
                     ($ (maybe (par d0 d3)))
                     (rup 16
                          (probs {(m/permutation :rand) 1
                                  (m/rotation :rand) 3
                                  (one-of* (map d-step (range -3 4))) 5}))])

                   (adjust 10)
                   (append [d2- (transpose c3)]
                           [d2 (transpose c3-)]
                           same)))))
