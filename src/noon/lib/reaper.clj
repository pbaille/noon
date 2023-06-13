(ns noon.lib.reaper
  (:use noon.score)
  (:require [noon.harmony :as harmony]
            [noon.utils.reaper :as reaper :refer [<<]]
            [backtick :refer [template]]))

(def REAPER_MIDI_RESOLUTION 960)

(defmacro nean [& xs]
  (let [score (vec (numerify-pitches (eval `(mk ~@xs))))]
    (template (>> (ru.take.insert-notes (ru.take.get-active)
                                        ~score)))))

(def score* (atom score0))

(defn convert-rationals [noon-event]
  (-> noon-event
      (update :position float)
      (update :duration float)))

(defn upd-score! [& xs]
  (swap! score*
         (lin* xs)))

(defn ppq-pos->q-pos [ppq]
  (/ ppq REAPER_MIDI_RESOLUTION))

(defn round [n]
  (Math/round (double n) ))

(defn q-pos->ppq-pos [q]
  (round (* q REAPER_MIDI_RESOLUTION)))

(defn noon-note->reaper-note
  [{:as e :keys [position duration]}]
  (let [start (q-pos->ppq-pos position)
        end (+ start (q-pos->ppq-pos duration))]
    (-> (dissoc e :position :duration)
        (assoc :start-position start
               :end-position end))))

(comment :old
         (defn almost-eq [x y]
           (or (= x y)
               (>= 1 (- x y) -1)))

         (defn equivalent-notes? [reaper-note noon-event]
           (and (= (:channel noon-event) (:channel reaper-note))
                (= (:velocity noon-event) (:velocity reaper-note))
                (let [start-position (:start-position reaper-note)
                      end-position (:end-position reaper-note)]
                  (and (almost-eq start-position (q-pos->ppq-pos (:position noon-event)))
                       (almost-eq (- end-position start-position) (q-pos->ppq-pos (:duration noon-event)))
                       (= (harmony/hc->chromatic-value (:pitch noon-event)) (:pitch reaper-note)))))))

(defn equivalent-notes? [reaper-note noon-event]
  (and (= (:channel noon-event) (:channel reaper-note))
       (= (:velocity noon-event) (:velocity reaper-note))
       (let [start-position (int (:start-position reaper-note))
             end-position (int (:end-position reaper-note))]
         (and (= start-position (q-pos->ppq-pos (:position noon-event)))
              (= (- end-position start-position) (q-pos->ppq-pos (:duration noon-event)))
              (= (harmony/hc->chromatic-value (:pitch noon-event)) (:pitch reaper-note))))))

(defn retrieve-reaper-note [reaper-note score]
  (first (filter (partial equivalent-notes? reaper-note)
                 score)))

(defn reaper-selection->split-score [xs]
  (reduce (fn [[selected remaining] n]
            (println "---")
            (if-let [picked (retrieve-reaper-note n remaining)]
              [(conj selected picked) (disj remaining picked)]
              (throw (Exception. (str "not found note: " n)))))
          [#{} @score*] xs))

(defn score->notes [score]
  (mapv noon-note->reaper-note (numerify-pitches score)))

(defn upd-selection! [& xs]
  (let [reaper-notes (<< (ru.take.note-selection.get (ru.take.get-active)))
        [selected remaining] (reaper-selection->split-score reaper-notes)
        updated (upd selected (lin* xs))]
    (reset! score*
            (into updated remaining))
    (<< (global T (ru.take.get-active))
        (ru.take.note-selection.delete-all T))
    (doseq [notes (partition-all 32 (score->notes updated))]
      (reaper/>> (ru.take.insert-notes T ~(vec notes))))))

(defn sync-score! []
  (<< (let [t (ru.take.get-active)]
        (ru.take.notes.clear t)
        (ru.take.insert-notes t ~(score->notes @score*))
        :ok)))


(comment (do :score

             (reset! score* score0)
             (sync-score!)
             (upd-score! (cat d1 d2 d3)
                         ($ (tup d0 d3 d6)))
             (upd-score! (cat s0 s1 s2 s3))

             (upd-selection! ($ {:selected false}))
             (upd-selection! ($ (tup d1- d1 d3 d0)))
             (upd-score! ($ (tup d1 d3)))))
