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

(defn upd-score! [& xs]
  (swap! score*
         (lin* xs)))

(defn ppq-pos->q-pos [ppq]
  (/ ppq REAPER_MIDI_RESOLUTION))

(defn q-pos->ppq-pos [q]
  (* q REAPER_MIDI_RESOLUTION))

(defn equivalent-notes? [reaper-note noon-event]
  (and (= (:channel noon-event) (:channel reaper-note))
       (= (:velocity noon-event) (:velocity reaper-note))
       (let [start-position (ppq-pos->q-pos (:start-position reaper-note))
             end-position (ppq-pos->q-pos (:end-position reaper-note))]
         (and (= start-position (:position noon-event))
              (= (- end-position start-position) (:duration noon-event))
              (= (harmony/hc->pitch (:pitch noon-event)) (:pitch reaper-note))))))

(defn retrieve-reaper-note [reaper-note]
  (first (filter (partial equivalent-notes? reaper-note)
                 @score*)))

(defn reaper-selection->split-score [xs]
  (reduce (fn [[selected remaining] n]
            (if-let [picked (retrieve-reaper-note n remaining)]
              [(conj selected picked) (disj remaining picked)]
              (throw (Exception. (str "not found note: " n)))))
          [#{} @score*] xs))

(comment (require [])

         (<< (+ 4 5))

         (<< {:a 1 :b 2})

         (<< (global u (require :utils)))

         (<< (global u (u.reload :utils))
             (global ru (u.reload :ruteal)))

         (<< (ru.take.note-selection (ru.take.get-active)))

         (<< (ru.take.get-active))
         (<< (ru.take.time-selection (ru.take.get-active)))

         (numerify-pitches (mk (cat d1 d2 d3)))

         (nean (cat d1 d2 d3)))

(comment )



























(comment :json
         (require '[clojure.data.json :as json])

         (defn score->json [score]
           (-> score
               (score->reaper-notes)
               (json/write-str)))

         (defn mk-json [& xs]
           (score->json (mk* xs))))
