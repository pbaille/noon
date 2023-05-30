(ns noon.lib.reaper
  (:use noon.score)
  (:require [noon.harmony :as harmony]
            [noon.utils.reaper :as reaper]))

(defn note-event->reaper-note
  [{:as event :keys [pitch position duration]} resolution]
  (when pitch
    (let [start-position (* position resolution)
          end-position (+ start-position (* duration resolution))]
      (assoc event
             :start-position start-position
             :end-position end-position
             :selected true
             :muted false))))

(def REAPER_MIDI_RESOLUTION 960)

(def scores* (atom {}))

(def notes* (atom {}))

(defn score->hash [score]
  (or (some-> score meta :hash)
      (hash score)))

(defn reg-score! [score]
  (let [score-hash (score->hash score)]
    (swap! scores* assoc score-hash score)
    (doseq [note score]
      (let [hash (hash (assoc note :score-hash score-hash))]
        (swap! notes* assoc hash (assoc note :hash hash))))))

(defn get-notes [score-hash]
  (mapv (fn [note]
          (-> (update note :pitch harmony/hc->chromatic-value)
              (note-event->reaper-note REAPER_MIDI_RESOLUTION)))
        (get @scores* score-hash)))

(comment :scratch

         (score->reaper-notes (mk (lin d2 d3))))

(defmacro nean
  [& xs]
  (let [score-data (noon.score/score->reaper-notes (eval `(noon.score/mk ~@xs)))]
    (compile-send (template (global score ~score-data)))))


(comment (>> (+ 4 5))

         (>> (global u (require :utils)))

         (>> (global u (u.reload :utils))
             (global ru (u.reload :ruteal)))

         (>> (ru.take.get-active))

         (require '[noon.score :as noon])

         (nean (noon/cat noon/d1 noon/d2 noon/d3))

         (>> (let [ru (u.reload :ruteal)
                   t (ru.take.get-active)
                   pos (ru.cursor.position t)]
               (each [_ n (ipairs score)]
                     (tset n :take t)
                     (ru.note.shift-position n pos)
                     (ru.note.insert n)
                     ))))

         (defn send-score-to-reaper []
           (let [midifiable (score->reaper-notes @score*)]))
















(comment :json
         (require '[clojure.data.json :as json])

         (defn score->json [score]
           (-> score
               (score->reaper-notes)
               (json/write-str)))

         (defn mk-json [& xs]
           (score->json (mk* xs))))
