(ns noon.lib.reaper
  (:require [noon.score :as n]
            [noon.harmony :as harmony]
            [noon.utils.reaper.interop :as reaper :refer [<<]]
            #_[backtick :refer [template]]
            #_[noon.utils.misc :as u]
            [noon.midi :as midi]
            [noon.utils.emacs.cider-keybindings :as kbs]
            [clojure.string :as str]))

(def MIDI_RESOLUTION midi/MIDI_RESOLUTION)

(def NOON_MIDI_NOTE_ON_SHIFT 2)
(def NOON_MIDI_NOTE_OFF_SHIFT 1)

(def REAPER_SYNC_MIDI_FILE
  "/Users/pierrebaille/Code/WIP/noon/generated/reaper-sync.mid")

(def score* (atom n/score0))

(defn upd-score! [& xs]
  (swap! score*
         (n/lin* xs)))

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
            (assoc :start-position (+ start NOON_MIDI_NOTE_ON_SHIFT)
                   :end-position (+ end NOON_MIDI_NOTE_OFF_SHIFT)))))

    (defn score->notes [score]
      (mapv noon-note->reaper-note (n/numerify-pitches score))))

(do :get-reaper-selection

    (def equivalent-notes?
      (memoize
       (fn [reaper-note noon-event]
         (and (= (:channel noon-event) (:channel reaper-note))
              (= (:velocity noon-event) (:velocity reaper-note))
              (let [start-position (- (int (:start-position reaper-note)) 2)
                    end-position (- (int (:end-position reaper-note)) 1)]
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
        (n/lin {:position #(- % start)}
               upd
               {:position #(+ % start)})))

    (defn upd-selection! [& xs]
      (let [[time-selection reaper-notes] (<< [(ru.take.time-selection.get (ru.take.get-active))
                                               (ru.take.note-selection.get (ru.take.get-active))])
            [selected remaining] (split-selection @score* reaper-notes)
            updated (n/upd selected (if time-selection
                                      (time-framed-upd time-selection (n/lin* xs))
                                      (n/lin* xs)))]
        (reset! score*
                (into updated remaining))
        (<< (global T (ru.take.get-active))
            (ru.take.note-selection.delete-all T))
        (doseq [notes (partition-all 32 (score->notes updated))]
          (reaper/>> (ru.take.insert-notes T ~(vec notes))))))

    (defn upd-focus! [u]
      (let [score @score*
            focused-note (<< (ru.take.focused-note (ru.take.get-active)))]
        (if focused-note
          (let [noon-note (retrieve-reaper-note focused-note score)
                updated-subscore (n/upd #{noon-note} u)
                reaper-new-notes (score->notes updated-subscore)]
            (reset! score* (-> (disj score noon-note)
                               (into updated-subscore)))
            (<< (let [t ru.take
                      T (t.get-active)]
                  (t.delete-note T (. (t.focused-note T) :idx))
                  (t.insert-notes T ~reaper-new-notes)
                  (t.focus.closest-note T)))))))

    (defn sync-score! []
      (let [notes (score->notes @score*)]
        (<< (global T (ru.take.get-active))
            (ru.take.notes.clear T))
        (doseq [notes (partition-all 32 notes)]
          (reaper/>> (ru.take.insert-notes T ~(vec notes)))))))

(defn reset-score! [x]
  (let [score (cond (n/score? x) x
                    (n/score-update? x) (n/mk x)
                    :else n/score0)]
    (reset! score* score)
    (n/write-score {:filename REAPER_SYNC_MIDI_FILE} score)
    (reaper/>> (let [t ru.take
                     item (reaper.GetSelectedMediaItem 0 0)]
                 (if item
                   (do (t.cursor.set (t.get-active) 0)
                       (reaper.DeleteTrackMediaItem (reaper.GetMediaItemTrack item) item)))
                 (reaper.InsertMedia ~REAPER_SYNC_MIDI_FILE 0)))))

(kbs/emit-bindings
 "emacs/compiled/cider-bindings.el"
 'noon-mode-map
 '{:score {:upd ["H-C-u" (upd-selection! *expr*)]}}
 'reaper-mode-map
 '{:score {:upd ["U" (reset-score! *expr*)]}
   :selection {:upd ["u" (upd-selection! *expr*)]}
   :focus {:d1 ["M-k" (upd-focus! noon.score/d1)]
           :d1- ["M-j" (upd-focus! noon.score/d1-)]
           :s1 ["M-s-k" (upd-focus! noon.score/s1)]
           :s1- ["M-s-j" (upd-focus! noon.score/s1-)]
           :c1 ["M-s-k" (upd-focus! noon.score/c1)]
           :c1- ["M-s-j" (upd-focus! noon.score/c1-)]}})

(comment (do :score

             (<< (ru.take.note-selection.get (ru.take.get-active)))
             ()
             (score->notes @score*)
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
             (upd-selection! s1)
             (upd-score! ($ (tup d1 d3)))
             (upd-score! d1)
             (sync-score!)

             '(require '[noon.lib.melody :as m]))

         (do :noon-hydra
             (letfn [(symjoin [sep xs] (->> (map name xs) (str/join sep) symbol))
                     (hsym [segments] (symjoin "/" (cons :noon-hydra segments)))
                     (hbody-var [segments] (symbol (str "#'" (hsym segments) "/body")))
                     (hform [name-segments children]
                       (template (defhydra ~(hsym name-segments) (:color teal)
                                   ~(list "q" nil "quit" :exit true)
                                   ~@(when-let [back-desc (some-> (butlast name-segments) last name)]
                                       [(list "<escape>" (hbody-var (butlast name-segments)) back-desc)])
                                   ~@children)))
                     ($ [x f] (map f x))
                     ($* [x f] (mapcat f x))
                     (noon-action [context-update code] (list 'my-cider/eval! (str (list context-update code))))]

               (let [contexts [[:focus "f" `upd-focus!]
                               [:selection "s" `upd-selection!]
                               [:score "S" `upd-score!]]
                     levels [[:diatonic "d" `d-step]
                             [:chromatic "c"  `c-step]
                             [:structural "s" `s-step]]
                     directions [[:up "k" 1]
                                 [:down "j" -1]]]

                 (cons (hform []
                              ($ contexts
                                 (fn [[context key]] (list key (hbody-var [context]) (name context)))))
                       ($* contexts
                           (fn [[context _key update]]
                             (cons (hform [context]
                                          ($ levels
                                             (fn [[level key]] (list key (hbody-var [context level]) (name level)))))
                                   ($ levels
                                      (fn [[level _ step-fn]]
                                        (hform [context level]
                                               ($ directions
                                                  (fn [[_dir key delta]]
                                                    (list key (noon-action update (list step-fn delta)) :color 'red))))))))))))))
