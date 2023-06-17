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

(def state*
  (atom {:time-selection [0 0]
         :cursor [0 60]
         :grid 1/2
         :focus nil
         :score noon/score0}))

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

    (defn sync-selection! [score]
      (<< (let [seq u.seq
                tbl u.tbl
                t ru.take
                T (t.get-active)
                selected-notes ~(mapv noon-note->reaper-note (filter :selected score))]
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

    (add-watch state* :state-sync
               (fn [_ _ old {:as new :keys [score cursor time-selection grid]}]
                 (let [score-changed? (not (= score (:score old)))]
                   (if score-changed?
                     (noon/write-score score :filename REAPER_SYNC_MIDI_FILE))
                   (<< (let [t ru.take
                             T (t.get-active)]
                         (if ~score-changed?
                           (let [item (reaper.GetSelectedMediaItem 0 0)]
                             (if item
                               (do (t.cursor.set (t.get-active) 0)
                                   (reaper.DeleteTrackMediaItem (reaper.GetMediaItemTrack item) item)))
                             (reaper.InsertMedia ~REAPER_SYNC_MIDI_FILE 0)))
                         (if ~(not (= cursor (:cursor old)))
                           (t.focus.set T {:x ~(position->ppq (cursor 0)) :y ~(cursor 1)}))
                         (if ~(not (= time-selection (:time-selection old)))
                           (t.time-selection.set T
                                                 ~(position->ppq (time-selection 0))
                                                 ~(position->ppq (time-selection 1))))
                         (if ~(not (= grid (:grid old)))
                           (t.grid.set T ~(float grid)))
                         :ok))
                   (if score-changed?
                     (sync-selection! score))))))

(do :updates

    (defn upd-state! [& xs]
      (apply swap! state* xs))

    (defn upd-score! [& xs]
      (swap! state* update :score (noon/lin* xs)))

    (defn set-score! [& xs]
      (swap! state* assoc :score (noon/mk* xs))))

(defn framed-upd! [& xs]
  (let [[start end] (:time-selection @state*)]
    (if-not (= start end)
      (upd-score! (noon/parts (fn [{:keys [duration position]}]
                                (and (<= start position)
                                     (<= (+ position duration) end)))
                         (noon/lin (noon/start-from start)
                                   (noon/until (- end start))
                                   (noon/lin* xs)
                                   (noon/$ {:position (partial + start)})))))))

(do :focus-moves
    (def sort-score
      (memoize (partial sort-by (juxt :position :duration))))

    (defn my-split-with [f s]
      (loop [taken [] todo s]
        (if-let [[x & xs] (seq todo)]
          (if (f x)
            (recur (conj taken x) xs)
            [taken todo])
          [taken todo])))

    (defn get-sorted-score []
      (sort-score (:score @state*)))

    (defn set-focus! [{:as note :keys [position pitch]}]
      (if note
        (upd-state! assoc
                    :focus note
                    :cursor [position (harmony/hc->chromatic-value pitch)])))

    (defn focus-closest! []
      (if-not (:focus @state*)
        (let [[position pitch] (:cursor @state*)
              sorted (get-sorted-score)
              [before after] (my-split-with #(< (:position %) position) sorted)
              [positioned after] (my-split-with #(= position (:position %)) after)
              candidates (or (seq positioned)
                             (concat (first (partition-by :position (reverse before)))
                                     (first (partition-by :position after))))]
          (set-focus! (first (sort-by #(vector (Math/abs (float (- position (:position %))))
                                               (Math/abs (- pitch (:pitch %))))
                                      candidates))))))

    (defn focus-move! [delta]
      (let [focus (:focus @state*)
            sorted (get-sorted-score)
            [before after] (my-split-with (fn [e] (not (= e focus)))
                                          sorted)]
        (if (seq after)
          (set-focus! (if (neg? delta)
                        (nth (reverse before) (dec (- delta)) nil)
                        (nth after delta nil))))))

    (defn focus-upd! [efn]
      (let [{:as state :keys [score focus]} @state*
            new-focus (efn focus)]
        (upd-state! assoc
                    :focus new-focus
                    :score (-> score (disj focus) (conj new-focus)))))



    )

(do :keybindings
    (kbs/emit-bindings
       "emacs/cider-bindings.el"
       'reaper-mode-map
       '{:focus {:closest ["F" (focus-closest!)]
                 :fw ["l" (focus-move! 1)]
                 :bw ["h" (focus-move! -1)]
                 :s1 ["K" (focus-upd! noon/s1)]
                 :s1- ["J" (focus-upd! noon/s1-)]}
         :cursor {:fw ["f" (upd-reaper! (fn [{:as reaper :keys [grid focus]}]
                                          (update-in reaper [:focus 0] + grid)))]
                  :bw ["b" (upd-reaper! (fn [{:as reaper :keys [grid focus]}]
                                          (update-in reaper [:focus 0] - grid)))]}}))

(comment :tries

         (do :basics
             (set-score! (noon/cat noon/s0 noon/s1 noon/s2))
             (upd-score! (parts {:channel 1} {:selected true}))
             (upd-state! assoc :cursor [0 57] :grid 1/3)
             (framed-upd! ($ d3)))

         (do :focus
             (focus-upd! noon/s1)
             (focus-closest!)
             (focus-move! 1)
             (focus-move! -1)))
