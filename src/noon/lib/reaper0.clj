(ns noon.lib.reaper0
  (:use noon.score)
  (:require [noon.harmony :as harmony]
            [noon.utils.reaper :as reaper :refer [<<]]
            [backtick :refer [template]]
            [noon.utils.misc :as u]
            [noon.midi :as midi]
            [noon.score :as noon]
            [noon.utils.cider-keybindings :as kbs]
            [clojure.string :as str]))

(def MIDI_RESOLUTION midi/MIDI_RESOLUTION)

(def NOON_MIDI_NOTE_ON_SHIFT 2)
(def NOON_MIDI_NOTE_OFF_SHIFT 1)

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
            (assoc :start-position (+ start NOON_MIDI_NOTE_ON_SHIFT)
                   :end-position (+ end NOON_MIDI_NOTE_OFF_SHIFT)))))

    (defn score->notes [score]
      (mapv noon-note->reaper-note (numerify-pitches score))))

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

    (defn upd-focus! [u]
      (let [score @score*
            focused-note (<< (ru.take.focused-note (ru.take.get-active)))]
        (if focused-note
          (let [noon-note (retrieve-reaper-note focused-note score)
                updated-subscore (upd #{noon-note} u)
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
  (let [score (cond (score? x) x
                    (score-update? x) (mk x)
                    :else score0)]
    (reset! score* score)
    (noon/write-score score :filename REAPER_SYNC_MIDI_FILE)
    (reaper/>> (let [t ru.take
                     item (reaper.GetSelectedMediaItem 0 0)]
                 (if item
                   (do (t.cursor.set (t.get-active) 0)
                       (reaper.DeleteTrackMediaItem (reaper.GetMediaItemTrack item) item)))
                 (reaper.InsertMedia ~REAPER_SYNC_MIDI_FILE 0)))))

(kbs/emit-bindings
 "emacs/cider-bindings.el"
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

             (require '[noon.lib.melody :as m])
             (reset-score!
              (mk (chans

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
                          same))))

         (do :noon-hydra
             (letfn [(symjoin [sep xs] (->> (map name xs) (str/join sep) symbol))
                     (hsym [segments] (symjoin "/" (cons :noon-hydra segments)))
                     (hbody-var [segments] (symbol (str "#'" (hsym segments) "/body")))
                     (hform [name-segments children]
                       (template (defhydra ~(hsym name-segments) (:color teal)
                                   ("q" nil "quit" :exit true)
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
                           (fn [[context key update]]
                             (cons (hform [context]
                                          ($ levels
                                             (fn [[level key]] (list key (hbody-var [context level]) (name level)))))
                                   ($ levels
                                      (fn [[level _ step-fn]]
                                        (hform [context level]
                                               ($ directions
                                                  (fn [[dir key delta]]
                                                    (list key (noon-action update (list step-fn delta)) :color 'red)))))))))))))

         (do :just-hydra

             (def sample-hydra-def
               (template
                [:root "H"
                 {:description "this is the root"}
                 [:one "a"
                  [:a "a" (message "one a")]
                  [:b "b" (message "one b")]]
                 [:two "b" {:wrap-head ~(fn [x] (list 'progn x x))}
                  [:a "a" (message "two a")]
                  [:b "b" (message "two b")]
                  [:three "c" {:foo bar}
                   [:deep "d" (message "deep")]]]]))

             (def default-hydra-options
               {:color 'teal
                :wrap-head identity})

             (def hydra-option-keys [:color :pre :post :exit :foreign-keys :bind :hint :timeout])
             (def hydra-head-option-keys [:color :exit :bind :column])

             (do :help
                 (defn symjoin [sep xs] (->> (map name xs) (str/join sep) symbol))
                 (defn map->plist [m]
                   (mapcat identity m))
                 (defn hydra-sym [segments] (symjoin "/" (cons :noon-hydra segments)))
                 (defn hydra-body-varsym [segments] (symbol (str "#'" (hydra-sym segments) "/body")))
                 (defn hydra-description [h]
                   (or (:description (:hydra/options h))
                       (name (:hydra/name h))))
                 (defn options->plist [options]
                   (map->plist (select-keys options hydra-option-keys)))
                 (defn options->head-plist [options]
                   (map->plist (select-keys options hydra-head-option-keys)))
                 (defn merge-hydra-options [options {:as more :keys [wrap-head]}]
                   (-> (merge (dissoc options :description) (dissoc more :wrap-head))
                       (update :wrap-head (fn [x] (if wrap-head
                                                   (comp x wrap-head)
                                                   x))))))

             (comment :first-try
                      (defn parse-hydra [options [hydra-name key x & xs]]
                        (let [[opts body] (if (map? x) [x xs] [{} (cons x xs)])
                              next-options (merge-hydra-options options opts)]
                          (merge {:hydra/name hydra-name
                                  :hydra/key key
                                  :hydra/options next-options}
                                 (if (vector? (first body))
                                   {:hydra/body true
                                    :hydra/children (mapv (partial parse-hydra next-options) body)}
                                   {:hydra/head true
                                    :hydra/impl (first body)}))))
                      (defn compile-hydra-child
                        [at {:as h :hydra/keys [head options key impl name]}]
                        (if head
                          (let [wrap-head (:wrap-head options)]
                            (list* key (wrap-head impl) (hydra-description h) (options->head-plist options)))
                          (list key (hydra-body-varsym (conj at name)) (hydra-description h))))

                      (defn compile-hydra
                        [at {:as hydra :hydra/keys [options body head children wrap-head]}]
                        (let [at (conj at (:hydra/name hydra))]
                          (cons (template (defhydra ~(hydra-sym at) ~(options->plist options)
                                            ~(hydra-description hydra)
                                            ~@(map (partial compile-hydra-child at) children)))
                                (mapcat (partial compile-hydra at)
                                        (filter :hydra/body children)))))

                      (->> sample-hydra-def
                           (parse-hydra default-hydra-options)
                           (compile-hydra [])))

             (defn parse-hydra [from options [hydra-name key x & xs]]
               (let [[opts body] (if (map? x) [x xs] [{} (cons x xs)])
                     next-options (merge-hydra-options options opts)
                     from (conj from hydra-name)
                     base {:hydra/name (hydra-sym from)
                           :hydra/key key
                           :hydra/path from}]
                 (if (vector? (first body))
                   (cons (assoc base
                                :hydra/options (options->plist next-options)
                                :hydra/docstring (:doc opts)
                                :hydra/heads (map first body))
                         (mapcat (partial parse-hydra from next-options) body))
                   [(assoc base
                           :hydra/hint (or (:hint opts) (name hydra-name))
                           :hydra/options (options->head-plist opts)
                           :hydra/expr ((:wrap-head options) (first body)))])))

             (defn hydra-path-map [hydras]
               (reduce (fn [ret h] (assoc ret (:hydra/path h) h))
                       {} hydras))

             (defn hydra-compile-one [path path-map]
               (let [{:hydra/keys [heads name options]} (get path-map path)]
                 (when heads
                   (template (defhydra ~name ~options
                               ~@(map (fn [h] (let [{:hydra/keys [key hint expr name options path]} (get path-map (conj path h))]
                                               (if expr
                                                 (list* key expr hint options)
                                                 (list key (hydra-body-varsym path) hint))))
                                      heads))))))

             (defn hydra-compile [spec]
               (let [hydras (parse-hydra [] default-hydra-options spec)
                     path-map (hydra-path-map hydras)]
                 (keep (fn [[path h]] (hydra-compile-one path path-map)) path-map)))

             (hydra-compile sample-hydra-def)))
