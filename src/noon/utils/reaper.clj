(ns noon.utils.reaper
  (:require [noon.utils.socket :as socket]
            [noon.utils.fennel :as fennel]
            [bencode.core :as bc]
            [backtick :refer [template]]
            [clojure.string :as str]
            [clojure.core.async :as async]
            [clojure.data.json :as json]))

(def REAPER_HOST "127.0.0.1")
(def REAPER_PORT 9999)

(defn encode [data]
  (-> (doto (java.io.ByteArrayOutputStream.)
        (bc/write-bencode data))
      .toString))

(defn message [code]
  {:code (str code)
   :compiled (fennel/compile code)})

(defn send [code]
  (socket/send-message REAPER_HOST REAPER_PORT
                       (encode (assoc (message code)
                                      :no-return "yes"))))

(defmacro >>
  ([code]
   `(send (template ~code)))
  ([x & xs]
   `(>> (do ~x ~@xs))))

(defonce reaper-input-chan
  (socket/udp-chan 9997))

(defn get-response! [timeout]
  (let [timeout-chan (async/timeout timeout)
        [v p] (async/alts!! [reaper-input-chan timeout-chan])]
    (if (= p timeout-chan)
      ::reaper-timeout
      (json/read-str v :key-fn keyword))))

(defn ask [code]
  (socket/send-message REAPER_HOST REAPER_PORT
                       (encode (message code)))
  (get-response! 2000))

(defmacro <<
  ([code]
   `(ask (template ~code)))
  ([x & xs]
   `(<< (do ~x ~@xs))))

(comment (do :checks

             (<< (+ 4 5))
             (reaper/>> (+ 1 2))

             (<< {:a 1 :b 2}))

         (<< (global u (u.reload :utils)))

         (<< (global u (require :utils))
             (global ru (u.reload :ruteal))
             (global T (ru.take.get-active))
             (global E (ru.midi-editor.get-active)))

         (<< (ru.take.get-active))
         (<< (ru.midi-editor.get-active))
         (<< (ru.take.clear T))
         (<< (ru.take.delete-selection T))

         (<< (ru.take.grid.get T))
         (<< (/ (ru.take.grid.get T) 2))
         (<< (ru.take.grid.set T (/ 1 24)))
         (<< (let [(_ bpi) (reaper.GetProjectTimeSignature2 0)]
               bpi))
         (<< (ru.take.grid.set T 0.125))
         (<< (let [x (ru.take.grid.get T)]
               (ru.misc.log x)
               (ru.take.grid.set T (/ x 2))))
         (<< (let [x (* 2 (ru.take.grid.get T))]
               (ru.take.grid.set T x)))


         (<< (ru.take.time-selection.get T))
         (<< (ru.take.time-selection.set T 0 0))
         (<< (ru.take.time-selection.update T nil 2))
         (<< (ru.take.time-selection.update T :fw 1))

         (<< (ru.take.cursor.get T))
         (<< (ru.take.cursor.set T 2))
         (<< (ru.take.cursor.update T 1))
         (<< (ru.take.cursor.update T -1))

         (<< (ru.midi-editor.pitch-cursor.get E))

         (>> (ru.take.insert-note T {:pitch 57 :position 1 :duration 3})))

(do :actions

    (def action-prelude
      '[(global u (require :utils))
        (global ru (require :ruteal))
        (global T (ru.take.get-active))])

    (defn action-name [x]
      (str "pb_"(cond (keyword? x) (name x)
                      (sequential? x) (str/join "_" (map name x)))))

    (defn reg-action! [name code]
      (let [lua-file (str "/Users/pierrebaille/Code/Lua/reascripts/" (action-name name) ".lua")]
        (spit lua-file (fennel/compile-string (clojure.string/join "\n" (concat action-prelude code))))
        (<< (reaper.AddRemoveReaScript true 0 ~lua-file true))))

    (defn reg-actions!
      [m]
      (let [name->id
            (->> (mapv (fn [[name binding code]]
                         [name (cond (seq? code) (reg-action! name [code])
                                     (int? code) code)])
                       m)
                 (into {}))]
        (spit "reaper-bindings.el"
              (with-out-str
                (clojure.pprint/pprint
                 (template
                  (progn (setq reaper-osc-client (osc-make-client "192.168.1.60" 8001))
                         (defvar reaper-mode-map (make-sparse-keymap))
                         (map! (:map reaper-mode-map
                                     ~@(mapcat (fn [[nam binding id]]
                                                 (if binding
                                                   [:desc (str/join "-" (map name nam)) :n binding
                                                    (template (lambda ()
                                                                      (interactive)
                                                                      (osc-send-message reaper-osc-client "/action" ~(get name->id nam))))]))
                                               m)))
                         (define-minor-mode reaper-mode
                           "reaper mode"
                           :init-value nil
                           :lighter " Reaper"
                           :keymap reaper-mode-map))))))))

    (comment
      (reg-action! "cursor-fw-grid-step"
                   '[(ru.take.cursor.update T 1)]))

    (defn all-paths
      ([m] (all-paths m []))
      ([x at]
       (if (map? x)
         (->> (mapcat (fn [[k v]] (all-paths v [k])) x)
              (map (fn [[p v]] [(concat at p) v])))
         [[at x]])))

    (all-paths actions)

    (defn reg-action-tree!
      [t]
      (->> (all-paths t)
           (filter (comp seq second))
           (mapv (fn [[p v]] (cons p v)))
           (reg-actions!)))

    (reg-action-tree!
     '{:repeat-last-command ["." 2999]
       :undo ["u" 40013]
       :redo ["C-r" 40014]

       :grid {:quarters [nil (ru.take.grid.set T 1)]
              :mul3 [nil (ru.take.grid.set T (* 3 (ru.take.grid.get T)))]
              :mul2 [nil (ru.take.grid.set T (* 2 (ru.take.grid.get T)))]
              :div2 [nil (ru.take.grid.set T (/ (ru.take.grid.get T) 2))]
              :div3 [nil (ru.take.grid.set T (/ (ru.take.grid.get T) 3))]}

       :time-selection {:shift {:fw ["M-l" (ru.take.time-selection.update T nil 1)]
                                :bw ["M-h" (ru.take.time-selection.update T nil -1)]}
                        :shrink {:fw [nil (ru.take.time-selection.update T :fw -1)]
                                 :bw [nil (ru.take.time-selection.update T :bw 1)]}
                        :grow {:fw [nil (ru.take.time-selection.update T :fw 1)]
                               :bw [nil (ru.take.time-selection.update T :bw -1)]}
                        :clear ["M-d" (ru.take.time-selection.set T 0 0)]}

       :cursor {:step {:grid {:fw ["l" (ru.take.cursor.update T 1)]
                              :bw ["h" (ru.take.cursor.update T -1)]}
                       :level {:fw [] :bw []}
                       :parent {:fw [] :bw []}}
                :goto {:beginning ["g g" (ru.take.focus.set T {:x 0 :y 60})]
                       :end []}}

       :pitch-cursor {:step {:semitone
                             {:up ["k" (let [me ru.midi-editor]
                                         (me.pitch-cursor.update (me.get-active) 1))]
                              :down ["j" (let [me ru.midi-editor]
                                           (me.pitch-cursor.update (me.get-active) -1))]}}}

       :note {:insert ["i" (let [t ru.take
                                 focus (t.focus.get T)
                                 grid (t.grid.get-ppq T)]
                             (t.insert-note T {:start-position focus.x
                                               :end-position (+ focus.x grid)
                                               :pitch focus.y}))]
              :step {:fw ["f" (ru.take.focus.next-note T)]
                     :bw ["b" (ru.take.focus.previous-note T)]}

              :toggle-selection ["s" (ru.take.set-note T (u.tbl.upd (ru.take.focused-note T)
                                                                    {:selected u.hof.not}))]

              :channel {:up ["c k" (ru.take.set-note T (u.tbl.upd (ru.take.focused-note T)
                                                                  {:channel (fn [c] (% (+ 1 c) 16))}))]
                        :down ["c j" (ru.take.set-note T (u.tbl.upd (ru.take.focused-note T)
                                                                    {:channel (fn [c] (% (- c 1) 16))}))]}

              :velocity {:up ["v k" (ru.take.set-note T (u.tbl.upd (ru.take.focused-note T)
                                                                   {:velocity (fn [v] (math.min 127 (+ 10 v)))}))]
                         :down ["v j" (ru.take.set-note T (u.tbl.upd (ru.take.focused-note T)
                                                                     {:velocity (fn [v] (math.min 127 (- v 10)))}))]}}

       :selection {:shrink {:fw []
                            :bw []}
                   :grow {:fw []
                          :bw []}}}))
