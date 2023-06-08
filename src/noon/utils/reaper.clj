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
         (<< (ru.take.grid.set T (/ 1 24)))

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

(def actions
  '{:grid {:toggle-triplet ()
           :double ()
           :half ()}
    :time-selection {:shift {:fw ()
                             :bw ()}
                     :shrink {:fw ()
                              :bw ()}
                     :grow {:fw ()
                            :bw ()}
                     :clear ()}
    :cursor {:step {:grid {:fw () :bw ()}
                    :level {:fw () :bw ()}
                    :parent {:fw () :bw ()}}
             :goto {:beginning ()
                    :end ()}}
    :note {:insert {}
           }})

(comment :direct-osc-actions

  (def action-prelude
    '[(global ru (require :ruteal))
      (global T (ru.take.get-active))])

  (defn reg-action! [name code]
    (let [lua-file (str "/Users/pierrebaille/Code/Lua/reascripts/" name ".lua")]
      (spit lua-file (fennel/compile-string (clojure.string/join "\n" (concat action-prelude code))))
      (<< (reaper.AddRemoveReaScript true 0 ~lua-file true))))

  (reg-action! "cursor-fw-grid-step"
               '[(ru.take.cursor.update T 1)])

  (defn reg-actions!
    [m]
    (let [name->id
          (->> (mapv (fn [[nam binding & code]]
                       [nam (reg-action! (name nam) (vec code))])
                     m)
               (into {}))]
      (spit "reaper-bindings.el"
            (template
             (progn (setq reaper-osc-client (osc-make-client "192.168.1.60" 8001))
                    (defvar reaper-mode-map (make-sparse-keymap))
                    (map! (:map reaper-mode-map
                                ~@(mapcat (fn [[nam binding id]]
                                            [:n binding
                                             (template (lambda ()
                                                               (interactive)
                                                               (osc-send-message reaper-osc-client "/action" ~(get name->id nam))))])
                                          m)))
                    (define-minor-mode reaper-mode
                      "reaper mode"
                      :init-value nil
                      :lighter " Reaper"
                      :keymap reaper-mode-map))))))

  (reg-actions!
   '[[:cursor-fw-grid-step "l"
      (ru.take.cursor.update T 1)]
     [:cursor-bw-grid-step "h"
      (ru.take.cursor.update T -1)]
     [:pitch-cursor-semitone-up "k"
      (let [me ru.midi-editor]
        (me.pitch-cursor.update (me.get-active) 1))]
     [:pitch-cursor-semitone-down "j"
      (let [me ru.midi-editor]
        (me.pitch-cursor.update (me.get-active) -1))]
     [:pitch-cursor-goto-closest-note "f"
      (let [log ru.misc.log
            me ru.midi-editor
            M (me.get-active)
            t ru.take
            cursor-ppq (t.cursor.get T)
            candidates (t.select-notes T (fn [n] (= n.start-position cursor-ppq)))
            pitch-cursor (me.pitch-cursor.get M)
            deltas (accumulate [ret [] i n (ipairs candidates)]
                               (do (table.insert ret (- n.pitch pitch-cursor))
                                   ret))]
        (table.sort deltas (fn [a b] (< (math.abs a) (math.abs b))))
        (me.pitch-cursor.update M (. deltas 1)))]
     [:time-selection-shift-fw "M-l"
      (ru.take.time-selection.update T nil 1)]
     [:time-selection-shift-bw "M-h"
      (ru.take.time-selection.update T nil -1)]
     [:time-selection-clear "M-d"
      (ru.take.time-selection.set T 0 0)]
     [:time-selection-at-cursor "M-i"
      (let [t ru.take
            grid-size (t.grid.get T)
            cursor-pos (t.cursor.get T)
            end-pos (+ cursor-pos (ru.time.qpos->ppq grid-size))]
        (ru.take.time-selection.set T cursor-pos end-pos))]]))
