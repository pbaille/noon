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

(defonce reaper-input-chan
  (socket/udp-chan 9997))

(defn reset-reaper-input-chan! []
  (alter-var-root #'reaper-input-chan
                  (fn [{:keys [socket channel]}]
                    (.close socket)
                    (async/close! channel)
                    (socket/udp-chan 9997))))

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

(defn get-response! [timeout]
  (let [timeout-chan (async/timeout timeout)
        [v p] (async/alts!! [(:buffer reaper-input-chan) timeout-chan])]
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

         (<< (math.floor (reaper.MIDI_GetPPQPosFromProjQN (ru.take.get-active) 1)))

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
      [action-tree]
      (->> (mapv (fn [[name _binding code]]
                   [name (cond (seq? code) (reg-action! name [code])
                               (int? code) code)])
                 action-tree)
           (into {})))

    (defn install-actions!
      [action-tree]
      (let [name->action-id (reg-actions! action-tree)]
        (spit "emacs/reaper-bindings.el"
              (with-out-str
                (clojure.pprint/pprint
                 (template (map! (:map reaper-mode-map
                                       :n "<escape>" (lambda () (interactive) (reaper-mode -1))
                                       ~@(mapcat (fn [[nam binding id]]
                                                   (if binding
                                                     [:desc (str/join "-" (map name nam)) :n binding
                                                      (template (lambda ()
                                                                        (interactive)
                                                                        (osc-send-message reaper-osc-client "/action" ~(get name->action-id nam))))]))
                                                 action-tree)))))))))

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

    (defn install-action-tree!
      [t]
      (->> (all-paths t)
           (filter (comp seq second))
           (mapv (fn [[p v]] (cons p v)))
           (install-actions!)))

    (defn install-edn-actions! []
      (install-action-tree!
       (read-string (slurp "emacs/reaper-actions.edn"))))

    (comment
      (all-paths (read-string (slurp "emacs/reaper-actions.edn")))))
