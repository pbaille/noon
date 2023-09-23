(ns noon.utils.reaper
  (:require [noon.utils.socket :as socket]
            [noon.utils.fennel :as fennel]
            [bencode.core :as bc]
            [backtick :refer [template]]
            [clojure.string :as str]
            [clojure.core.async :as async]
            [clojure.data.json :as json]
            [noon.utils.misc :as u]
            [noon.score :as noon]))

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
         (<< (ru.take.focused-note T))

         (<< (ru.take.cursor.get T))
         (<< (ru.take.cursor.set T 2))
         (<< (ru.take.cursor.update T 1))
         (<< (ru.take.cursor.update T -1))

         (<< (ru.midi-editor.pitch-cursor.get E))

         (>> (ru.take.insert-note T {:pitch 57 :position 1 :duration 3})))

(do :actions

    (def REASCRIPT_PATH "/Users/pierrebaille/Code/Lua/reascripts")

    (def *actions (atom {}))

    (def action-prelude
      '[(global u (require :utils))
        (global ru (require :ruteal))
        (global T (ru.take.get-active))])

    (defn action-name [x]
      (str "pb_"(cond (keyword? x) (name x)
                      (sequential? x) (str/join "_" (map name x)))))

    (defn init-actions! [actions-file]
      (->> (slurp actions-file)
           (read-string)
           (u/all-paths)
           (map (fn [[path [binding code]]]
                  (let [action-name (action-name path)
                        action-id (if (int? code) code)
                        code (if-not action-id code)]
                    [(keyword action-name) {:action-name action-name
                                            :path path
                                            :binding binding
                                            :code code
                                            :action-id action-id}])))
           (into {})
           (reset! *actions)))

    (defn compile-action! [{:keys [path binding code]}]
      (let [action-name (action-name path)
            lua-file (str REASCRIPT_PATH "/" action-name ".lua")
            lua-source (fennel/compile-string (clojure.string/join "\n" (concat action-prelude [code])))]
        (spit lua-file lua-source)
        (swap! *actions assoc-in [(keyword action-name) :lua-file] lua-file)))

    (defn compile-actions!
      []
      (doseq [action (vals @*actions)]
        (if (:code action)
          (compile-action! action))))

    (defn compile-actions-loading-script []
      (let [name->lua-file
            (->> (vals @*actions)
                 (keep (fn [{:keys [action-name lua-file]}] (when lua-file [action-name lua-file])))
                 (into {}))
            action-count (count name->lua-file)
            action-reg-forms (map-indexed (fn [i [name file]]
                                            (template  (tset actions ~name (reaper.AddRemoveReaScript true 0 ~file ~(= (inc i) action-count)))))
                                          name->lua-file)]
        (fennel/compile-string (str '(local actions {})
                                    (cons 'do action-reg-forms)
                                    'actions))))

    (defn register-actions! [loading-script-path]
      (doseq [[name id] (<< (dofile ~loading-script-path))]
        (swap! *actions assoc-in [name :action-id] id)))

    (defn compile-emacs-bindings []
      (with-out-str
        (clojure.pprint/pprint
         (template (map! (:map reaper-mode-map
                               :n "<escape>" (lambda () (interactive) (reaper-mode -1))
                               ~@(mapcat (fn [{:keys [path binding action-id]}]
                                           (if binding
                                             [:desc (str/join "-" (map name path)) :n binding
                                              (template (lambda ()
                                                                (interactive)
                                                                (osc-send-message noon/reaper-osc-client "/action" ~action-id)))]))
                                         (vals @*actions))))))))

    (defn compile-hydra-bindings []
      ())

    (defn install-actions!
      [actions-file]
      (let [loading-script-path (str REASCRIPT_PATH "/load-pb-actions.lua")]
        (init-actions! actions-file)
        (compile-actions!)
        (spit loading-script-path (compile-actions-loading-script))
        (register-actions! loading-script-path)
        (spit "emacs/reaper-bindings.el" (compile-emacs-bindings))))

    (comment (def actions-file "emacs/reaper-actions.edn")
             (install-actions! "emacs/reaper-actions.edn")
             (compile-actions!)
             (init-actions! actions-file)
             (spit (str REASCRIPT_PATH "/load-pb-actions.lua") (compile-actions-loading-script)))
    (comment
      (vals @*actions)
      (slurp (str REASCRIPT_PATH "/load-pb-actions.lua"))
      (all-paths (read-string (slurp "emacs/reaper-actions.edn")))))
