(ns noon.utils.reaper.interop
  (:refer-clojure :exclude [send])
  (:require [noon.utils.socket :as socket]
            [noon.utils.fennel :as fennel]
            [bencode.core :as bc]
            [backtick :as bt]
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
  {:clj-kondo/ignore true}
  ([code]
   `(send (bt/template ~code)))
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
  {:clj-kondo/ignore true}
  ([code]
   `(ask (bt/template ~code)))
  ([x & xs]
   `(<< (do ~x ~@xs))))

(comment (do :checks

             (<< (+ 4 5))
             (>> (+ 1 2))

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
