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
                                      :no-return true))))

(defmacro >>
  ([code]
   `(send '~code))
  ([x & xs]
   `(>> (do ~x ~@xs))))

(defonce reaper-input-chan
  (socket/udp-chan 9997))

(defn ask [code]
  (socket/send-message REAPER_HOST REAPER_PORT
                       (encode (message code)))
  (json/read-str (async/<!! reaper-input-chan)
                 :key-fn keyword))

(defmacro <<
  ([code]
   `(ask '~code))
  ([x & xs]
   `(<< (do ~x ~@xs))))

(comment

  (<< (let [notes (ru.take.notes (ru.take.get-active))]
        (each [_ n (ipairs notes)]
              (tset n :take nil))
        {:notes notes})))

(comment (>> (+ 4 5))

         (>> (global u (require :utils)))

         (>> (global u (u.reload :utils))
             (global ru (u.reload :ruteal)))

         (>> (ru.take.get-active))

         (require '[noon.score :as noon])

         (nean (noon/cat noon/d1 noon/d2 noon/d3))

         (>> (. (ru.take.insert-note (ru.take.get-active)
                                     {:pitch 57 :position 1 :duration 3})
                :idx))

         (>> (each [_ n (pairs (ru.take.notes (ru.take.get-active)))]
                   (ru.misc.log n)))

         (>> (let [ru (u.reload :ruteal)
                   t (ru.take.get-active)
                   pos (ru.cursor.position t)]
               (each [_ n (ipairs score)]
                     (tset n :take t)
                     (ru.note.shift-position n pos)
                     (ru.note.insert n)
                     ))))
