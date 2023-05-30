(ns noon.utils.reaper
  (:require [noon.utils.socket :as socket]
            [noon.utils.fennel :as fennel]
            [bencode.core :as bc]
            [backtick :refer [template]]
            [clojure.string :as str]))

(def REAPER_HOST "127.0.0.1")
(def REAPER_PORT 9999)

(defn encode [data]
  (-> (doto (java.io.ByteArrayOutputStream.)
        (bc/write-bencode data))
      .toString))

(defn send [code]
  (socket/send-message REAPER_HOST REAPER_PORT
                       (encode {:code (str code)
                                :compiled (fennel/compile code)})))

(defmacro >>
  ([code]
   `(send '~code))
  ([x & xs]
   `(>> (do ~x ~@xs))))

(comment (>> (+ 4 5))

         (>> (global u (require :utils)))

         (>> (global u (u.reload :utils))
             (global ru (u.reload :ruteal)))

         (>> (ru.take.get-active))

         (require '[noon.score :as noon])

         (nean (noon/cat noon/d1 noon/d2 noon/d3))

         (>> (. (ru.take.insert-note (ru.take.get-active) {:pitch 47})
                :idx))
         (>> (let [ru (u.reload :ruteal)
                   t (ru.take.get-active)
                   pos (ru.cursor.position t)]
               (each [_ n (ipairs score)]
                     (tset n :take t)
                     (ru.note.shift-position n pos)
                     (ru.note.insert n)
                     ))))
