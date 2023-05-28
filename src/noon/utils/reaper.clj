(ns noon.utils.reaper
  (:import (java.net InetAddress DatagramPacket DatagramSocket)
           (java.io ByteArrayOutputStream))
  (:require [bencode.core :as bc]
            [clojure.java.shell :refer [sh]]
            [backtick :refer [template]]
            [clojure.string :as str]))

(def socket (DatagramSocket.))

(defn message [text]
  (DatagramPacket. (.getBytes text)
                   (.length text)
                   (InetAddress/getByName "127.0.0.1")
                   9999))

(defn encode [data]
  (-> (doto (ByteArrayOutputStream.)
        (bc/write-bencode data))
      .toString))

(defn send-message [text]
  (.send socket (message (encode text))))

(defn compile-fennel-string [code-string]
  (->> (template (let [fen (require :fennel)
                       (compiled) (fen.compile-string ~code-string)]
                   compiled))
       (str)
       (sh "fennel" "-e")))

(defn compile-send [code]
  (let [code-string (str/replace (str code) "," " ")
         compiled (compile-fennel-string code-string)]
     (println "\n---\n" compiled)
     (if (not-empty (:err compiled))
       (println compiled)
       `(send-message {:code ~code-string
                       :compiled ~(:out compiled)}))))

(defmacro >>
  ([code]
   (compile-send code))
  ([x & xs]
   `(>> (do ~x ~@xs))))

(defmacro nean
  [& xs]
  (let [score-data (noon.score/score->reaper-notes (eval `(noon.score/mk ~@xs)))]
    (compile-send (template (global score ~score-data)))))


(comment (.length (str (noon/mk (noon/cat noon/d1 noon/d2 noon/d3))))

         (>> (+ 4 5))

         (>> (global u (require :utils)))

         (>> (global u (u.reload :utils))
             (global ru (u.reload :ruteal)))

         (>> (ru.take.get-active))

         (require '[noon.score :as noon])

         (nean (noon/cat noon/d1 noon/d2 noon/d3))

         (>> (let [ru (u.reload :ruteal)
                   t (ru.take.get-active)
                   pos (ru.cursor.position t)]
               (each [_ n (ipairs score)]
                     (tset n :take t)
                     (ru.note.shift-position n pos)
                     (ru.note.insert n)
                     ))))
