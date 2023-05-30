(ns noon.utils.socket
  (:import (java.net InetAddress DatagramPacket DatagramSocket))
  (:require [clojure.core.async :as async]))

(def socket (DatagramSocket.))

(defn message [host port text]
  (DatagramPacket. (.getBytes text)
                   (.length text)
                   (InetAddress/getByName host)
                   port))

(defn send-message
  ([port m]
   (send-message "127.0.0.1" port m))
  ([host port m]
   (.send socket (message host port m))))


(defn udp-chan [port]
  (let [buffer (async/chan)]
    (async/go
      (let [socket (DatagramSocket. port)
            packet (DatagramPacket. (byte-array 1024) 1024)]
        (while true
          (.receive socket packet)
          (async/>! buffer (new String (.getData packet) 0 (.getLength packet))))))
    buffer))

(defn listen-udp [port f]
  (let [buffer (udp-chan port)]
    (async/go-loop []
      (when-let [message (async/<! buffer)]
        (f message)
        (recur)))))

(comment
  (listen-udp 9997
              (fn [message] (println 9997 message)))

  (send-message 5555 "pingui"))
