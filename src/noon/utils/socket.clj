(ns noon.utils.socket
  (:import (java.net InetAddress DatagramPacket DatagramSocket)))

(def socket (DatagramSocket.))

(defn message [host port text]
  (DatagramPacket. (.getBytes text)
                   (.length text)
                   (InetAddress/getByName host)
                   port))

(defn send-message [host port text]
  (.send socket (message host port text)))
