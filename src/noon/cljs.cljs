(ns noon.cljs
  (:require [noon.score :as n]))

(def ^:export audio-ctx (new js/AudioContext))

(defn ^:export init []
  (js/console.log "loading noon cljs")
  (.resume audio-ctx))

(defn ^:dev/after-load reload []
  (.resume audio-ctx)
  (println :score
           (n/mk (n/tup n/s0 n/s1)))
  (n/play-cljs (n/tup n/s0 n/s1)))
