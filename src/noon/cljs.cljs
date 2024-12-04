(ns noon.cljs
  (:require [noon.utils.misc :as u]
            [noon.utils.pseudo-random :as pr]
            [noon.utils.chance :as ch]
            [noon.score :as n]))

(defn ^:export init []
  (js/console.log "loading noon cljs")
  (println (n/mk (n/tup n/s0 n/s1))))

#_(defn my-identity-fn
  {:doc (str "my" " identity" " fn")}
  [x] x)






(comment
  (pr/rand)
  (pr/rand-int 89)
  (pr/rand-int-between 8 89)
  (pr/shuffle (range 8)))
