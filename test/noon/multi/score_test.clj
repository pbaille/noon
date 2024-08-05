(ns noon.multi.score-test
  (:use noon.multi.score)
  (:require [noon.utils.multi-val :as mv]
            [clojure.test :refer []]))

(comment
  (mk vel2)
  (mk d0)
  (mk (lin d0 d1 d2))
  (mk (lin d0 d1 d2)
      (each vel2))
  (mk (mv/join d0 d3))
  (mk (lin (mv/join d0 d3) d1 d2))
  (mk (lin d0 d1 d2)
      (each (mv/join vel4 vel2)))

  (mk (fit (lin d0 d1 d2)))
  (play (fit (lin d0 d1 d2)))
  (play (lin d0 d1 d2))

  (play (rep 4 (tup d0 (mv/join d3 d4))))
  (noon {:play true}
        (mv/once (concat-scores (mv/get-all (mk (rep 4 (tup d0 (mv/join d3 d4))))))))
  (count (mv/get-all (mk (rep 4 (tup d0 (mv/join d3 d4))))))
  (play (par _ {:channel 1} {:channel 2}))
  (play (par _ _))
  (play (lin _ _))
  (play (par [o1 _] {:channel 1} [o1- {:channel 2}]))
  (play (par [o1 _] {:channel 1} [o1- {:channel 2}])
        (parts {:channel 0} (tup s0 s1-)
               {:channel 1} (tup d0 d1 d2)))
  (play (chans o1 _ o1-)
        (parts chan0 (tup s0 s1-)
               chan1 (tup d0 d1 d2)
               chan2 _)))
