(ns noon.test
  (:require [cljs.test :as t]
            #_[noon.doc.noon-org-test]
            [noon.lib.harmony-test]
            [noon.lib.melody-test]
            [noon.lib.rythmn-test]
            [noon.utils.chance-test]
            [noon.utils.contour-test]
            [noon.utils.euclidean-sums-test]
            [noon.utils.maps-test]
            [noon.utils.mapsets-test]
            [noon.utils.misc-test]
            [noon.utils.pseudo-random-test]
            [noon.utils.sequences-test]
            [noon.events-test]
            [noon.harmony-test]
            [noon.numbers-test]
            [noon.parse.harmony-test]
            [noon.score-test]
            [noon.updates-test]
            #_[noon.freezer :refer [freezer]]))

(defn all []
  (t/run-all-tests #"^noon\..*test$")
  #_@freezer)
