(ns noon.test
  (:require #_[noon.doc.noon-org-test]
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
            [noon.freezer :refer [freezer]]
            [noon.output]
            #?(:clj [cognitect.test-runner.api :as test-runner]
               :cljs [clojure.test :as t])))

(defn ^:export run-all [_]
  (swap! noon.output/options*
         assoc :mute true)
  #?(:cljs (t/run-all-tests #"^noon\..*test$")
     :clj (test-runner/test {:dirs ["test"]}))

  #_(println @freezer))

(comment
  (reset! freezer {})
  (test-runner/test {:dirs ["test"]})
  @freezer
  (run-all nil))
