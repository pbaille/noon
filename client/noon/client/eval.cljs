(ns noon.client.eval
  (:require
   [noon.score]
   [noon.updates]
   [noon.output]
   [noon.lib.harmony]
   [noon.lib.melody]
   [noon.lib.rythmn]
   [sci.core :as sci])
  (:require-macros [noon.client.eval :refer [sci-namespace]]))

(def sci-ctx (sci/init {:namespaces {'user (sci-namespace noon.updates)
                                     'output (sci-namespace noon.output)
                                     'score (sci-namespace noon.score)
                                     'h (sci-namespace noon.lib.harmony)
                                     'm (sci-namespace noon.lib.melody)
                                     'r (sci-namespace noon.lib.rythmn)}}))

(defn sci-eval [x]
  (sci/eval-string sci-ctx x))

(comment (prn (sci/eval-string* sci-ctx "(tup s0 s1 s2 s3)")))

(defn sci-play [x]
  (sci-eval (str "(output/play-score (score/mk " x "))")))
