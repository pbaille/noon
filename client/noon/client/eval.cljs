(ns noon.client.eval
  (:require
   [noon.score]
   [noon.lib.harmony]
   [noon.lib.melody]
   [noon.lib.rythmn]
   [sci.core :as sci])
  (:require-macros [noon.client.eval :refer [sci-namespace primitive-map]]))

(defn sci-eval [x]
  (sci/eval-string x {:namespaces {'user (primitive-map)
                                   'h (sci-namespace noon.lib.harmony)
                                   'm (sci-namespace noon.lib.melody)
                                   'r (sci-namespace noon.lib.rythmn)}}))

(defn sci-play [x]
  (sci-eval (str "(play-score (mk " x "))")))
