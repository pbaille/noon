(ns noon.client.eval
  (:require
   [noon.score]
   [noon.updates]
   [noon.output]
   [noon.lib.harmony]
   [noon.lib.melody]
   [noon.lib.rythmn]
   [noon.utils.misc]
   [sci.core :as sci]
   [noon.client.sci-macros :as sci-macros])
  (:require-macros [noon.client.eval :refer [sci-namespace]]))

(def sci-ctx
  (sci/init
   {:namespaces {'user (merge (sci-namespace noon.updates)
                              sci-macros/all)
                 'output (ns-publics 'noon.output)
                 'score (sci-namespace noon.score)
                 'h (sci-namespace noon.lib.harmony)
                 'm (sci-namespace noon.lib.melody)
                 'r (sci-namespace noon.lib.rythmn)
                 'noon.utils.misc (ns-publics 'noon.utils.misc)}
    :bindings {}}))

#_ (println sci-ctx)
(defn sci-eval [x]
  #_(println sci-ctx)
  (let [ret (sci/eval-string* sci-ctx x)]
    (println "evaluated " ret)
    ret))

(comment
  (println "ui")
  (println (sci/eval-string* sci-ctx "(tup s0 s1 s2 s3)"))
  (println (sci/eval-string* sci-ctx "(output/play-score (score/mk (tup s0)))"))
  (println (sci/eval-string* sci-ctx "(sfn s (merge s s))"))
  (sci-play "(tup s0 s1 s2 s3)"))

(defn sci-play [x]

  (println (str "(output/play-score (score/mk " x "))"))
  (sci-eval (str "(output/play-score (score/mk " x "))")))
