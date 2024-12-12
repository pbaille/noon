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
   [noon.client.sci-macros])
  (:require-macros [noon.client.eval :refer [sci-namespace]]))

(def sci-ctx
  (sci/init
   {:namespaces {'user (merge (sci-namespace noon.updates)
                              (ns-publics 'noon.client.sci-macros)
                              {'mk #'noon.score/mk})
                 'noon.output (ns-publics 'noon.output)
                 'noon.score (sci-namespace noon.score)
                 'h (sci-namespace noon.lib.harmony)
                 'm (sci-namespace noon.lib.melody)
                 'r (sci-namespace noon.lib.rythmn)
                 'noon.utils.misc (ns-publics 'noon.utils.misc)
                 'events (ns-publics 'noon.events)
                 'noon.midi (ns-publics 'noon.midi)
                 'noon.vst.general-midi (ns-publics 'noon.vst.general-midi)
                 'noon.constants (ns-publics 'noon.constants)
                 'noon.utils.sequences (ns-publics 'noon.utils.sequences)}
    :bindings {}}))

#_ (println sci-ctx)
(defn sci-eval [x]
  #_(println sci-ctx)
  (let [evaluation (try {:result (sci/eval-string* sci-ctx x)}
                        (catch js/Error e {:error e}))]
    (println (assoc evaluation :form x))
    evaluation))

(comment
  (println "ui")
  (println (sci/eval-string* sci-ctx "(tup s0 s1 s2 s3)"))
  (println (sci/eval-string* sci-ctx "(output/play-score (score/mk (tup s0)))"))
  (println (sci/eval-string* sci-ctx "(sfn s (merge s s))"))
  (sci-play "(tup s0 s1 s2 s3)"))

(defn sci-play [x]

  (println (str "(output/play-score (score/mk " x "))"))
  (sci-eval (str "(output/play-score (score/mk " x "))")))
