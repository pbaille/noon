(ns noon.client.eval
  (:require
   [cljs.env :as env])
  #?(:cljs (:require-macros [noon.client.eval :refer [analyzer-state]]))
  #?(:cljs (:require [cljs.js]
                     [cljs.reader :as reader]
                     [noon.client.user])))

(defmacro analyzer-state [[_ ns-sym]]
  `'~(get-in @env/*compiler* [:cljs.analyzer/namespaces ns-sym]))

#?(:cljs (do (defonce state (cljs.js/empty-state))

             (defn read-string [s]
               (reader/read-string s))

             (defn evaluate-string [source cb & {:as opts}]
               (cljs.js/eval-str state source nil
                                 (merge {:eval cljs.js/js-eval :context :expr}
                                        opts)
                                 cb))

             (defn evaluate [expr cb & {:as opts}]
               (println "evaluating " expr opts)
               #_(js/console.log (find-ns "expredit.user"))
               (cljs.js/eval state expr
                             (merge {:eval cljs.js/js-eval :context :expr}
                                    opts)
                             cb))

             (defn load-library-analysis-cache! []
               (cljs.js/load-analysis-cache! state 'cljs.core (analyzer-state 'cljs.core))
               (cljs.js/load-analysis-cache! state 'noon.client.user (analyzer-state 'noon.client.user))
               nil)

             (load-library-analysis-cache!)

             (comment

               (evaluate '(clojure.set/union #{} #{})
                         println)
               (evaluate 'expredit.eval/evaluate
                         println
                         {:ns 'expredit.eval})
               (evaluate 'noon.score/mk
                         println
                         {:ns 'expredit.user})
               (evaluate 'mk
                         println
                         {:ns 'expredit.user})
               (evaluate "(eval.library.core/my-inc 1)"
                         println)
               (do @state))))
