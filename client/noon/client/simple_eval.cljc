(ns noon.client.simple-eval
  #?(:clj (:require [cljs.env :as env]))
  #?(:cljs (:require-macros [noon.client.simple-eval :refer [analyzer-state]]))
  #?(:cljs (:require [cljs.js])))

(defmacro analyzer-state [[_ ns-sym]]
  `'~(get-in @env/*compiler* [:cljs.analyzer/namespaces ns-sym]))

#?(:cljs (do (def state (cljs.js/empty-state))

             (cljs.js/load-analysis-cache! state 'cljs.core (analyzer-state 'cljs.core))

             (cljs.js/eval-str state
                               "(require-macros '[cljs.core]) (let [x 1] (+ 1 x))"
                               nil
                               {:eval cljs.js/js-eval}
                               println)

             (eval '(+ 1 2))

             (cljs.js/eval state
                           '(+ 1 1)
                           {:eval cljs.js/js-eval}
                           println)))
