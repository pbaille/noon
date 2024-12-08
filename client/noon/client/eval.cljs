(ns noon.client.eval
  (:require
   [cljs.env :as env]
   [cljs.js]
   [shadow.cljs.bootstrap.browser :as boot])
  (:require-macros [noon.client.eval :refer [user-ns-str]]))

(defonce compile-state-ref (env/default-compiler-env))

(defn evaluate-string [expr cb & {:as opts}]
  (cljs.js/eval-str
   compile-state-ref
   (str expr)
   "[test]"
   (merge {:eval cljs.js/js-eval
           :load (partial boot/load compile-state-ref)}
          opts)
   cb))

(defn eval-noon [source]
  (evaluate-string (str "(play-score (mk " source "))")
                   println
                   {:ns 'noon.client.user}))

(defn simple-play-test []
  (evaluate-string (str '(play-score (mk (let [x s2] (tup s0 s1 x)))))
                   println
                   {:ns 'noon.client.user}))

(defn init []
  (boot/init compile-state-ref
             {:path "bootstrap"}
             (fn []
               (evaluate-string (user-ns-str) println))))
