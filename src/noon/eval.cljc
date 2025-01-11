(ns noon.eval
  (:refer-clojure :exclude [eval])
  (:require
   [noon.score]
   [noon.events]
   [noon.updates]
   [noon.output]
   [noon.midi]
   [noon.lib.harmony]
   [noon.lib.melody]
   [noon.lib.rythmn]
   [noon.utils.misc]
   [noon.vst.general-midi]
   [sci.core :as sci]
   [noon.utils.pseudo-random]
   [noon.sci.namespaces :as namespaces]
   #?@(:cljs [[sci.async :as scia]
              [noon.sci.macros]])
   #?@(:clj [[noon.vst.vsl]
             [noon.utils.multi-val]]))
  #?(:cljs (:require-macros [noon.eval :refer [play noon]])))

(def default-namespaces
  (namespaces/sci-namespaces
   [noon.updates :refer :all]
   [noon.events :as events :refer [ef_ efn]]
   [noon.score :as score :refer [score sf_ sfn e->s]]
   [noon.output :as out :refer [noon #?(:clj play)]]
   [noon.harmony :as hc]
   [noon.midi :as midi]
   [noon.numbers :refer [mul div add sub]]
   [noon.lib.harmony :as h]
   [noon.lib.melody :as m]
   [noon.lib.rythmn :as r]
   [noon.utils.misc :as u]
   [noon.constants :as constants]
   [noon.utils.pseudo-random :as rand]
   [noon.utils.sequences :as seqs]
   [noon.vst.general-midi]
   [clojure.math.combinatorics :as combinatorics]
   #?@(:clj [[noon.utils.multi-val :as multi-val]
             [noon.vst.vsl :as vsl :refer [vsl]]])
   #?(:cljs [noon.sci.macros :refer :all])))

(defn fresh-context []
  (sci/init
   {:namespaces (update-vals default-namespaces namespaces/deref-def-bindings)}))

(def default-ctx
  (fresh-context))

(defn eval-string
  "Evaluate a string of noon code."
  ([x]
   (eval-string default-ctx x))
  ([ctx x]
   (try {:result (sci/eval-string* ctx x)}
        (catch #?(:clj Exception
                  :cljs js/Error) e
          {:error e}))))

(defn eval
  "Evaluate a noon expression."
  ([x] (eval default-ctx x))
  ([ctx x]
   (try {:result (sci/eval-form ctx x)}
        (catch #?(:clj Exception
                  :cljs js/Error) e
          {:error e}))))

(defn eval-and-return
  ([x] (eval-and-return default-ctx x))
  ([ctx x] (let [{:keys [result error]} (eval ctx x)]
             (if error
               (throw error)
               result))))

(defmacro score [& xs]
  `(let [{res# :result err# :error}
         (eval '~(vec xs))]
     (cond res# (noon.score/score* res#)
           err# err#)))

(defmacro play [& xs]
  `(let [{res# :result err# :error}
         (eval '~(vec xs))]
     (cond res# (noon.output/noon {:play true} (noon.score/score* res#))
           err# err#)))

(defmacro noon [options score]
  `(let [{res# :result err# :error}
         (eval '~score)]
     (cond res# (noon.output/noon ~options res#)
           err# err#)))

(defn stop []
  (noon.output/stop))

#?(:cljs (do (defn eval-string-async [x on-success & [on-failure]]
               (.then (scia/eval-string* default-ctx x)
                      (fn [ret] (on-success {:result ret}))
                      (fn [err] ((or on-failure
                                     on-success) {:error err}))))))



(comment
  (play (tup s0 s1 s2))

  (macroexpand '(play (tup s0 s1 s2)))

  (noon {:midi true}
        (mk (tup s0 s1)))

  (eval '(noon.score/score))
  (eval '(mk))
  (eval '(tup s0))
  (play Eb0)
  (eval '(defn pouetpouet [] "poeut")))
