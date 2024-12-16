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
   [sci.core :as sci]
   #?(:cljs [sci.async :as scia]))
  #?(:cljs (:require-macros [noon.eval :refer [sci-namespaces]])))

(defmacro sci-namespaces [& xs]
  (let [refer-map
        (fn [ns-sym refered-syms]
          (zipmap (map (fn [sym] (list 'quote sym))
                       refered-syms)
                  (map (fn [sym]
                         (list 'var (symbol (str ns-sym) (str sym))))
                       refered-syms)))

        {:as namespaces :keys [user]}
        (reduce (fn [ret [ns-sym & {:as opts}]]
                  (let [ns-publics-form `(ns-publics '~ns-sym)]
                    (merge (assoc ret `'~ns-sym ns-publics-form)
                           (when-let [as (:as opts)]
                             {`'~as ns-publics-form})
                           (when-let [refered-syms (:refer opts)]
                             {:user
                              (conj (:user ret)
                                    (if (= :all refered-syms)
                                      ns-publics-form
                                      (refer-map ns-sym refered-syms)))}))))
                {:user []}
                xs)]

    (-> (dissoc namespaces :user)
        (assoc (list 'quote 'user)
               (cons `merge user)))))

(def sci-ctx
  (sci/init
   {:namespaces (sci-namespaces
                 [noon.updates :refer :all]
                 [noon.events :as events]
                 [noon.score :as score :refer [mk]]
                 [noon.harmony]
                 [noon.output]
                 [noon.lib.harmony :as h]
                 [noon.lib.melody :as m]
                 [noon.lib.rythmn :as r])}))

(defn eval-string [x]
  #_(println sci-ctx)
  (try {:result (sci/eval-string* sci-ctx x)}
       (catch #?(:clj Exception
                 :cljs js/Error) e
         {:error e})))


(defn eval [x]
  (eval-string (str x)))

#?(:cljs (defn sci-eval-async [x on-success & [on-failure]]
           #_(.resume (get-audio-ctx))
           (.then (scia/eval-string* sci-ctx x)
                  (fn [ret] (on-success {:result ret}))
                  (fn [err] ((or on-failure
                                 on-success) {:error err})))))

#?(:cljs (defn stop-audio []
           (noon.midi/stop-midi)))

(comment
  (eval '(noon.score/mk))
  (eval '(mk))
  (eval '(tup s0)))

(comment
  (println "ui")
  (println (sci/eval-string* sci-ctx "(tup s0 s1 s2 s3)"))
  (println (sci/eval-string* sci-ctx "(output/play-score (score/mk (tup s0)))"))
  (println (sci/eval-string* sci-ctx "(sfn s (merge s s))"))
  (sci-play "(tup s0 s1 s2 s3)"))
