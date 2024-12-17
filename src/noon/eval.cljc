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
   [noon.utils.multi-val]
   [noon.vst.vsl]
   [noon.vst.general-midi]
   [sci.core :as sci]
   #?@(:cljs [[sci.async :as scia]
              [noon.macros]])
   #?(:clj [clojure.string :as str]))
  #?(:cljs (:require-macros [noon.eval :refer [sci-namespaces]])))

(do :utils

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
                    xs)

            requirements
            (mapv (fn [[ns-sym & {:as opts} :as reqv]]
                    (if (= :all (:refer opts))
                      [ns-sym :refer (vec (keys (ns-publics ns-sym)))]
                      reqv))
                  xs)]

        (list `with-meta
              (-> (dissoc namespaces :user)
                  (assoc (list 'quote 'user)
                         (cons `merge user)))
              {:ns-requirements (list 'quote requirements)})))

    (macroexpand '(sci-namespaces
                   [noon.updates :refer :all]
                   [noon.events :as events :refer [ef_ efn]]
                   [noon.score :as score :refer [mk mk* sf_ sfn e->s]]
                   [noon.output :as out :refer [noon play write]]
                   [noon.harmony :as hc]
                   [noon.numbers :refer [mul div add sub]]
                   [noon.lib.harmony :as h]
                   [noon.constants :as constants]
                   [noon.lib.melody :as m]
                   [noon.lib.rythmn :as r]
                   [noon.utils.misc :as u]
                   [noon.utils.pseudo-random :as rand]
                   [noon.utils.sequences :as seqs]
                   [clojure.math.combinatorics :as combinatorics]
                   #?@(:clj [[noon.utils.multi-val :as multi-val]
                             [noon.vst.vsl :as vsl :refer [vsl]]])
                   #?(:cljs [noon.macros :refer :all])))

    (defn deref-def-bindings_bu
      "Within sci evaluation in clojurescript,
       regular defs have to be dereferenced to be used by name.
       This function dereferences all defs in an ns-map.
       Has no effect in clojure."
      [ns-map]
      #?(:clj ns-map
         :cljs (update-vals ns-map (fn [v]
                                     (if (seq (:arglists (meta v)))
                                       v
                                       (deref v))))))

    (defn deref-def-bindings
      "Within sci evaluation in clojurescript,
       regular defs have to be dereferenced to be used by name.
       This function dereferences all defs in an ns-map.
       Has no effect in clojure."
      [ns-map]
      (update-vals ns-map (fn [v]
                            (if (seq (:arglists (meta v)))
                              v
                              (deref v))))))

(def default-namespaces
  (sci-namespaces
   [noon.updates :refer :all]
   [noon.events :as events :refer [ef_ efn]]
   [noon.score :as score :refer [mk mk* sf_ sfn e->s]]
   [noon.output :as out]
   [noon.harmony :as hc]
   [noon.numbers :refer [mul div add sub]]
   [noon.lib.harmony :as h]
   [noon.constants :as constants]
   [noon.lib.melody :as m]
   [noon.lib.rythmn :as r]
   [noon.utils.misc :as u]
   [noon.utils.pseudo-random :as rand]
   [noon.utils.sequences :as seqs]
   [clojure.math.combinatorics :as combinatorics]
   #?@(:clj [[noon.utils.multi-val :as multi-val]
             [noon.vst.vsl :as vsl :refer [vsl]]])
   #?(:cljs [noon.macros :refer :all])))

(defn fresh-context []
  (sci/init
   {:namespaces (update-vals default-namespaces deref-def-bindings)}))

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
  ([ctx x] (eval-string ctx (str x))))

#?(:cljs (do (defn eval-string-async [x on-success & [on-failure]]
               (.then (scia/eval-string* default-ctx x)
                      (fn [ret] (on-success {:result ret}))
                      (fn [err] ((or on-failure
                                     on-success) {:error err}))))
             (defn eval-async [x on-success & [on-failure]]
               (eval-string-async (str x) on-success on-failure))))

(defmacro play [& xs]
  `(let [{res# :result err# :error}
         (eval '~(vec xs))]
    (cond res# (noon.output/play-score (noon.score/mk* res#))
          err# err#)))

(defmacro noon [options score]
  `(let [{res# :result err# :error}
         (eval '~score)]
     (cond res# (noon.output/noon ~options res#)
           err# err#)))

#?(:clj (do :ns-spit

            (defn- ns->filename [ns-sym]
              (-> (name ns-sym)
                  (str/replace "." "/")
                  (str/replace "-" "_")))

            (defn spit-ns
              [{:keys [ns path content target]}]
              (spit (str path "/" (ns->filename ns) "." (or target "clj"))
                    (str/join "\n\n"
                              (cons (list 'ns 'noon.tries.generated
                                          (cons :require
                                                (cons '[noon.eval :refer [play noon]]
                                                      (:ns-requirements (meta default-namespaces)))))
                                    content))))

            (comment
              (spit-ns '{:ns noon.tries.spit-test
                         :path "src"
                         :content [(play (tup s0 s1))]}))))

(comment
  (play (tup s0 s1 s2))

  (macroexpand '(play (tup s0 s1 s2)))

  (noon {:midi true}
        (mk (tup s0 s1))))

(comment
  (eval '(noon.score/mk))
  (eval '(mk))
  (eval '(tup s0)))
