(ns noon.sci.namespaces)

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

    (list `with-meta
          (-> (dissoc namespaces :user)
              (assoc (list 'quote 'user)
                     (cons `merge user)))
          {:ns-requirements (list 'quote xs)})))


(defn deref-def-bindings
  "Within sci evaluation,
   regular defs have to be dereferenced to be used by name.
   This function dereferences all defs in an ns-map."
  [ns-map]
  (update-vals ns-map (fn [v]
                        (if (seq (:arglists (meta v)))
                          v
                          (deref v)))))


;; This is a duplicated in noon.eval, have to find a better way
;; Should probably not be here, but babashka needs it to compile noon-org.clj
(def DEFAULT_REQUIREMENTS
  '(sci-utils/sci-namespaces
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
    #?(:cljs [noon.sci-macros :refer :all])))

(defn clj-ns-form [ns-sym]
  (list 'ns ns-sym
        (cons :require DEFAULT_REQUIREMENTS)))

(comment
  (macroexpand '(sci-namespaces
                 [noon.updates :refer :all]
                 [noon.events :as events :refer [ef_ efn]]
                 [noon.score :as score :refer [mk mk* sf_ sfn e->s]]
                 [noon.output :as out :refer [noon play]]
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
                 #?(:cljs [noon.sci-macros :refer :all]))))
