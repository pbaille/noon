(ns noon.sci.namespaces)


(defn deref-def-bindings
  "Within sci evaluation,
   regular defs have to be dereferenced to be used by name.
   This function dereferences all defs in an ns-map."
  [ns-map]
  (update-vals ns-map (fn [v]
                        (if (seq (:arglists (meta v)))
                          v
                          (deref v)))))


(defn get-requirements
  "returns the content of a require ns clause, depending on the host platoform."
  [cljs?]
  (concat '([noon.updates :refer :all]
            [noon.events :as events :refer [ef_ efn]]
            [noon.score :as score :refer [score sf_ sfn e->s]]
            [noon.harmony :as hc]
            [noon.output.midi :as midi]
            [noon.numbers :refer [mul div add sub]]
            [noon.lib.harmony :as h]
            [noon.lib.melody :as m]
            [noon.lib.rythmn :as r]
            [noon.utils.misc :as u]
            [noon.constants :as constants]
            [noon.utils.pseudo-random :as rand]
            [noon.utils.sequences :as seqs]
            [noon.vst.general-midi]
            [clojure.math.combinatorics :as combinatorics])
          (if cljs?
            '([noon.sci.macros :refer :all]
              [noon.output :as out :refer [noon]])
            '([noon.utils.multi-val :as multi-val]
              [noon.vst.vsl :as vsl :refer [vsl]]
              [noon.output :as out :refer [noon play]]))))


(defmacro sci-namespaces []
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
                (get-requirements (:ns &env)))]

    (-> (dissoc namespaces :user)
        (assoc (list 'quote 'user)
               (cons `merge user)))))

(defn clj-ns-form [ns-sym]
  (list 'ns ns-sym
        (cons :require (get-requirements false))))

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
