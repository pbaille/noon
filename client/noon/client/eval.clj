(ns noon.client.eval
  (:require [noon.updates]
            [noon.output]
            [noon.score]
            [noon.lib.harmony]
            [noon.lib.melody]
            [noon.lib.rythmn]))

(defmacro sci-namespace [ns-sym & [only?]]
  (->> (ns-publics ns-sym)
       (map (fn [[n v]] [n (meta v)]))
       (keep (fn [[n meta]] (let [ns-str (str (:ns meta))]
                              (when (= ns-str (name ns-sym))
                                [(list 'quote n) (symbol ns-str (name n))]))))
       (filter (fn [[n _]] (if only? (contains? (set only?) n) true)))
       (into {})))
