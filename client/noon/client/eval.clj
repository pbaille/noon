(ns noon.client.eval
  (:require [noon.score]
            [noon.lib.harmony]
            [noon.lib.melody]
            [noon.lib.rythmn]))

(defn get-refered-varsyms []
  (->> (ns-publics 'noon.score)
       (map (fn [[n v]] [n (meta v)]))
       (filter (fn [[_ meta]] (-> (:ns meta) str (= "noon.score"))))
       (reduce (fn [ret [n meta]]
                 (let [tags (set (:tags meta))]
                   (if (seq tags)
                     (cond (tags :alias) (update ret :aliased conj n)
                           :else (update ret :refered conj n))
                     ret)))
               {:refered [] :aliased []})))

(defmacro user-ns-str []
  (let [{:keys [refered aliased]} (get-refered-varsyms)]
    (str `(~'ns noon.client.user
                (:require [noon.lib.harmony :as ~'h]
                          [noon.lib.melody :as ~'m]
                          [noon.lib.rythmn :as ~'r]
                          [noon.score :as ~'n :refer [~@refered ~@aliased]])))))

(defmacro primitive-map []
  (let [{:keys [refered aliased]} (get-refered-varsyms)]
    (into {}
          (map (fn [s]
                 [(list 'quote s) (symbol "noon.score" (name s))])
               (concat refered aliased)))))

(defmacro sci-namespace [ns-sym]
  (println (->> (ns-publics ns-sym)
       (map (fn [[n v]] [n (meta v)]))
       (keep (fn [[n meta]] (let [ns-str (str (:ns meta))]
                              (when (= ns-str (name ns-sym))
                                [(list 'quote n) (symbol ns-str (name n))]))))
       (into {})))
  (->> (ns-publics ns-sym)
       (map (fn [[n v]] [n (meta v)]))
       (keep (fn [[n meta]] (let [ns-str (str (:ns meta))]
                              (when (= ns-str (name ns-sym))
                                [(list 'quote n) (symbol ns-str (name n))]))))
       (into {})))
