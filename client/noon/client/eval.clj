(ns noon.client.eval
  (:require [noon.score]))

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
                          [noon.score :refer [~@refered ~@aliased]])))))
