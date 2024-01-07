(ns noon.vst.general-midi
  (:require [clojure.edn :as edn]
            [noon.utils.misc :as u]
            [clojure.string :as str]
            [clj-fuzzy.metrics :as fm]))

(defn name->key [name]
  (u/str->keyword (str/replace name #"\((.*)\)" "$1")))

(def instruments
  (->> (edn/read-string (slurp "data/GM.edn"))
       (mapv (fn [i] (-> (update i :group u/str->keyword)
                        (assoc :key (name->key (:name i))))) )))

(def groups
  (into {}
        (map (fn [[group instruments]]
               [group (mapv #(dissoc % :group) instruments)])
             (group-by :group instruments))))

(def by-key
  (into {} (map (juxt :key #(dissoc % :key)) instruments)))

(defn get-instrument [k]
  (or (by-key k)
      (if-let [instruments (groups k)]
        (rand-nth instruments)
        (first (sort-by (fn [i] (fm/dice (name k) (:name i))) > (shuffle instruments))))))

(def summary
  (u/map-vals
   (partial mapv :key)
   (group-by :group instruments)))

#_(get-instrument :piano)

(comment
  (spit "data/gm-summary.edn" (with-out-str (clojure.pprint/pprint summary)))

  (comment :soundfont-tests
           '(use 'noon.score)
           (play dur2
                 [o1- (rup 7 d1)
                  (cat same (k (par s0 s2 s4)))]
                 (cat* (map patch (map (partial + 56) (range 24)))))))
