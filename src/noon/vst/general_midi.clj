(ns noon.vst.general-midi
  (:require [clojure.edn :as edn]
            [noon.utils.misc :as u]
            [clojure.string :as str]
            [clj-fuzzy.metrics :as fm]
            [clojure.java.io :as io]
            [noon.utils.pseudo-random :as pr]))

(defn name->key [name]
  (u/str->keyword (str/replace name #"\((.*)\)" "$1")))

(def instruments
  (->> (edn/read-string (slurp (io/resource "data/GM.edn")))
       (mapv (fn [i] (-> (update i :group u/str->keyword)
                         (assoc :key (name->key (:name i))))))))

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
        (pr/rand-nth instruments)
        (first (sort-by (fn [i] (fm/dice (name k) (:name i)))
                        > (pr/shuffle instruments))))))

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
                  (lin same (k (par s0 s2 s4)))]
                 (lin* (map patch (map (partial + 56) (range 24))))))

  (defn instrument-key [name]
    (-> (str/replace name #"\((.*)\)" "$1")
        (str/lower-case)
        (str/replace #" " "_")
        (str/replace #"-" "_")))

  (spit "resources/data/instruments.edn"
        (with-out-str (clojure.pprint/pprint (->> (edn/read-string (slurp (io/resource "data/GM.edn")))
                                                  (mapv (fn [i] [(:val i) (instrument-key (:name i))]))
                                                  (into {}))))))
