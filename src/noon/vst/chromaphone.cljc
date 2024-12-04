(ns noon.vst.chromaphone
  (:require [noon.utils.pseudo-random :as pr]
            [noon.data.chromaphone :as data])
  #?(:clj (:require [clojure.string :as str]
                    [clojure.pprint :as pp]
                    [noon.utils.misc :as u]
                    [me.raynes.fs :as fs]
                    [clojure.java.io :as io])))

(def banks
  data/banks)


(def patches
  (mapcat (fn [{:keys [elements idx]}]
            (map (fn [e] (assoc e :bank idx :coord [idx (:idx e)]))
                 elements))
          banks))

(def bank-by-key
  (into {} (mapv (juxt :key identity) banks)))

(def patch-by-key
  (into {} (mapv (juxt :key identity) patches)))

(def categories
  {:percussed [:percusions :kicks :snares :toms :hi-hats :cymbals :chromakits]
   :sustained [:organs-and-pipes :synths :strings-and-pads]
   :attacked [:mallets :plucked-strings :keys :chimes-and-bells]})

(def categorie-aliases
  {:short :attacked
   :long :sustained
   :perc :percussed
   :drums :percussed})

(defn pick [k]
  (if-let [bank (bank-by-key k)]
    [(:idx bank) (pr/rand-nth (range (count (:elements bank))))]
    (if-let [banks (categories (categorie-aliases k k))]
      (pick (pr/rand-nth banks))
      (:coord (patch-by-key k)))))

(defn coord->key
  [[bank-idx patch-idx]]
  (:key (nth (:elements (first (filter #(= (:idx %) bank-idx) banks)))
             patch-idx)))

(def summary
  (map (fn [{:keys [key elements]}]
         [key (mapv :key elements)])
       banks))

#?(:clj (defn write-bank-file
          "Build the chromaphone banks edn representation and write it to resources/data/chromaphone-banks.edn."
          [bank-dir]
          (let [banks (->> (fs/list-dir bank-dir)
                           (map (fn [file]
                                  (let [path (str file)
                                        [idx name] (str/split (last (str/split path #"/")) #"\.")
                                        name (str/trim name)
                                        elems
                                        (->> (str/split (slurp path) #"\n")
                                             (keep (fn [l] (second (re-matches #"\{name=([^,]*).*" l))))
                                             (mapv #(str/replace % #"[\"\(\)]" ""))
                                             (mapv #(str/replace % #"@" "at")))]
                                    {:idx (read-string idx)
                                     :name name
                                     :key (u/str->keyword name)
                                     :elements (mapv (fn [i n] {:idx i :name n :key (u/str->keyword n)})
                                                     (range)
                                                     elems)})))

                           (filter (comp number? :idx))
                           (sort-by :idx)
                           (mapv (fn [x] (update x :idx dec))))]
            (spit (io/resource "data/chromaphone-banks.edn")
                  (with-out-str (pp/pprint banks))))))

(comment (write-bank-file
          "/Users/pierrebaille/Library/Application Support/Applied Acoustics Systems/Chromaphone 2/Banks")

         (coord->key (pick :sustained))
         (coord->key (pick :smooth-carillon)))
