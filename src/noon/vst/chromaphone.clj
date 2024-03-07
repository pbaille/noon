(ns noon.vst.chromaphone
  (:require [clojure.string :as str]
            [me.raynes.fs :as fs]
            [noon.utils.misc :as u]
            [clojure.pprint :as pp]))

(def banks
  (->> (fs/list-dir
       "/Users/pierrebaille/Library/Application Support/Applied Acoustics Systems/Chromaphone 2/Banks")
      (map (fn [file]
             (let [path (str file)
                   [idx name] (str/split (last (str/split path #"/")) #"\.")
                   name (str/trim name)
                   elems
                   (->> (str/split (slurp path) #"\n")
                        (keep (fn [l] (second (re-matches #"\{name=([^,]*).*" l))))
                        (mapv #(str/replace % #"\"" "")))]
               {:idx (read-string idx)
                :name name
                :key (u/str->keyword name)
                :elements (mapv (fn [i n] {:idx i :name n :key (u/str->keyword n)})
                                (range)
                                elems)})))

      (filter (comp number? :idx))
      (sort-by :idx)
      (mapv (fn [x] (update x :idx dec)))))

(comment (spit "data/chromaphone-banks.edn"
               (with-out-str (pp/pprint banks))))

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
    [(:idx bank) (rand-nth (range (count (:elements bank))))]
    (if-let [banks (categories (categorie-aliases k k))]
      (pick (rand-nth banks))
      (:coord (patch-by-key k)))))

(defn coord->key
  [[bank-idx patch-idx]]
  (:key (nth (:elements (first (filter #(= (:idx %) bank-idx) banks)))
             patch-idx)))

(coord->key (pick :sustained))
(coord->key (pick :smooth-carillon))

(def summary
  (map (fn [{:keys [key elements]}]
         [key (mapv :key elements)])
       banks))
