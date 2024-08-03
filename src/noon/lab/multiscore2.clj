(ns noon.lab.multiscore2
  (:require [noon.score :as n]
            [noon.utils.multi-val :as m]
            [noon.utils.misc :as u]))

;; types of updates

(defmacro many->many
  {:clj-kondo/ignore true}
  [arg & body]
  `(with-meta
     (fn [~arg] ~@body)
     {:type ::many->many}))

(defn many->many? [x]
  (u/t? ::many->many x))

(defmacro one->many
  {:clj-kondo/ignore true}
  [arg & body]
  `(with-meta
     (fn [~arg] ~@body)
     {:type ::one->many}))

(defn one->many? [x]
  (u/t? ::one->many x))

(defmacro many->one
  {:clj-kondo/ignore true}
  [arg & body]
  `(with-meta
     (fn [~arg] ~@body)
     {:type ::many->one}))

(defn many->one? [x]
  (u/t? ::many->one x))

;; lift base operations

(defn update-multiscore [ms u]
  (cond (one->many? u) (m/bind ms u)
        (many->one? u) (m/once (u ms))
        (many->many? u) (u ms)
        :else (m/fmap ms (n/->upd u))))

(defn chain* [xs]
  (many->many ms
              (n/?reduce update-multiscore ms xs)))

(defn one-of* [xs]
  (many->many ms
              (m/bind (m/join* xs)
                      (fn [u] (update-multiscore ms u)))))

(def collect
  (many->one ms (n/concat-scores (m/get-all ms))))

(defn search [f]
  (many->many ms
    (m/such-that ms f)))

(defn pick [f]
  (many->many ms
              (if-let [s (m/get-1 (update-multiscore ms (search f)))]
                (m/once s))))

(defn mk* [xs]
  (update-multiscore (m/once n/score0) (chain* xs)))

(defn mk [& xs]
  (mk* xs))

(defn play [& xs]
  (n/noon {:play true}
          (m/get-1 (mk* xs))))
