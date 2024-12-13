(ns noon.client.sci-macros
  (:require [noon.utils.misc :refer [t]]))

(defn sfn
  {:sci/macro true}
  [_ _ arg & body]
  `(t :score-update
      (fn [~arg] ~@body)))

(defn sf_
  {:sci/macro true}
  [_ _ & body]
  `(t :score-update
      (fn [~'_] ~@body)))

(defn efn
  {:sci/macro true}
  [_ _ arg & body]
  `(t :event-update
      (fn [~arg] ~@body)))

(defn ef_
  {:sci/macro true}
  [_ _ & body]
  `(t :event-update
      (fn [~'_] ~@body)))

(defn !
  {:sci/macro true}
  [_ _ expression]
  (let [sym (gensym)]
    `(vary-meta ~(sfn nil nil sym `(noon.score/update-score ~sym ~expression))
                     assoc :non-deterministic true)))

(defn play
  {:sci/macro true}
  [_ _ & xs]
  `(noon.output/play-score (noon.score/mk ~@xs)))

(defn noon
  {:sci/macro true}
  [_ _ opts score]
  `(do ~@(when (:play opts) [`(noon.output/play-score ~score)])
       ~@(when-let [unsupported-keys (seq (keys (dissoc opts :play)))]
           [`(throw (js/Error. ~(str "Not supported noon options: " unsupported-keys)))])))

#_(def all
  {'sf_ #'sf_ 'sfn #'sfn
   'ef_ #'ef_ 'efn #'efn
   '! #'!
   'play #'play
   'noon #'noon})