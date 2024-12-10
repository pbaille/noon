(ns noon.client.sci-macros
  (:require [noon.score :as score]
            [noon.utils.misc :refer [t]]))

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
    `(vary-meta ~(sfn nil nil sym `(score/update-score ~sym ~expression))
                     assoc :non-deterministic true)))

(def all
  {'sf_ #'sf_ 'sfn #'sfn
   'ef_ #'ef_ 'efn #'efn
   '! #'!})
