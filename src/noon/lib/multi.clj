(ns noon.lib.multi
  "tools to deal with multiple scores"
  (:require [noon.score :as n]
            [noon.utils.multi-val :as mv]
            [noon.utils.misc :as u]))

(defmacro forking-update
  "Creates a score -> multiscore update."
  [pat & body]
  `(with-meta (fn [~pat] ~@body)
     {:forking-update true}))

(defn forking-update?
  "Return true if `x` is a forking-update."
  [x]
  (some-> x meta :forking-update))

(defmacro multi-update
  "Creates a multiscore -> multiscore update."
  [pat & body]
  `(with-meta (fn [~pat] ~@body)
     {:multi-update true}))

(defn multi-update?
  "Return true if `x` is a multi-update."
  [x]
  (some-> x meta :multi-update))

(defn ->forking-update
  "Turn `x` into a forking-update if possible."
  [x]
  (if (forking-update? x) x
      (if-let [update (n/->score-update x)]
        (forking-update score (if-let [updated-score (update score)]
                                (mv/once updated-score)
                                mv/none)))))

(defn ->forking-update!
  "Turn `x` into a forking-update or throw."
  [x]
  (or (->forking-update x)
      (u/throw* `->forking-update " not convertible to forking-update: " x)))

(defn ->multi-update
  "Turn `x` into a multi-update if possible."
  [x]
  (if (multi-update? x) x
      (if-let [forking-update (->forking-update x)]
        (multi-update multi-score (mv/bind multi-score forking-update)))))

(defn ->multi-update!
  "Turn `x` into a multi-update or throw."
  [x]
  (or (->multi-update x)
      (u/throw* `->multi-update " not convertible to multi-update: " x)))
