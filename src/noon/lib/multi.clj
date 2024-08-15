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

(declare update-multiscore)

(defn ->forking-update
  "Turn `x` into a forking-update if possible."
  [x]
  (if-let [update (n/->score-update x)]
    (forking-update score (if-let [updated-score (update score)]
                            (mv/once updated-score)
                            mv/none))
    (cond (forking-update? x) x
          (mv/multi-val? x) (forking-update s (mv/bind x (fn [update] (n/update-score s update))))
          (vector? x) (forking-update s (reduce update-multiscore (mv/once s) x)))))

(defn ->forking-update!
  "Turn `x` into a forking-update or throw."
  [x]
  (or (->forking-update x)
      (u/throw* `->forking-update " not convertible to forking-update: " x)))

(defn ->multi-update
  "Turn `x` into a multi-update if possible."
  [x]
  (if-let [forking-update (->forking-update x)]
    (multi-update multi-score (mv/bind multi-score forking-update))
    (cond (multi-update? x) x
          (mv/multi-val? x) (multi-update ms (mv/bind x (fn [update] (update-multiscore ms update))))
          (vector? x) (multi-update ms (reduce update-multiscore ms x)))))

(defn ->multi-update!
  "Turn `x` into a multi-update or throw."
  [x]
  (or (->multi-update x)
      (u/throw* `->multi-update " not convertible to multi-update: " x)))

(defn update-multiscore [multiscore update]
  ((->multi-update update) multiscore))

(do :composition

        (defn concat-multiscores
          "Concat several multiscores into one."
          [multiscores]
          (mv/fmap (mv/tup* multiscores)
                   n/concat-scores))

        (defn merge-multiscores
          "Merge several multiscores into one."
          [multiscores]
          (mv/fmap (mv/tup* multiscores)
                   n/merge-scores))

        (defn fit-multiscore
          [multiscore options]
          (mv/fmap multiscore
                   (fn [score] (n/fit-score score options)))))

(defn map-multiscore-update
  "map a multiscore `update` over `score`"
  [score update]
  (->> (map (fn [e]
              (mv/fmap (update-multiscore (mv/once #{(assoc e :position 0)})
                                          update)
                       (fn [score] (n/shift-score score (:position e)))))
            score)
       (reduce (fn [ret segment]
                 (mv/bind ret (fn [score] (mv/fmap segment (partial into score)))))
               (mv/once #{}))))

(defn partial-update-multiscore
  "Use 'filt to match some events of the score 's, apply 'x to the resulting subscore,
   then merge unselected events into the updated subscore.
   This second version allows you to provide an event update as a filter.
   If the result of the update is equal to the original event, it is considered a match."
  [multi-score filt upd]
  (mv/bind multi-score
           (fn [score]
             (let [grouped (group-by (n/->event-matcher filt) score)
                   common (set (get grouped false))
                   updated (update-multiscore (mv/once (get grouped true)) upd)]
               (mv/fmap updated
                        (fn [new-events] (into common new-events)))))))

(defn map-update
  "map an update over a multiscore"
  [multi-score update]
  (if-let [event-update (n/->event-update update)]
    (mv/fmap multi-score (fn [s] (n/map-event-update s event-update)))
    (if-let [score-update (n/->score-update update)]
      (mv/fmap multi-score (fn [s] (n/map-score-update s score-update)))
      (mv/bind multi-score (fn [s] (map-multiscore-update s update))))))

(defn update-score
  "Update a single `score` using `update`.
   If `update` is a multi-update, lift the `score` and apply `update` to it,
   then return the first score from the resulting multiscore."
  [score update]
  (if-let [upd (n/->score-update update)]
    (upd score)
    (if-let [upd (->forking-update update)]
      (mv/get-1 (upd score))
      (if-let [upd (->multi-update update)]
        (mv/get-1 (upd (mv/once score)))))))
