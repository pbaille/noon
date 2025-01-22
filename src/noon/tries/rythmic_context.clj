(ns noon.tries.rythmic-context
  (:require [noon.events :as events]
            [noon.utils.misc :as u]
            [noon.score :as score]))

(do :ctx

    (defn layer [resolution index length]
      {:resolution resolution
       :index index
       :length length})

    (defn mk
      "build a rythmic context given successive `layers`
       layer are vectors of the form [resolution index length]."
      [& layers]
      (mapv (fn [[resolution index length]]
              (layer resolution index length))
            layers))

    (defn tup
      "tupify `ctx` according to given `format`"
      [ctx format]
      (let [total (reduce + format)]
        (map (fn [index length]
               (conj ctx
                     {:resolution total
                      :index index
                      :length length}))
             (reductions + 0 format)
             format)))

    (defn ctx->pos-dur
      "given `ctx`, returns a vector [start-position duration]"
      [ctx]
      (reduce (fn [[pos dur] {:keys [resolution index length]}]
                (let [unit (/ dur resolution)]
                  [(+ pos (* index unit))
                   (* length unit)]))
              [0 1]
              ctx))

    (defn compare-ctxs
      "compare time bounds of `ctx1` and `ctx2`
       returns a vector of 2 elements such that:
       [(compare <ctx2-start-pos> <ctx1-start-pos>>)
        (compare <ctx-1-end-pos> <ctx2-end-pos>>)]"
      [ctx1 ctx2]
      (let [[pos1 dur1] (ctx->pos-dur ctx1)
            [pos2 dur2] (ctx->pos-dur ctx2)]
        [(compare pos2 pos1)
         (compare (+ pos1 dur1)
                  (+ pos2 dur2))]))

    (comment
      (tup []
           [1 2 1])

      (mapv ctx->pos-dur (tup []
                              [1 2 1]))))

(do :score

    (defn tup-score
      "tupify `score` according to `format`"
      [score format]
      (set (mapcat (fn [ctx]
                     (map (fn [e] (update e :time (fn [t] (into ctx t))))
                          score))
                   (tup [] format))))

    (defn split-score
      "split `score` into 2 scores.
       the first including events contained in the given `ctx`
       the second including events outside (or overlapping) `ctx`."
      [score ctx]
      (let [grouped (group-by (fn [event]
                                (let [[a b] (compare-ctxs ctx (:time event))]
                                  (or (= -1 a) (= -1 b))))
                              score)]
        [(set (grouped false)) (set (grouped true))]))

    (defn ->old-format [score]
      (set (map (fn [e]
                  (let [[position duration] (ctx->pos-dur (:time e))]
                    (assoc e :position position :duration duration)))
                score)))

    (defn score->timescale
      "Get the timescale of `score`
       (every events hold it under the :timescale key)"
      [score]
      (:timescale (first score) 1))

    (defn tup-scores
      "tups `scores` into one"
      [scores timescale]
      (set (mapcat (fn [score ctx]
                     (map (fn [e]
                            (-> (assoc e :timescale timescale)
                                (update :time (fn [t] (into ctx t)))))
                          score))
                   scores
                   (tup [] (map score->timescale scores)))))

    (u/defn* rtup
      "like regular tup but using rythmn-context"
      [updates]
      (score/sfn score
                 (tup-scores (mapv (fn [u] (score/update-score score u))
                                   updates)
                             (score->timescale score))))

    (defn rscale
      "This is not good, scale should be stored at the score lvl..."
      [ratio]
      (score/sfn score
                 (set (map (fn [e] (update e :timescale (fnil * 1) ratio))
                           score))))

    (u/defn* rlin
      "like regular lin but using rythmn-context"
      [updates]
      (score/sfn score
                 (let [scores (mapv (fn [u] (score/update-score score u))
                                    updates)]
                   (tup-scores scores
                               (reduce + (map score->timescale scores))))))

    (comment
      (require '[noon.output :refer [noon play]])
      (require '[noon.updates :as upds :refer [s0 s1 s2 d0 d1 d2]])
      (def score0 #{(assoc events/DEFAULT_EVENT
                           :time [])})
      (defn p [& xs]
        (let [score (reduce score/update-score score0 xs)]
          (noon {:play true
                 :bpm (/ 60 (score->timescale score))}
                (->old-format score))))

      (p (rlin s0 [s1 (rscale 2)] s2)
         (rtup d0 d1 d2))
      (play (upds/lin s0 [upds/dur2 s1] s2)
            (upds/tup d0 d1 d2)))

    (comment
      (def score0 #{(assoc events/DEFAULT_EVENT
                           :time [])})

      (noon.output/noon {:play true
                         :bpm 10}
                        (-> score0
                            (tup-score [1 2 1])
                            (tup-score [1 1 2])
                            (tup-score [1 1])
                            ->old-format)))

    (comment
      (mapv (fn [score]
              (set (map #(assoc % :pos-dur (map float (ctx->pos-dur (:time %)))) score)))
            (tup-score (set (map (fn [ctx] {:time ctx})
                                 (tup [] [1 2 1])))
                       [1 2 1]))

      (split-score (tup-score (set (map (fn [ctx] {:time ctx})
                                        (tup [] [1 2 1])))
                              [1 2 1])
                   (mk [4 1 2]))))
