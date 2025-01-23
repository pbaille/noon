(ns noon.tries.rythmic-context
  (:require [noon.events :as events]
            [noon.utils.misc :as u]
            [noon.score :as score]))

(do :help

    (defn gcd [a b]
      (if (zero? b)
        a
        (recur b (mod a b))))

    (defn lcm [a b]
      (/ (* a b) (gcd a b)))

    (defn t> [id x]
      (tap> [id x])
      x))

(do :layer

    (defn layer [resolution index length]
      {:resolution resolution
       :index index
       :length length})

    (defn layer-simplify
      [{:keys [resolution index length]}]
      (let [divisor (reduce gcd [resolution index length])]
        (layer (/ resolution divisor)
               (/ index divisor)
               (/ length divisor))))

    (defn set-resolution
      [lr new-resolution]
      (let [{:keys [resolution index length]} (layer-simplify lr)]
        (when (zero? (rem new-resolution resolution))
          (let [m (quot new-resolution resolution)]
            (layer (* m resolution)
                   (* m index)
                   (* m length))))))

    (defn focus-layer
      [lr focus-lr]
      (let [simplified (layer-simplify lr)
            simplified-focus (layer-simplify focus-lr)
            common-res (* (:resolution simplified) (:resolution simplified-focus))
            lr' #tap (set-resolution lr common-res)
            focus-lr' #tap (set-resolution focus-lr common-res)]
        (layer-simplify
         (layer (- common-res (:index focus-lr'))
                (- (:index lr') (:index focus-lr'))
                (:length lr')))))

    (defn embed-layer [lr host-lr]
      (let [common-res (* (:resolution lr) (:resolution host-lr))
            lr' (set-resolution lr common-res)
            host-lr' (set-resolution host-lr common-res)
            embedding-ratio (/ (:length host-lr')
                               common-res)]
        (layer-simplify
         (layer common-res
                (+ (:index host-lr')
                   (* (:index lr')
                      embedding-ratio))
                (* (:length lr')
                   embedding-ratio)))))

    (comment

      (= (layer 2 1 1)
         (layer-simplify (layer 4 2 2))
         (set-resolution (layer 4 2 2) 2))

      (= nil
         (set-resolution (layer 4 2 1) 2)
         (set-resolution (layer 4 2 1) 5)
         (set-resolution (layer 4 2 1) 3))

      (= (set-resolution (layer 4 2 1) 8)
         {:resolution 8, :index 4, :length 2})

      (= (set-resolution (layer 4 2 2) 8)
         {:resolution 8, :index 4, :length 4})

      (focus-layer
       (layer 8 4 3)
       (layer 4 2 2))

      (focus-layer
       (layer 3 2 1)
       (layer 3 1 2))

      (layer-simplify (embed-layer
                       (layer 8 4 3)
                       (layer 4 1 2)))))

(do :ctx

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

    (do :rebase

        (defn rebase-ctx
          "Replace the first layers of `ctx` by `base-ctx`, adjusting remaining layers accordingly."
          [ctx base-ctx]
          (if-let [[bc1 & bcs] (seq base-ctx)]
            (let [den (lcm (:resolution (first ctx))
                           (:resolution bc1))]
              den)
            ctx))

        (rebase-ctx (mk [4 2 1] [5 2 3])
                    (mk [2 1 1])))

    (comment
      (tup []
           [1 2 1])

      (mapv ctx->pos-dur (tup []
                              [1 2 1]))))

(do :score

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

    (defn concat-scores
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
                 (concat-scores (mapv (fn [u] (score/update-score score u))
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
                   (concat-scores scores
                                  (reduce + (map score->timescale scores))))))

    (defn focus-subscore
      [ctx score]
      (assert (= 1 (count ctx))
              "Only single layer contexts are supported in focus-subscore")
      (let [[selection remaining] (split-score score ctx)]
        (set (map (fn [e]
                    (update e :time (fn [ectx]
                                      (update ectx 0 focus-layer (first ctx)))))
                  selection))))

    (defn embed-subscore
      [ctx score]
      (assert (= 1 (count ctx))
              "Only single layer contexts are supported in embed-subscore")
      (set (map (fn [e] (update e :time (fn [ectx]
                                          (update ectx 0 embed-layer (first ctx)))))
                score)))

    (defn parts
      [& ctx-update-pairs]
      (score/sfn score
                 (reduce (fn [score [ctx upd]]
                           (let [[selection remaining] (split-score score ctx)
                                 normalised-selection (focus-subscore ctx selection)
                                 updated-selection (score/update-score normalised-selection upd)
                                 repositioned-events (embed-subscore ctx updated-selection)]
                             (into remaining
                                   repositioned-events)))
                         score
                         (partition 2 ctx-update-pairs))))

    (comment
      (do :help
          (require '[noon.output :refer [noon play]])
          (require '[noon.updates :as upds :refer [s0 s1 s2 s3 s4 s5 s1- s2- s3- s4- s5-
                                                   d0 d1 d2 d3 d4 d5 d6 d1- d2- d3- d4- d5-
                                                   o1 o2 o1- o2-]])
          (def score0 #{(assoc events/DEFAULT_EVENT
                               :time [])})

          (defn mk-score [& updates]
            (reduce score/update-score score0 updates))

          (defn play-score [score]
            (noon {:play true
                   :bpm (/ 60 (score->timescale score))}
                  (->old-format score)))

          (defn p [& xs]
            (play-score (apply mk-score xs))))

      (t> :tup-score
          (mk-score (rlin s0 [s1 (rscale 2)] s2)
                    (rtup s0 s1 s2)))

      (play-score
       (tap> (focus-subscore
              (mk [3 1 2])
              (mk-score
               (rlin s0 [s1 (rscale 2)] s2)
               (rtup d0 d3- d3)))))

      (+ 1 2)

      (t> :sub-score
          (select-subscore
           (mk [2 1 1])
           (mk-score (rlin s0 [s1 (rscale 2)] s2)
                     (rtup s0 s1))))

      (t> :sub-embed-score
          (embed-subscore
           (mk [2 1 1])
           (select-subscore
            (mk [2 1 1])
            (mk-score (rlin s0 [s1 (rscale 2)] s2)
                      (rtup s0 s1)))))

      (p (rlin s0 s1 s2)
         (rtup d0 d1 d2)
         (parts (mk [3 1 2]) upds/o1))

      (p (rlin s0 [s1 (rscale 2)] s2)
         (rtup d0 d1 d2)
         (parts (mk [3 1 2]) (rtup upds/_ [(rscale 2) o1])))

      (noon {:play true
             :bpm 20}
            (->old-format
             (mk-score (rlin s0 s1 s2)
                       (parts (mk [3 1 1])
                              (rtup d0 [(rscale 2) d1] d2)
                              (mk [3 2 1])
                              upds/o1))))

      (sort-by ctx->pos-dur
               (map :time
                    (reduce score/update-score
                            score0
                            [(rlin s0 [s1 (rscale 2)] s2)
                             (rtup d0 d1 d2)
                             (parts (mk [4 1 2]) (rtup upds/_ upds/o1))])))
      (play (upds/lin s0 [upds/dur2 s1] s2)
            (upds/tup d0 d1 d2))))
