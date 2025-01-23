(ns noon.tries.rythmic-context
  (:require [noon.events :as events]
            [noon.utils.misc :as u]
            [noon.score :as score]
            [clojure.walk :as walk]))

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
            lr' (set-resolution lr common-res)
            focus-lr' (set-resolution focus-lr common-res)]
        (layer-simplify
         (layer (:length focus-lr')
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

    (defn time-context
      "build a rythmic context given successive `layers`
       layer are vectors of the form [resolution index length]."
      [& layers]
      (mapv (fn [[resolution index length]]
              (layer resolution index length))
            layers))

    (defn time-tup
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

    (defn time->pos-dur
      "given `ctx`, returns a vector [start-position duration]"
      [ctx]
      (reduce (fn [[pos dur] {:keys [resolution index length]}]
                (let [unit (/ dur resolution)]
                  [(+ pos (* index unit))
                   (* length unit)]))
              [0 1]
              ctx))

    (defn compare-time-contexts
      "compare time bounds of `ctx1` and `ctx2`
       returns a vector of 2 elements such that:
       [(compare <ctx2-start-pos> <ctx1-start-pos>>)
        (compare <ctx-1-end-pos> <ctx2-end-pos>>)]"
      [ctx1 ctx2]
      (let [[pos1 dur1] (time->pos-dur ctx1)
            [pos2 dur2] (time->pos-dur ctx2)]
        [(compare pos2 pos1)
         (compare (+ pos1 dur1)
                  (+ pos2 dur2))]))

    (defn cons-layer [ctx lr]
      (vec (cons lr ctx)))

    (defn remove-base-layer [ctx]
      (vec (next ctx)))

    (comment
      (time-tup []
                [1 2 1])

      (mapv time->pos-dur (time-tup []
                                    [1 2 1]))))

(do :score

    (defn split-score
      "split `score` into 2 scores.
       the first including events contained in the given `ctx`
       the second including events outside (or overlapping) `ctx`."
      [score ctx]
      (let [grouped (group-by (fn [event]
                                (let [[a b] (compare-time-contexts ctx (:time event))]
                                  (or (= -1 a) (= -1 b))))
                              score)]
        [(set (grouped false)) (set (grouped true))]))

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
                   (time-tup [] (map score->timescale scores)))))

    (defn focus-subscore
      [ctx score]
      (assert (= 1 (count ctx))
              "Only single layer contexts are supported in focus-subscore")
      (set (map (fn [e]
                  (update e :time (fn [ectx]
                                    (update ectx 0 focus-layer (first ctx)))))
                score)))

    (defn embed-subscore
      [ctx score]
      (assert (= 1 (count ctx))
              "Only single layer contexts are supported in embed-subscore")
      (set (map (fn [e] (update e :time (fn [ectx]
                                          (update ectx 0 embed-layer (first ctx)))))
                score)))

    (defn ->old-format [score]
      (set (map (fn [e]
                  (let [[position duration] (time->pos-dur (:time e))]
                    (assoc e :position position :duration duration)))
                score)))

    (defn ->timetree
      ([score]
       (->timetree score (comp :position :pitch)))
      ([score event->edn]
       (let [tree (reduce (fn [tree event]
                            (update-in tree (->> (:time event)
                                                 (map (juxt :resolution :index :length))
                                                 (interpose :children))
                                       update :content
                                       (fn [content]
                                         (conj (or content [])
                                               (event->edn event)))))
                          {} score)

             sort-tree (fn self [tree]
                         (when tree
                           (cons := (sort-by key
                                             (update-vals tree (fn [{:keys [children content]}]
                                                                 (if-let [children (self children)]
                                                                   (if content
                                                                     (conj content children)
                                                                     children)
                                                                   content)))))))]
         (sort-tree tree)))))

(do :updates

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

    (defn parts
      [& ctx-update-pairs]
      (score/sfn score
                 (reduce (fn [score [ctx upd]]
                           (let [[selection remaining] (split-score score ctx)
                                 focused-score (focus-subscore ctx selection)
                                 updated-subscore (score/update-score focused-score upd)
                                 embedded-subscore (embed-subscore ctx updated-subscore)]
                             (into remaining
                                   embedded-subscore)))
                         score
                         (partition 2 ctx-update-pairs))))

    (defn rmap
      [& xs]
      (score/sfn score
                 (as-> score _
                   (group-by (comp first :time) _)
                   (update-vals _ set)
                   (mapcat (fn [[layer score]]
                             (let [focused (focus-subscore [layer] score)
                                   updated (reduce score/update-score focused xs)
                                   embedded (embed-subscore [layer] updated)]
                               #_(clojure.pprint/pprint {:score (->timetree score)
                                                         :focused (->timetree focused)
                                                         :updated (->timetree updated)
                                                         :embedded (->timetree embedded)})
                               embedded))
                           _)
                   (set _))))

    (defn rmap2
      [& xs]
      (score/sfn score
                 (as-> score _
                   (group-by (comp first :time) _)
                   (update-vals _ set)
                   (mapcat (fn [[layer score]]
                             (let [focused (set (map #(update % :time remove-base-layer) score))
                                   updated (reduce score/update-score focused xs)
                                   embedded (set (map #(update % :time cons-layer layer) updated))]
                               embedded))
                           _)
                   (set _)))))


(comment :dev
         (do (require '[noon.output :refer [noon play]])
             (require '[noon.updates
                        :as upds
                        :refer [s0 s1 s2 s3 s4 s5 s1- s2- s3- s4- s5-
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
               (play-score (apply mk-score xs)))))

(comment

  (->timetree
   (mk-score (rtup s0 s1 s2)
             (rmap (rtup o1 o1-))
             #_(rscale 4))
   (comp :position :pitch))

  (play-score
   (mk-score (rtup s0 s1 s2)
             (rmap2 (rtup o1 o1-))
             (rscale 4)))

  (map (fn [e] (time->pos-dur (:time e)))
       (mk-score (rtup s0 s1 s2)
                 (rmap (rtup o1 o1-))))

  (->timetree
   (mk-score (rlin s0 [s1 (rscale 2)] s2)
             (rtup s0 s1))
   (comp :position :pitch))

  (t> :tup-score
      (mk-score (rlin s0 [s1 (rscale 2)] s2)
                (rtup s0 s1 s2)))

  (play-score
   (tap> (focus-subscore
          (time-context [3 1 2])
          (mk-score
           (rlin s0 [s1 (rscale 2)] s2)
           (rtup d0 d3- d3)))))

  (+ 1 2)

  (t> :sub-score
      (select-subscore
       (time-context [2 1 1])
       (mk-score (rlin s0 [s1 (rscale 2)] s2)
                 (rtup s0 s1))))

  (t> :sub-embed-score
      (embed-subscore
       (time-context [2 1 1])
       (select-subscore
        (time-context [2 1 1])
        (mk-score (rlin s0 [s1 (rscale 2)] s2)
                  (rtup s0 s1)))))

  (p (rlin s0 s1 s2)
     (rtup d0 d1 d2)
     (parts (time-context [3 1 2]) upds/o1))

  (p (rlin s0 [s1 (rscale 2)] s2)
     (rtup d0 d1 d2)
     (parts (time-context [3 1 2]) (rtup upds/_ [(rscale 2) o1])))

  (noon {:play true
         :bpm 20}
        (->old-format
         (mk-score (rlin s0 s1 s2)
                   (parts (time-context [3 1 1])
                          (rtup d0 [(rscale 2) d1] d2)
                          (time-context [3 2 1])
                          upds/o1))))

  (sort-by time->pos-dur
           (map :time
                (reduce score/update-score
                        score0
                        [(rlin s0 [s1 (rscale 2)] s2)
                         (rtup d0 d1 d2)
                         (parts (time-context [4 1 2]) (rtup upds/_ upds/o1))])))
  (play (upds/lin s0 [upds/dur2 s1] s2)
        (upds/tup d0 d1 d2)))
