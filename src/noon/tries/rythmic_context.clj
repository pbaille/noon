(ns noon.tries.rythmic-context)

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

    (mapv (fn [score]
            (set (map #(assoc % :pos-dur (map float (ctx->pos-dur (:time %)))) score)))
          (tup-score (set (map (fn [ctx] {:time ctx})
                               (tup [] [1 2 1])))
                     [1 2 1]))

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

    (split-score (tup-score (set (map (fn [ctx] {:time ctx})
                                      (tup [] [1 2 1])))
                            [1 2 1])
                 (mk [4 1 2])))
