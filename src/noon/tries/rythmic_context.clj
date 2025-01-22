(ns noon.tries.rythmic-context
  (:refer-clojure :exclude [cat]))

(def base-layer {:resolution 1 :index 0 :length 1})

(defn layer [resolution index length]
  {:resolution resolution
   :index index
   :length length})

(defn mk [& layers]
  (mapv (fn [[resolution index length]]
          (layer resolution index length))
        layers))

(defn split [ctx format]
  (let [total (reduce + format)]
    (map (fn [index length]
           (conj ctx
                 {:resolution total
                  :index index
                  :length length}))
         (reductions + 0 format)
         format)))

(split []
       [1 2 1])

(defn ctx->pos-dur [ctx]
  (reduce (fn [[pos dur] {:keys [resolution index length]}]
            (let [unit (/ dur resolution)]
              [(+ pos (* index unit))
               (* length unit)]))
          [0 1]
          ctx))

(mapv ctx->pos-dur (split []
                          [1 2 1]))

(defn split-score [score format]
  (mapv (fn [ctx]
          (set (map (fn [e] (update e :time (fn [t] (into ctx t))))
                    score)))
        (split [] format)))

(mapv (fn [score]
        (set (map #(assoc % :pos-dur (map float (ctx->pos-dur (:time %)))) score)))
      (split-score (set (map (fn [ctx] {:time ctx})
                             (split [] [1 2 1])))
                   [1 2 1]))

(defn compare-ctxs [c1 c2]
  (let [[pos1 dur1] (ctx->pos-dur c1)
        [pos2 dur2] (ctx->pos-dur c2)]
    [(compare pos2 pos1)
     (compare (+ pos1 dur1)
              (+ pos2 dur2))]))

(defn select-subscore [score ctx]
  (set (remove (fn [event]
                 (let [[a b] (compare-ctxs ctx (:time event))]
                   (or (= -1 a) (= -1 b))))
               score)))

(select-subscore (reduce into #{}
                         (split-score (set (map (fn [ctx] {:time ctx})
                                                (split [] [1 2 1])))
                                      [1 2 1]))
                 (mk [4 1 2]))
