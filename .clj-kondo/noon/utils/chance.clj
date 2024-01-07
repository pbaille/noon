(ns noon.utils.chance
  (:require [clj-kondo.hooks-api :as api]))

(defn gen [{:keys [node]}]
  #_(prn (api/sexpr node))
  (let [body (rest (:children node))]
    {:node (api/list-node (list* (api/token-node `fn) (api/vector-node []) body))}))

(defmacro defgen
  ([name return]
   `(def ~name (gen ~return)))
  ([name x & xs]
   (let [[doc [x & xs]] (if (string? x) [x xs] [nil (cons x xs)])
         [attrs body] (if (map? x) [x xs] [nil (cons x xs)])
         arities (if (vector? (first body)) (list body) body)]
     `(defn ~name
        ~@(if doc [doc])
        ~@(if attrs [attrs])
        ~@(map (fn [[argv & body]] `(~argv (gen ~@body))) arities)))))

(defn defcoll [{:keys [node]}]
  (let [[name _ argv gen-expr] (rest (:children node))
        argv-node (or argv (api/vector-node []))
        gen-expr-node (or gen-expr (api/token-node nil))
        node (api/list-node (list (api/token-node `defn) name
                                  (api/list-node (list argv-node gen-expr-node))
                                  (api/list-node (list (api/vector-node (conj (vec (repeat (count (:children argv-node)) (api/token-node '_))) (api/token-node '&) (api/token-node '_)))
                                                       (api/token-node nil)))))]
    #_(prn (api/sexpr node))
    {:node node}))
