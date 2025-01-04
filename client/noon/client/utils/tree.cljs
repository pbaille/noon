(ns noon.client.utils.tree
  "NOT USED FOR NOW, REMOVE AT SOME POINT"
  (:refer-clojure :exclude [get rem]))

(def LEAF_KEY :value)
(def NODE_KEY :children)

(defn tree-path [x]
  (cond
    (empty? x) []
    (= NODE_KEY (first x)) x
    :else (interleave (repeat NODE_KEY) x)))

(defn node? [x]
  (contains? x NODE_KEY))

(defn leaf? [x]
  (contains? x LEAF_KEY))

(defn tree? [x]
  (or (node? x)
      (leaf? x)))

(defn get [tree pos]
  (if-not (seq pos)
    tree
    (get-in tree (tree-path pos))))

(defn upd
  ([tree pos f]
   (if (empty? pos)
     (f tree)
     (update-in tree (tree-path pos) f)))
  ([tree p f & pfs]
   (reduce (fn [t [p f]] (upd t p f))
           (upd tree p f)
           (partition 2 pfs))))

(defn put
  ([tree p v]
   (if (empty? p)
     v
     (assoc-in tree (tree-path p) v)))
  ([tree p v & pvs]
   (reduce (fn [t [p v]] (put t p v))
           (put tree p v)
           (partition 2 pvs))))
