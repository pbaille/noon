(ns noon.client.utils.flat-tree
  "A flat tree is a map from path -> value, path is a vector of keys, value is anything.")

(defn child-path? [child parent]
  (= (vec parent)
     (vec (butlast child))))

(defn parent-path [path]
  (when (next path)
    (vec (butlast path))))

(defn subpath? [descendant parent]
  (or (child-path? descendant parent)
      (when-let [descendant-parent (seq (butlast descendant))]
        (subpath? descendant-parent parent))))

(defn update-recursively-from [tree path f]
  (into {}
        (map (fn [[node-path node]]
               (if (or (= node-path path)
                       (subpath? node-path path))
                 [node-path (f node)]
                 [node-path node]))
             tree)))

(defn update-children [tree path f]
  (into {}
        (map (fn [[node-path node]]
               (if (child-path? node-path path)
                 [node-path (f node)]
                 [node-path node]))
             tree)))
