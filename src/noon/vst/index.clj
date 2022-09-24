(ns noon.vst.index
  (:require [noon.vst.chromaphone :as chroma]
            [noon.vst.general-midi :as gm]))

(defn pick [x]
  (let [ns (namespace x)
        nam (name x)]
    (case ns
      ("chroma" "chromaphone") (chroma/pick (keyword nam))
      nil [nil (:val (gm/get-instrument x))])))
