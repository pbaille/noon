(ns noon.test
  (:require [noon.score :as s]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]))

(defn dir-exists? [dir-name]
  (let [dir (io/file dir-name)]
    (and (.exists dir) (.isDirectory dir))))

(defn dir-equal? [dir1 dir2]
  (let [files1 (sort (next (file-seq (io/file dir1))))
        files2 (sort (next (file-seq (io/file dir2))))]
    (and (= (count files1) (count files2))
         (every? identity (map #(= (slurp %1) (slurp %2)) files1 files2)))))

(def FREEZE_DIR "./test/data/freeze/")

(defn freeze [id score]
  (let [id (name id)
        dir (str FREEZE_DIR id)
        filename (str dir "/" id)
        options {:filename filename :midi true}]

    (if (dir-exists? dir)
      (let [temp-dir (str FREEZE_DIR "temp")]
        (fs/delete-dir temp-dir)
        (s/noon (assoc options :filename (str temp-dir "/" id))
                score)
        (dir-equal? dir temp-dir))
      (s/noon options score))))

(comment (freeze "one"
                 (s/mk (s/cat s/d0 s/d1))))
