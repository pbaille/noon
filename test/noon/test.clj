(ns noon.test
  (:require [noon.score :as s]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [noon.utils.pseudo-random :as pr]
            [clojure.string :as str]))

(defn dir-exists? [dir-name]
  (let [dir (io/file dir-name)]
    (and (.exists dir) (.isDirectory dir))))

(defn dir-equal? [dir1 dir2]
  (let [files1 (sort (next (file-seq (io/file dir1))))
        files2 (sort (next (file-seq (io/file dir2))))]
    (and (= (count files1) (count files2))
         (every? identity (map #(= (slurp %1) (slurp %2)) files1 files2)))))

(def FREEZE_DIR "./test/frozen")

(defn frozen? [path score]
  (let [dir (str FREEZE_DIR "/" path)
        filename (str dir "/frozen")
        options {:filename filename :midi true}]

    (if (dir-exists? dir)
      (let [temp-dir (str FREEZE_DIR "/temp")]
        (fs/delete-dir temp-dir)
        (s/noon (assoc options :filename (str temp-dir "/frozen"))
                score)
        (dir-equal? dir temp-dir))
      (s/noon options score))))

(defmacro frozen
  ([id score-evaluating-expr]
   (let [dir (str/join "/"
                       (str/split (or (and id (namespace id))
                                      (str *ns*))
                                  #"\."))
         id-prefix (if id (str (name id) "__"))]
     `(frozen? ~(str dir "/" id-prefix (hash score-evaluating-expr))
               (pr/with-rand 0 ~score-evaluating-expr)))))

(defmacro frozen* [x & xs]
  (let [[id updates] (if (keyword? x)
                       [x xs]
                       [nil (cons x xs)])]
    `(frozen ~id
       (noon.score/mk ~@updates))))

(comment (macroexpand '(freezm (s/lin s/d0 s/d1)))
         (freezm (s/lin s/d0 s/d1)))
