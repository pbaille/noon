(ns clj-doc
  (:require [clojure.string :as str]
            [zprint.core :as z]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

"Utils for converting org files to clojure files (regular and test)"

(do :help

    (defn parse-org-headline [line]
      (if-let [[_ stars title] (re-matches #"^(\*+) (.*)$"
                                           line)]
        {:level (count stars)
         :title title}))

    (defn ns-decl? [x]
      (and (seq? x)
           (= 'ns (first x))))

    (defn ns-form->file-path [x src-path]
      (and (ns-decl? x)
           (str/join "/" (cons src-path (str/split (second x) #"\.")))))

    (defn pretty-file! [file]
      (z/zprint-file file file file)))

(do :clj-doc-tree

    (defn file->title [filename]
      (-> filename
          (str/replace #"^\d+-" "")     ; remove digit prefix
          (str/replace #"\.md$" "")     ; remove .md extension
          (str/replace "_" " ")))

    (defn build-doc-tree [dir-path]
      (let [root (io/file dir-path)]
        (letfn [(process-file [file]
                  (let [title (file->title (.getName file))]
                    [title {:file (.getPath file)}]))

                (process-dir [dir]
                  (let [files (sort-by #(.getName %) (.listFiles dir))
                        index-file (io/file (str (.getPath dir) ".md"))
                        subdirs (filter #(.isDirectory %) files)
                        index-file-name? (set (map (fn [d] (str (.getName d) ".md")) subdirs))]
                    #_(clojure.pprint/pprint {:dirs subdirs :index-file index-file})
                    (vec (concat (process-file index-file)
                                 (keep (fn [f]
                                         (cond
                                           (.isDirectory f) (process-dir f)
                                           (and (.isFile f)
                                                (not (index-file-name? (.getName f)))) (process-file f)))
                                       files)))))]

          (process-dir root))))

    (defn build-cljdoc-tree []
      (spit "doc/cljdoc.edn"
            (with-out-str (pp/pprint {:cljdoc/languages ["clj"]
                                      :cljdoc.doc/tree [(build-doc-tree "doc/Noon")]})))))
