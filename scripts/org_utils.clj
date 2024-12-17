(ns org-utils
  (:require [clojure.string :as str]
            [zprint.core :as z]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [zprint.core :as zp]))

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
           (str/join "/" (cons src-path (str/split (second x) #"\."))))))

(do :org->clj
    (defn org->clj [org-file clj-file]
      (let [s (slurp org-file)
            lines (str/split-lines s)
            tree (reduce (fn [{:as state :keys [level ret in-block]} line]
                           (if (= :end line)
                             (str ret (str/join (repeat level "]")))
                             (let [prefix (str (when (pos? level) "\n ") (str/join (repeat level " ")))]
                               (if-let [{headline-lvl :level title :title}
                                        (parse-org-headline line)]
                                 (if (> headline-lvl level)
                                   {:level headline-lvl
                                    :ret (str ret prefix "[\"" title "\"")}
                                   (assoc state :ret
                                          (str ret "\n"
                                               ;; closing
                                               (str/join (repeat level " "))
                                               (str/join (repeat (inc (- level headline-lvl)) "]"))
                                               "\n\n"
                                               ;; opening
                                               (str/join (repeat headline-lvl " "))
                                               "[\"" title "\"")
                                          :level headline-lvl))
                                 (cond (= "" line) (assoc state :ret (str ret "\n"))
                                       (str/starts-with? line "#+begin_src clojure") (assoc state :in-block true)
                                       (str/starts-with? line "#+end_src") (assoc state :in-block false)
                                       in-block (assoc state :ret (str ret prefix line))
                                       :else (assoc state :ret
                                                    (str ret prefix ";; " (str/replace line #"\n" (str prefix ";; ")))))))))
                         {:level 0 :in-block false :ret "'"}
                         (conj (vec lines)
                               :end))
            [_ ns-decl] (re-find #"(?s)#\+begin_src.*?\n(.*?)#\+end_src" s)]
        (spit clj-file (str ns-decl "\n\n" tree))))

    (defn org-str->clj-str
      "Build a `clj-file` from an `org-file`
  Assumes that the first code block of the org-file is a valid clojure ns form."
      [org-str]

      (let [lines (str/split-lines org-str)
            tree (reduce (fn [{:as state :keys [level ret in-block]} line]
                           (if (= :end line)
                             (str ret (str/join (repeat level "]")))
                             (let [prefix (str (when (pos? level) "\n ") (str/join (repeat level " ")))]
                               (if-let [{headline-lvl :level title :title}
                                        (parse-org-headline line)]
                                 (if (> headline-lvl level)
                                   {:level headline-lvl
                                    :ret (str ret prefix "[\"" title "\"")}
                                   (assoc state :ret
                                          (str ret "\n"
                                               ;; closing
                                               (str/join (repeat level " "))
                                               (str/join (repeat (inc (- level headline-lvl)) "]"))
                                               "\n\n"
                                               ;; opening
                                               (str/join (repeat headline-lvl " "))
                                               "[\"" title "\"")
                                          :level headline-lvl))
                                 (cond (= "" line) (assoc state :ret (str ret "\n"))
                                       (str/starts-with? line "#+begin_src clojure") (assoc state :in-block true)
                                       (str/starts-with? line "#+end_src") (assoc state :in-block false)
                                       in-block (assoc state :ret (str ret prefix line))
                                       :else (assoc state :ret
                                                    (str ret #_prefix ";; " (str/replace line #"\n" (str #_prefix ";; ")))))))))
                         {:level 0 :in-block false :ret "'"}
                         (conj (vec lines)
                               :end))]
        tree))

    (defn org->clj2
      "Build a `clj-file` from an `org-file`
  Assumes that the first code block of the org-file is a valid clojure ns form."
      [org-file clj-file]

      (spit clj-file
            (org-str->clj-str (slurp org-file)))))

(do :entry-points

    (defn build-examples-tests []
      (org->test "src/noon/doc/examples.org"
                 "test/noon/doc/examples_test.clj")
      (z/zprint-file "test/noon/doc/examples_test.clj"
                     "test/noon/doc/examples_test.clj"
                     "test/noon/doc/examples_test.clj"))

    (defn build-clj-examples []
      (org->clj2 "src/noon/doc/examples.org"
                 "src/noon/doc/examples.clj")
      (z/zprint-file "src/noon/doc/examples.clj"
                     "src/noon/doc/examples.clj"
                     "src/noon/doc/examples.clj"))

    (defn build-clj-guide []
      (org->clj2 "src/noon/doc/guide.org"
                 "src/noon/doc/guide.clj")
      (z/zprint-file "src/noon/doc/guide.clj"
                     "src/noon/doc/guide.clj"
                     "src/noon/doc/guide.clj"))

    (defn noon-org->clj []
      (let [clj-file "src/noon/doc/noon.clj"]
        (org->clj2 "src/noon/doc/noon.org"
                   clj-file)
        #_(z/zprint-file clj-file
                         clj-file
                         clj-file))))

(do :org->edn
    "attempt 2"

    (def code-block-regex
      #"^(?s)#\+begin_src.*?\n(.*?)#\+end_src(.*)")

    (defn split-code-block [s]
      (if (seq? s)
        (println s))
      (when-let [[_ code remaining] (re-find code-block-regex s)]
        [code remaining]))

    (defn split-section [s]
      (when-let [[l1 & lines] (seq (str/split-lines s))]
        (when-let [parsed (parse-org-headline l1)]
          (let [break? (fn [l] (re-find (re-pattern (str "^\\*{1," (:level parsed 1) "} ")) l))
                content (take-while (complement break?) lines)
                remaining (str/join "\n" (drop-while (complement break?) lines))]
            [(assoc parsed :content (str/join "\n" content))
             remaining]))))

    (defn split-paragraph [s]
      (let [lines (str/split-lines s)
            break? (fn [l]
                     (or (str/starts-with? l "*")
                         (str/starts-with? l "#+begin")))
            paragraph (str/join "\n" (take-while (complement break?) lines))
            remaining (str/join "\n" (drop-while (complement break?) lines))]
        (when (seq paragraph)
          [(str/trim paragraph)
           remaining])))

    (defn org->edn [s]
      (let [s (str/trim s)]
        (if-let [[code remaining] (split-code-block s)]
          (cons [:code code]
                (org->edn remaining))
          (if-let [[section remaining] (split-section s)]
            (cons (into [:section (:title section)]
                        (org->edn (:content section)))
                  (org->edn remaining))
            (if-let [[paragraph remaining] (split-paragraph s)]
              (if (seq paragraph)
                (cons [:p paragraph] (org->edn remaining))
                (org->edn remaining))
              (if (seq s)
                [[:as-is s]]
                []))))))

    (comment
      (def guide-org-str (slurp "src/noon/doc/guide.org"))
      (org->edn guide-org-str)
      (split-section guide-org-str)))

(defn build-all []
  (build-clj-examples)
  (build-clj-guide)
  (build-examples-tests))

(do :clj-doc-tree

    (defn file->title [filename]
      (-> filename
          (str/replace #"^\d+-" "")     ; remove digit prefix
          (str/replace #"\.md$" "")     ; remove .md extension
          (str/replace "_" " ")))

    (defn build-doc-tree [dir-path]
      (let [root (io/file dir-path)
            ]
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
                                      :cljdoc.doc/tree [(build-doc-tree "doc/Noon")]}))))); replace underscore with space

(do :test-noon-eval

    (defn org-file->clojure-expressions [org-file]
      (loop [blocks [] path [] block nil lines (str/split-lines (slurp org-file))]
        (if-let [[line & lines] (seq lines)]
          (cond (str/starts-with? line "*") (let [{:keys [level title]} (parse-org-headline line)]
                                              (recur (conj blocks {:path path :title title})
                                                     (concat (take (dec level) (concat path (repeat nil)))
                                                             (list title))
                                                     block lines))
                (str/starts-with? line "#+begin_src clojure") (recur blocks path "" lines)
                (str/starts-with? line "#+end_src") (recur (into blocks
                                                                 (map-indexed (fn [idx expr]
                                                                                {:expr expr
                                                                                 :path path})
                                                                              (read-string (str "[" block "\n]"))))
                                                           path false lines)
                block (recur blocks path (str block "\n" line) lines)
                :else (recur blocks path block lines))
          blocks)))

    (defn expressions->code-tree
      [exprs]
      (reduce (fn [tree [idx {:keys [title path expr]}]]
                (if (seq? path)
                  (update-in tree
                             (interpose :children path)
                             (fn [subtree]
                               (if title
                                 (assoc subtree :idx idx)
                                 (update subtree :content (fnil conj []) expr))))
                  tree))
              {} (map vector (range) exprs)))

    (defn code-tree->tests [from tree]
      (keep (fn [[k {:keys [content idx children]}]]
              (if (or (seq children) (seq content))
                (list* 'clojure.test/testing k
                       (concat content
                               (code-tree->tests (conj from k) children)))))
            (sort-by (comp :idx val)
                     tree)))

    (defn emit-noon-org-tests []
      (let [file "test/noon/doc/noon_org_tests.clj"
            expressions (->> (org-file->clojure-expressions "src/noon/doc/noon.org")
                             (expressions->code-tree)
                             (code-tree->tests [])
                             (list* 'clojure.test/deftest 'noon-tests)
                             (str '(ns noon.doc.noon-org-tests
                                     (:require [clojure.test]
                                               [noon.eval :refer [play noon score]]))
                                  "\n"))]
        (spit file expressions)
        (zp/zprint-file file file file)))

    #_(emit-noon-org-tests))
