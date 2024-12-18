(ns org-utils
  (:require [clojure.string :as str]
            [zprint.core :as z]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [zprint.core :as zp]
            [noon.utils.misc :as u]))

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
                                      :cljdoc.doc/tree [(build-doc-tree "doc/Noon")]})))))

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
                                                                 (map (fn [expr]
                                                                        {:expr expr
                                                                         :path path})
                                                                      (read-string (str "[" block "\n]"))))
                                                           path false lines)
                block (recur blocks path (str block "\n" line) lines)
                :else (recur blocks path block lines))
          blocks)))

    (defn code-block-options [s]
      (let [options (set (str/split (str/replace s "#+begin_src clojure" "")
                                    #" "))]
        {:clj-only (options ":clj-only")
         :cljs-only (options ":cljs-only")}))

    (defn org-file->clojure-expressions2 [org-file]
      (loop [lines (str/split-lines (slurp org-file))
             {:as state :keys [blocks path current-block block-options]}
             {:blocks [] :path [] :current-block nil :block-options nil}]
        (if-let [[line & lines] (seq lines)]
          (cond (str/starts-with? line "*")
                (let [{:keys [level title]} (parse-org-headline line)]
                  (recur lines
                         (assoc state
                                :blocks (conj blocks {:path path :title title})
                                :path (concat (take (dec level) (concat path (repeat nil)))
                                              (list title)))))

                (str/starts-with? line "#+begin_src clojure")
                (recur lines (assoc state
                                    :current-block ""
                                    :block-options (code-block-options line)))

                (str/starts-with? line "#+end_src")
                (recur lines
                       (assoc state
                              :blocks (into blocks
                                            (map (fn [expr]
                                                   {:expr expr
                                                    :path path
                                                    :options block-options})
                                                 (read-string (str "[" current-block "\n]"))))
                              :current-block false))

                current-block
                (recur lines (assoc state
                                    :current-block (str current-block "\n" line)))

                :else
                (recur lines state))
          blocks)))

    (defn expressions->code-tree
      [exprs]
      (reduce (fn [tree [idx {:keys [title path expr] :as block}]]
                (if (seq? path)
                  (update-in tree
                             (interpose :children path)
                             (fn [subtree]
                               (if title
                                 (assoc subtree :idx idx)
                                 (update subtree :content (fnil conj []) block))))
                  tree))
              {} (map vector (range) exprs)))

    (defn prepare-test-content [content target]
      (map (fn [{:keys [expr options]}]
             (let [expr `(t/is (noon.freeze/freeze ~expr))]
               (cond (:clj-only options) (when (= target :clj) expr)
                     (:cljs-only options) (when (= target :cljs) expr)
                     :else expr)))
           content))

    (defn code-tree->tests [from target tree]
      (keep (fn [[k {:keys [content children]}]]
              (if (or (seq children) (seq content))
                (list* 't/testing k
                       (concat (prepare-test-content content target)
                               (code-tree->tests (conj from k) target children)))))
            (sort-by (comp :idx val)
                     tree)))

    (defn replace-rational-literals [s]
      (clojure.string/replace
       s
       #"\b(\d+)/(\d+)\b"
       (fn [[_ n d]]
         (str "(/ " n " " d ")"))))

    (defn org->test-ns-str [file target]
      (let [code-str
            (->> (org-file->clojure-expressions2 file)
                 (expressions->code-tree)
                 (code-tree->tests [] target)
                 (list* 't/deftest 'noon-tests)
                 (str (list 'ns 'noon.doc.noon-org-test
                            (list :require (case target
                                             :clj '[clojure.test :as t]
                                             :cljs '[cljs.test :as t])

                                  '[noon.eval :refer [play noon score]]
                                  '[noon.freeze]))
                      "\n"))]
        (if (= :cljs target)
          (replace-rational-literals code-str)
          code-str)))
    (comment

      (org->test-ns-str "src/noon/doc/noon.org" :clj)

      (let [file "src/noon/doc/noon.org"]
        (->> (org-file->clojure-expressions2 file)
             (expressions->code-tree)
             (code-tree->tests [])))))

(do :entry-points

    (defn build-doc-tests [& [pretty?]]
      (let [file "test/noon/doc/noon_org_test"
            clj-file (str file ".clj")
            cljs-file (str file ".cljs")]
        (spit clj-file (org->test-ns-str "src/noon/doc/noon.org" :clj))
        (spit cljs-file (org->test-ns-str "src/noon/doc/noon.org" :cljs))
        (when pretty?
          (pretty-file! clj-file)
          (pretty-file! cljs-file))))

    (defn build-doc-ns [& [pretty?]]
      (let [clj-file "src/noon/doc/noon.clj"]
        (org->clj2 "src/noon/doc/noon.org"
                   clj-file)
        (when pretty? (pretty-file! clj-file)))))

(defn build-all []
  (build-doc-tests true)
  (build-doc-ns))
