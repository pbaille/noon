(ns org-utils
  (:require [clojure.string :as str]
            [zprint.core :as z]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [zprint.core :as zp]
            [noon.utils.misc :as u]
            [noon.eval]
            [clojure.java.shell :as shell]))

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
    (defn org-str->clj-str [org-str & {:as opts :keys [ns-form]}]
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
                                                    (str ret prefix ";; " (str/replace line #"\n" (str prefix ";; ")))))))))
                         {:level 0 :in-block false :ret ""}
                         (conj (vec lines)
                               :end))
            [_ ns-decl] (or [nil ns-form] (re-find #"(?s)#\+begin_src.*?\n(.*?)#\+end_src" org-str))]
        (str ns-decl "\n\n" tree)))

    (defn org->clj
      "Build a `clj-file` from an `org-file`
  Assumes that the first code block of the org-file is a valid clojure ns form."
      [org-file clj-file ns-sym]

      (spit clj-file
            (org-str->clj-str (slurp org-file)
                              :ns-form (u/pretty-str (noon.eval/clj-ns-form 'noon.doc.noon))))))

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
      (def noon-org-str (slurp "src/noon/doc/noon.org"))
      (org->edn noon-org-str)
      (split-section noon-org-str)))

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

(do :noon-org->test-files

    (defn code-block-options [s]
      (let [options (set (str/split (str/replace s "#+begin_src clojure" "")
                                    #" "))]
        {:clj-only (options ":clj-only")
         :cljs-only (options ":cljs-only")
         :no-tests (options ":no-tests")}))

    (defn scan-org-file [org-file]
      (loop [lines (str/split-lines (slurp org-file))
             {:as state :keys [blocks path current-block current-text block-options]}
             {:blocks []
              :path []
              :current-block nil
              :block-options nil
              :current-text nil}]
        (if-let [[line & lines] (seq lines)]
          (cond (str/starts-with? line "*")
                (let [{:keys [level title]} (parse-org-headline line)]
                  (recur lines
                         (assoc state
                                :blocks (if current-text
                                          (into blocks [{:text current-text :path path}
                                                        {:path path :title title}])
                                          (conj blocks {:path path :title title}))
                                :path (concat (take (dec level) (concat path (repeat nil)))
                                              (list title))
                                :current-text nil)))

                (str/starts-with? line "#+begin_src clojure")
                (recur lines (assoc state
                                    :blocks (if current-text
                                              (conj blocks {:text current-text :path path})
                                              blocks)
                                    :current-block ""
                                    :block-options (code-block-options line)
                                    :current-text nil))

                (str/starts-with? line "#+end_src")
                (recur lines
                       (assoc state
                              :blocks (conj blocks
                                            {:exprs (read-string (str "[" current-block "\n]"))
                                             :source (str/trim current-block)
                                             :path path
                                             :options block-options})
                              :current-block nil))

                current-block
                (recur lines (assoc state
                                    :current-block (str current-block "\n" line)))

                (or current-text (seq line))
                (recur lines (assoc state
                                    :current-text (if current-text
                                                    (str current-text "\n" line)
                                                    line)))
                :else
                (recur lines state))
          blocks)))

    (defn org-blocks->code-tree
      [exprs]
      (reduce (fn [tree [idx {:keys [title path] :as block}]]
                (if (seq? path)
                  (update-in tree
                             (interpose :children path)
                             (fn [subtree]
                               (if title
                                 (assoc subtree :idx idx :path path)
                                 (update subtree :content (fnil conj []) block))))
                  tree))
              {} (map vector (range) exprs)))

    (defn prepare-test-content [content target]
      (mapcat (fn [{:keys [exprs options]}]
                (map (fn [expr]
                       (let [expr `(t/is (noon.freeze/freeze ~expr))]
                         (cond (:no-tests options) nil
                               (:clj-only options) (when (= target :clj) expr)
                               (:cljs-only options) (when (= target :cljs) expr)
                               :else expr)))
                     exprs))
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
       #"(-?\d+)/(\d+)\b"
       (fn [[_ n d]]
         (str "(/ " n " " d ")"))))

    (defn org->test-ns-str [file target]
      (let [code-str
            (->> (scan-org-file file)
                 (remove :text)
                 (org-blocks->code-tree)
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

    (do :client-markup-gen

        (def org-to-html
          (memoize
           (fn [input-str]
             (let [result (shell/sh "pandoc" "-f" "org" "-t" "html" :in input-str)]
               (if (zero? (:exit result))
                 (:out result)
                 (throw (Exception. (:err result))))))))

        (defn htmlify-text-blocks [blocks]
          (map (fn [node]
                 (if-let [text (:text node)]
                   (-> (dissoc node :text)
                       (assoc :html (org-to-html text)))
                   node))
               blocks))

        (defn slugify [s]
          (-> s
              (str/lower-case)
              (str/replace #"[^\w\s-]" "")
              (str/replace #"\s+" "-")
              (str/trim)))

        (defn breadcrumbs [at]
          (mapv (fn [path]
                  {:text (last path)
                   :level (count path)
                   :href (->> (map slugify path)
                              (interpose "/")
                              (cons "#/")
                              (apply str))})
                (next (reductions conj [] at))))

        (defn node->markup [{:keys [content children path html source]}]
          (cond source (list '$ 'noon.client.ui/code-editor {:source source})
                html (list '$ 'noon.client.ui/raw {:html html})
                :else (let [title (last path)
                            [inline-code simple-title] (if (= \= (first title))
                                                         [true (str/replace title #"=" "")]
                                                         [false title])
                            id (str "/"
                                    (str/join "/" (map slugify path)))]

                        (concat (list '$ 'noon.client.ui/section
                                      {:id id
                                       :level (count path)
                                       :title simple-title
                                       :inline-code inline-code
                                       :breadcrumbs (breadcrumbs path)
                                       :has-subsections (boolean (seq children))})
                                (mapv node->markup (concat content (sort-by :idx (vals children))))))))

        (defn org-file->client-markup [file]
          (->> (scan-org-file file)
               (htmlify-text-blocks)
               (org-blocks->code-tree)))

        (comment
          (def code-tree
            (org-file->client-markup "src/noon/doc/noon.org"))

          (first (mapv node->markup (vals (org-file->client-markup "src/noon/doc/noon.org"))))))

    (comment

      (org->test-ns-str "src/noon/doc/noon.org" :clj)

      (let [file "src/noon/doc/noon.org"]
        (->> (org-file->clojure-expressions file)
             #_(expressions->code-tree)
             #_(code-tree->tests [] :cljs)))

      (let [file "src/noon/doc/noon.org"]
        (->> (org-file->clojure-expressions file)
             (map (fn [node]
                    (if-let [text (:text node)]
                      (-> (dissoc node :text)
                          (assoc :html (org-to-html text)))
                      node)))
             (org-blocks->code-tree)))))

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
        (org->clj "src/noon/doc/noon.org"
                  clj-file
                  'noon.doc.noon)
        (when pretty? (pretty-file! clj-file))))

    (defn build-client-doc-ns [& _]
      (spit "client/noon/client/doc.cljs"
            (str "(ns noon.client.doc (:require [noon.client.ui] [uix.core :refer [$ defui]]))\n\n"
                 "(defui doc [_]\n  "
                 (seq (first (mapv node->markup (vals (org-file->client-markup "src/noon/doc/noon.org")))))
                 ")"))))

(defn build-all []
  (build-client-doc-ns)
  (build-doc-tests)
  (build-doc-ns))

(comment
  (noon.eval/clj-ns-form 'noon.doc.noon)
  (spit "src/noon/doc/noon.clj"
        (org-str->clj-str (slurp "src/noon/doc/noon.org")
                          :ns-form (u/pretty-str (noon.eval/clj-ns-form 'noon.doc.noon)))))
