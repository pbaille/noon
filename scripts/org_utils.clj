(ns org-utils
  (:require [clojure.string :as str]
            [zprint.core :as z]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [me.raynes.fs :as fs]))

"Utils for converting org files to clojure files (regular and test)"

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
                         {:level 0 :in-block false :ret ""}
                         (conj (vec lines)
                               :end))]
        tree))

    (defn org->clj2
      "Build a `clj-file` from an `org-file`
  Assumes that the first code block of the org-file is a valid clojure ns form."
      [org-file clj-file]

      (let [clj-str (org-str->clj-str (slurp org-file))
            without-square-brackets (-> clj-str
                                        (subs 0 (dec (count clj-str)))
                                        (subs 1))
            without-first-line (->> without-square-brackets
                                    str/split-lines
                                    rest
                                    (str/join "\n"))]
        (spit clj-file
              without-first-line))))

(defn org-file->clojure-expressions [org-file]
  (loop [blocks [] path [] block nil lines (str/split-lines (slurp org-file))]
    (if-let [[line & lines] (seq lines)]
      (cond (str/starts-with? line "*") (let [{:keys [level title]} (parse-org-headline line)]
                                          (recur blocks (concat (take (dec level) (concat path (repeat nil)))
                                                                (list title))
                                                 block lines))
            (str/starts-with? line "#+begin_src clojure") (recur blocks path "" lines)
            (str/starts-with? line "#+end_src") (recur (concat blocks
                                                               (map (fn [expr]
                                                                      (with-meta expr {:path path}))
                                                                    (read-string (str "[" block "\n]"))))
                                                       path false lines)
            block (recur blocks path (str block "\n" line) lines)
            :else (recur blocks path block lines))
      blocks)))

(defn parse-score-creating-form
  "Parse a score creating form into :return and :bindings or return nil."
  [e]
  (if (seq? e)
    (some-> (cond (contains? #{"play" "mk"} (name (first e)))
                  {:return (cons 'noon.score/mk (rest e))}
                  (= "mk*" (name (first e)))
                  {:return (list* 'noon.score/mk* (second e))}
                  (= "noon" (name (first e)))
                  {:return (nth e 2)}
                  (= 'let (first e))
                  (if-let [parsed-return (parse-score-creating-form (nth e 2))]
                    (assoc parsed-return :bindings (second e))))
            (with-meta (meta e)))))

(defn assertion-tree->nested-testing
  [content]
  (concat (:content content)
          (mapv (fn [[subtitle subcontent]]
                  (list* 'testing subtitle (assertion-tree->nested-testing subcontent)))
                (dissoc content :content))))

(defn org->test
  "Turn an org files containing noon examples into a test ns.
       top forms starting with 'play and 'let are assumed to be the one we froze."
  [org-file clj-file]
  (let [[ns-decl & expressions] (org-file->clojure-expressions org-file)
        {:keys [statements score-creating-forms]}
        (reduce (fn [ret expr]
                  (if-let [parsed-noon-expr (parse-score-creating-form expr)]
                    (update ret :score-creating-forms conj parsed-noon-expr)
                    (if (and (seq? expr) (str/starts-with? (name (first expr)) "def"))
                      (update ret :statements conj expr)
                      ret)))
                {:statements [] :score-creating-forms []}
                expressions)
        assertions (mapv (fn [{:as e :keys [bindings return]}]
                           (with-meta (list 'is (list 'noon.test/frozen* nil
                                                      (if bindings
                                                        (list 'let bindings return)
                                                        return)))
                             (meta e)))
                         score-creating-forms)
        assertion-tree (reduce (fn [ret expr]
                                 (update-in ret (conj (vec (:path (meta expr)))
                                                      :content)
                                            (fnil conj [])
                                            expr))
                               {} assertions)
        [_ ns & ns-body] ns-decl
        enriched-body (-> (group-by first (filter seq? ns-body))
                          (update :require (fn [reqs] (map (fn [req]
                                                             (concat req
                                                                     '([clojure.test :refer [deftest testing is]] [noon.test])))
                                                           reqs))))]
    (spit clj-file
          (str/join "\n\n"
                    (cons (list* 'ns (symbol (str (name ns) "-test"))
                                 (str "This file is generated from `" org-file "`")
                                 (map first (vals enriched-body)))
                          (concat statements
                                  (list (list* 'deftest 'main
                                               (assertion-tree->nested-testing assertion-tree)))))))))

(comment (slurp org-file)
         (org-file->clojure-expressions "src/noon/doc/examples.org")
         (org->test "src/noon/doc/examples.org"
                    "test/noon/doc/examples_test.clj"))

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
  (build-examples-tests)); replace underscore with space

(defn file->title [filename]
  (-> filename
      (str/replace #"^\d+-" "")  ; remove digit prefix
      (str/replace #"\.md$" "")  ; remove .md extension
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
                                  :cljdoc.doc/tree [(build-doc-tree "doc/Noon")]}))))

(comment :guide->md->hiccup

         (require '[commonmark-hiccup.core :as h])

         (defmethod h/node-properties org.commonmark.node.IndentedCodeBlock [node]
           (h/property-map node))

         (def guide-md-filepath "src/noon/doc/guide.md")

         (defn md-str->noon-client-hiccup [md-str]
           (let [with-h-index (map (fn [[k content :as elem]] (if (and (string? k) (re-matches #"h[1-9]" k))
                                                                [(Integer/parseInt (subs k 1 2))
                                                                 content]
                                                                elem))
                                   (h/markdown->hiccup md-str))
                 group-sections (fn self [elems]
                                  (if-let [[[h & _ :as x] & xs] (seq elems)]
                                    (cond
                                      (int? h) (let [take? (fn [[k]] (or (not (int? k)) (< h k)))
                                                     content (take-while take? xs)
                                                     remaining (drop-while take? xs)]
                                                 (cons (concat (list '$ 'noon.client.ui/section
                                                                     {:level h :title (first (self [(first (second x))]))})
                                                               (self content))
                                                       (self remaining)))

                                      (= :pre h) (cons (list '$ 'noon.client.ui/code-editor {:source (str/trim (second (second x)))})
                                                       (self xs))

                                      (seq? (second x)) (cons (concat (list '$ h) (self (second x)))
                                                              (self xs))
                                      (seq? (nth x 2 nil)) (cons (concat (list '$ h (second x))
                                                                         (self (nth x 2)))
                                                                 (self xs))
                                      (vector? x) (cons (cons '$ x) (self xs))
                                      :else (cons x (self xs)))
                                    ()))]
             (vec (group-sections with-h-index))))

         (md-str->noon-client-hiccup (slurp "src/noon/doc/guide.md"))

         (defn create-client-guide-ns []
           (spit "client/noon/client/guide.cljs"
                 (str "(ns noon.client.guide (:require [noon.client.ui] [uix.core :refer [$ defui]]))\n\n"
                      "(defui guide [_]\n  "
                      (seq (first (md-str->noon-client-hiccup (slurp guide-md-filepath))))
                      ")"))))
