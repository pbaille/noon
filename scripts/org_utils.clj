(ns org-utils
  (:require [clojure.string :as str]
            [zprint.core :as z]))

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
          (str/join "\n"
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
  (org->clj "src/noon/doc/examples.org"
            "src/noon/doc/examples.clj")
  (z/zprint-file "src/noon/doc/examples.clj"
                 "src/noon/doc/examples.clj"
                 "src/noon/doc/examples.clj"))

(defn build-clj-guide []
  (org->clj "src/noon/doc/guide.org"
            "src/noon/doc/guide.clj")
  (z/zprint-file "src/noon/doc/guide.clj"
                 "src/noon/doc/guide.clj"
                 "src/noon/doc/guide.clj"))

(defn split-org-file
  "Split a large org file into multiple files based on heading levels.
   Creates separate files for subtrees at the specified level and replaces
   them with links in the main file."
  [input-file output-dir max-level]
  (let [lines (str/split-lines (slurp input-file))
        base-name (str/replace (last (str/split input-file #"/")) #"\.org$" "")]

    (loop [remaining-lines lines
           current-file []
           current-path output-dir
           file-counter 1
           result []]

      (if-let [line (first remaining-lines)]
        (if-let [{:keys [level title]} (parse-org-headline line)]
          (if (> level max-level)
            ;; Continue accumulating lines for current subtree
            (recur (rest remaining-lines)
                   (conj current-file line)
                   current-path
                   file-counter
                   result)
            ;; Create new file for this subtree
            (let [file-name (format "%s-%03d.org" base-name file-counter)
                  full-path (str current-path "/" file-name)
                  link-line (format "* [[./%s][%s]]" file-name title)]

              ;; Write accumulated content if any
              (when (seq current-file)
                (spit full-path (str/join "\n" current-file)))

              (recur (rest remaining-lines)
                     [line]
                     current-path
                     (inc file-counter)
                     (conj result link-line))))

          ;; Not a headline, accumulate line
          (recur (rest remaining-lines)
                 (conj current-file line)
                 current-path
                 file-counter
                 result))

        ;; Write main file with links
        (spit (str output-dir "/" base-name ".org")
              (str/join "\n" result))))))

#_(split-org-file "src/noon/doc/examples.org" "src/noon/doc/examples" 2)

(defn build-all []
  (build-clj-examples)
  (build-clj-guide)
  (build-examples-tests))
