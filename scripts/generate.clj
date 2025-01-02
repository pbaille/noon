(ns generate
  (:require [clojure.string :as str]
            [babashka.process :as bpr]
            [noon.utils.sci :as sci-utils]
            [clojure.pprint :as pp]))

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
    (defn org-str->clj-str [org-str & {:keys [ns-form]}]
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
                              :ns-form (with-out-str (pp/pprint (sci-utils/clj-ns-form ns-sym)))))))

(do :noon-org->test-files

    (defn code-block-options [s]
      (let [options (set (str/split (str/replace s "#+begin_src clojure" "")
                                    #" "))]
        {:clj-only (boolean (options ":clj-only"))
         :cljs-only (boolean (options ":cljs-only"))
         :no-tests (boolean (options ":no-tests"))}))

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
                              :current-block nil
                              :block-options nil))

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
          (mapv (fn [i b] (assoc b :idx i))
                (range) blocks))))

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

        (defn org-to-html
          [input-str]
          (let [{:keys [out err _exit]} (bpr/process ["pandoc" "-f" "org" "-t" "html"]
                                                     {:in input-str :out :string :err :string})]
            (if out
              @out
              (throw (Exception. err)))))

        (defn htmlify-text-blocks
          "blocks containing a :text entry will be enriched with an
          :html entry holding the text content turned into html by pandoc."
          [blocks]

          (let [text-blocks (filter :text blocks)
                ;; not a particularly elegant solution
                ;; but I want to avoid calling org-to-html many times (perf)
                sep "\n* SPLIT\n"
                split #"<h1 id=\".*?\">SPLIT</h1>"
                org-content (str/join sep (map :text text-blocks))
                html-content (str/split (org-to-html org-content) split)]
            (reduce (fn [blocks b]
                      (assoc blocks (:idx b) b))
                    blocks
                    (map (fn [b html]
                           (assoc b :html (str/trim html)))
                         text-blocks
                         html-content))))

        (defn slugify [s]
          (-> (or s "")
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

        (defn node->markup [{:keys [content children path html source options]}]
          (cond source (list '$ 'noon.client.ui/code-editor {:source source
                                                             :options options})
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
                                       :breadcrumbs (breadcrumbs (concat (butlast path) (list simple-title)))
                                       :has-subsections (boolean (seq children))})
                                (mapv node->markup (concat content (sort-by :idx (vals children))))))))

        (defn org-file->client-markup [file]
          (->> (scan-org-file file)
               (htmlify-text-blocks)
               (org-blocks->code-tree)))

        (comment
          (def code-tree
            (org-file->client-markup "src/noon/doc/noon.org"))

          (->> (scan-org-file "src/noon/doc/noon.org")
               (htmlify-text-blocks))

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

    (defn build-doc-tests [& _]
      (let [file "test/noon/doc/noon_org_test"
            clj-file (str file ".clj")
            cljs-file (str file ".cljs")]
        (spit clj-file (org->test-ns-str "src/noon/doc/noon.org" :clj))
        (spit cljs-file (org->test-ns-str "src/noon/doc/noon.org" :cljs))))

    (defn build-doc-ns [& _]
      (let [clj-file "src/noon/doc/noon.clj"]
        (org->clj "src/noon/doc/noon.org"
                  clj-file
                  'noon.doc.noon)))

    (defn build-client-doc-ns [& _]
      (spit "client/noon/client/doc.cljs"
            (str "(ns noon.client.doc (:require [noon.client.ui] [uix.core :refer [$ defui]]))\n\n"
                 "(defui doc [_]\n  "
                 (seq (first (mapv node->markup (vals (org-file->client-markup "src/noon/doc/noon.org")))))
                 ")"))))

(defn build-all []
  (build-client-doc-ns)
  (build-doc-ns)
  (build-doc-tests))

(comment
  (noon.eval/clj-ns-form 'noon.doc.noon)
  (deref (keys (bpr/process ["pandoc" "-f" "org" "-t" "html"]
                            {:in "#+OPTIONS: H:9" :out :string :err :string}))))
