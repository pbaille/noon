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
           (str/join "/" (cons src-path (str/split (second x) #"\.")))))

    (defn slugify [s]
      (-> (or s "")
          (str/lower-case)
          (str/replace #"[^\w\s-]" "_")
          (str/replace #"\s+" "-")
          (str/trim))))

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
                (let [{:keys [level title]} (parse-org-headline line)
                      path' (take (dec level) (concat path (repeat nil)))]
                  (recur lines
                         (assoc state
                                :blocks (if current-text
                                          (into blocks [{:text current-text :path path}
                                                        {:path path' :title title}])
                                          (conj blocks {:path path' :title title}))
                                :path (concat path' (list title))
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

          (mapv (fn [i {:as block :keys [path]}]
                  (assoc block
                         :idx i
                         :path (mapv slugify path)))
                (range)
                blocks))))

    (defn org-blocks->tree
      [blocks]
      (reduce (fn [tree {:keys [idx title path] :as block}]
                #_(println path (concat path (list (slugify title))))
                (if title
                  (let [path' (concat path (list (slugify title)))]
                    (update-in tree (interpose :children path')
                               assoc :title title :path (vec path') :idx idx))
                  (update-in tree
                             (interpose :children path)
                             (fn [subtree]
                               (update subtree :content (fnil conj []) block)))))
              {} blocks))

    (do :tests

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
                     (org-blocks->tree)
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
              code-str))))

    (do :client-data

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

        (defn node->ui-data [{:keys [title content children path html source options idx]}]
          (cond source {:type :code
                        :source source
                        :options options}
                html {:type :raw :html html}
                :else (let [[inline-code title] (if (= \= (first title))
                                                  [true (str/replace title "=" "")]
                                                  [false title])
                            id (str "/"
                                    (str/join "/" path))]

                        {:type :section
                         :id id
                         :idx idx
                         :path path
                         :level (count path)
                         :title title
                         :inline-code inline-code
                         :content (mapv node->ui-data content)
                         :children (update-vals children node->ui-data)})))

        (defn org-file->client-markup [file]
          (->> (scan-org-file file)
               ; (take 60) vec
               (htmlify-text-blocks)
               (org-blocks->tree)))

        (comment
          (def code-tree
            (org-file->client-markup "src/noon/doc/noon.org"))

          (->> (scan-org-file "src/noon/doc/noon.org")
               (org-blocks->tree))

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
            (str "(ns noon.client.doc)\n\n"
                 "(def doc-data\n  "
                 (node->ui-data (get (org-file->client-markup "src/noon/doc/noon.org")
                                     "noon"))
                 #_(with-out-str (pp/pprint (first (mapv node->ui-data (vals (org-file->client-markup "src/noon/doc/noon.org"))))))
                 ")"))))

(defn build-all []
  (build-client-doc-ns)
  (build-doc-ns)
  (build-doc-tests))

(comment
  (noon.eval/clj-ns-form 'noon.doc.noon)
  (deref (keys (bpr/process ["pandoc" "-f" "org" "-t" "html"]
                            {:in "#+OPTIONS: H:9" :out :string :err :string}))))
