(ns create-client-guide-ns
  (:require [commonmark-hiccup.core :as h]
            [clojure.string :as str]
            [clojure.java.shell :as shell]))

(defmethod h/node-properties org.commonmark.node.FencedCodeBlock [node]
  (h/property-map node))

(defmethod h/node-properties org.commonmark.node.Code [node]
  (h/property-map node))

(def config
  (assoc-in h/default-config
            [:renderer :nodes org.commonmark.node.Heading]
            [:header {:level :node-level
                      :children :content}]))

(def noon-org-filepath "src/noon/doc/noon.md")

(defn slugify [s]
  (-> s
      (str/lower-case)                      ; Convert to lowercase
      (str/replace #"[^\w\s-]" "")          ; Remove non-word and non-space characters
      (str/replace #"\s+" "-")              ; Replace spaces with dashes
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

(defn parse-deep-header [s]
  (when (string? s)
    (when-let [[_ hashes title] (re-matches #"^(#+) (.+)$" s)]
      [(count hashes) title])))

(defn md-str->noon-client-markup [from md-str]
  (let [group-sections
        (fn self [at elems]
          (if-let [[[h props & _ :as x] & xs] (seq elems)]
            (cond
              (= :header h) (let [take? (fn [[k props*]] (or (not (= :header k))
                                                             (< (:level props) (:level props*))))
                                  content (take-while take? xs)
                                  remaining (drop-while take? xs)
                                  title (first (:children props))
                                  [inline-code simple-title] (if (string? title)
                                                               [false title]
                                                               [true (second title)])
                                  path (conj at simple-title)
                                  id (str "/" (str/join "/" (map slugify path)))
                                  has-subsections (boolean (some (fn [x] (and (vector? x) (= :header (first x))))
                                                                 content))]

                              (cons (concat (list '$ 'noon.client.ui/section
                                                  {:id id :level (:level props)
                                                   :title simple-title
                                                   :inline-code inline-code
                                                   :breadcrumbs (breadcrumbs path)
                                                   :has-subsections has-subsections})
                                            (self path content))
                                    (self at remaining)))

              (= :pre h) (let [[_ [_ props source]] x
                               source (str/trim source)]
                           (cons (list '$ 'noon.client.ui/code-editor
                                       (merge props
                                              {:source source}))
                                 (self at xs)))

              ;; the commonmark parser starts emitting Paragraph Nodes when headings exceeds level 7
              ;; we take care of fixing this here
              (= :p h) (if-let [[level title] (parse-deep-header (first (second x)))]
                         (self at (cons [:header {:level level :children (cons title (rest (second x)))}]
                                        xs))
                         (cons (concat (list '$ :p) (self at (second x)))
                               (self at xs)))

              (seq? (second x)) (cons (concat (list '$ h) (self at (second x)))
                                      (self at xs))
              (seq? (nth x 2 nil)) (cons (concat (list '$ h (second x))
                                                 (self at (nth x 2)))
                                         (self at xs))
              (vector? x) (cons (cons '$ x) (self at xs))
              :else (cons x (self at xs)))
            ()))]
    (vec (group-sections (or from []) (h/markdown->hiccup config md-str)))))

#_(md-str->noon-client-hiccup (slurp "src/noon/doc/guide.md"))

(defn create-client-guide-ns [& _]
  (spit "client/noon/client/guide.cljs"
        (str "(ns noon.client.guide (:require [noon.client.ui] [uix.core :refer [$ defui]]))\n\n"
             "(defui guide [_]\n  "
             (seq (first (md-str->noon-client-markup [] (slurp noon-org-filepath))))
             ")")))

(comment
  (shell/sh "pandoc" "-f" "org" "-t" "gfm" "-o" "src/noon/doc/guide.md" "src/noon/doc/guide.org")
  (shell/sh "pandoc" "-f" "org" "-t" "gfm" "-o" "src/noon/doc/examples.md" "src/noon/doc/examples.org")
  (create-client-guide-ns))
