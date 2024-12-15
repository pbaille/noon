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

(def guide-md-filepath "src/noon/doc/guide.md")

(defn parse-deep-header [s]
  (when (string? s)
    (when-let [[_ hashes title] (re-matches #"^(#+) (.+)$" s)]
     [(count hashes) title])))

(defn md-str->noon-client-hiccup [md-str]
  (let [group-sections (fn self [elems]
                         (if-let [[[h props & _ :as x] & xs] (seq elems)]
                           (cond
                             (= :header h) (let [take? (fn [[k props*]] (or (not (= :header k))
                                                                            (< (:level props) (:level props*))))
                                                 content (take-while take? xs)
                                                 remaining (drop-while take? xs)
                                                 title (first (:children props))
                                                 [inline-code simple-title] (if (string? title)
                                                                              [false title]
                                                                              [true (second title)])]
                                             (cons (concat (list '$ 'noon.client.ui/section
                                                                 {:level h
                                                                  :title simple-title
                                                                  :inline-code inline-code})
                                                           (self content))
                                                   (self remaining)))

                             (= :pre h) (let [[_ [_ props source]] x
                                              source (str/trim source)]
                                          (cons (list '$ 'noon.client.ui/code-editor
                                                      (merge props
                                                             {:source source}))
                                                (self xs)))

                             ;; the commonmark parser starts emitting Paragraph Nodes when headings exceeds level 7
                             ;; we take care of fixing this here
                             (= :p h) (if-let [[level title] (parse-deep-header (first (second x)))]
                                        (self (cons [:header {:level level :children (cons title (rest (second x)))}]
                                                    xs))
                                        (cons (concat (list '$ :p) (self (second x)))
                                              (self xs)))

                             (seq? (second x)) (cons (concat (list '$ h) (self (second x)))
                                                     (self xs))
                             (seq? (nth x 2 nil)) (cons (concat (list '$ h (second x))
                                                                (self (nth x 2)))
                                                        (self xs))
                             (vector? x) (cons (cons '$ x) (self xs))
                             :else (cons x (self xs)))
                           ()))]
    (vec (group-sections (h/markdown->hiccup config md-str)))))

#_(md-str->noon-client-hiccup (slurp "src/noon/doc/guide.md"))

(defn create-client-guide-ns [& _]
  (spit "client/noon/client/guide.cljs"
        (str "(ns noon.client.guide (:require [noon.client.ui] [uix.core :refer [$ defui]]))\n\n"
             "(defui guide [_]\n  "
             (seq (first (md-str->noon-client-hiccup (slurp guide-md-filepath))))
             ")")))

(comment
  (shell/sh "pandoc" "-f" "org" "-t" "gfm" "-o" "src/noon/doc/guide.md" "src/noon/doc/guide.org")
  (create-client-guide-ns))
