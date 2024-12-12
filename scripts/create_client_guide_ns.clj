(ns create-client-guide-ns
  (:require [commonmark-hiccup.core :as h]
            [clojure.string :as str]))

(defmethod h/node-properties org.commonmark.node.IndentedCodeBlock [node]
  (h/property-map node))

(defmethod h/node-properties org.commonmark.node.Code [node]
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

(defn create-client-guide-ns [& _]
  (spit "client/noon/client/guide.cljs"
        (str "(ns noon.client.guide (:require [noon.client.ui] [uix.core :refer [$ defui]]))\n\n"
             "(defui guide [_]\n  "
             (seq (first (md-str->noon-client-hiccup (slurp guide-md-filepath))))
             ")")))
