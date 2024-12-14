(ns create-client-guide-ns
  (:require [commonmark-hiccup.core :as h]
            [clojure.string :as str]))

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

(defn md-str->noon-client-hiccup [md-str]
  (let [group-sections (fn self [elems]
                         (if-let [[[h props & _ :as x] & xs] (seq elems)]
                           (cond
                             (= :header h) (let [take? (fn [[k props*]] (or (not (= :header k))
                                                                            (< (:level props) (:level props*))))
                                                 content (take-while take? xs)
                                                 remaining (drop-while take? xs)]
                                             (cons (concat (list '$ 'noon.client.ui/section
                                                                 {:level h :title (first (self [(first (:children props))]))})
                                                           (self content))
                                                   (self remaining)))

                             (= :pre h) (let [[_ [_ props source]] x
                                              source (str/trim source)
                                              multi-line? (boolean (next (str/split source #"\n")))]
                                          (cons (if multi-line?
                                                  (list '$ 'noon.client.ui/code-editor
                                                        (merge props
                                                               {:source source}))
                                                  (list '$ 'noon.client.ui/snippet-runner
                                                        (merge props
                                                               {:source source})))
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
