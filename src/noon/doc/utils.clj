(ns noon.doc.utils
  (:require [noon.score :as n]
            [noon.harmony :as h]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [noon.doc.intro :as intro]))

(do :piano-roll
    (defn elispify [x]
      (cond
        (map? x) (seq (mapcat (fn [[k v]]
                                [k (elispify v)])
                              x))
        (or (seq? x)
            (vector? x)) (seq (map elispify x))

        (ratio? x) (float x)

        (integer? x) (int x)

        :else x))

    (defn harmonic-chunks [s & [strict]]
      (let [e->harmony (fn [e] (dissoc (:pitch e) :position))
            validate (fn [xs]
                       (when strict
                         (assert
                          (every? (fn [[a b]] (<= (:position b) (+ (:position a) (:duration a))))
                                  (partition 2 1 xs))
                          "overlapping harmonies"))
                       xs)]
        (->> s
             (sort-by :position)
             (partition-by e->harmony)
             (map (fn [xs]
                    (let [pos (:position (first xs))
                          dur (- (n/score-duration (set xs)) pos)]
                      {:position (:position (first xs))
                       :duration dur
                       :harmonic-ctx (e->harmony (first xs))})))
             (validate))))

    (defn ->piano-roll
      "Turns a score into a datastructure suitable for elisp piano-roll display.
  X can be a score or something that holds a :score entry in metadata."
      [x]
      (if-let [score (if (n/score? x) x (some-> (meta x) :score))]
        (elispify
         {:notes (->> (filter :pitch score)
                      (sort-by :position)
                      (map (fn [{:as e
                                p :pitch}]
                             (assoc (select-keys e [:position :duration :channel])
                                    :pitch (h/hc->chromatic-value p)
                                    :kind (cond
                                            (h/tonic-equivalent? p) :tonic
                                            (h/structural-equivalent? p) :structural
                                            (h/diatonic-equivalent? p) :diatonic
                                            :else :chromatic)))))
          :harmony (harmonic-chunks score)}))))

(comment
  (use 'noon.score)
  (require '[noon.lib.harmony :as lh])
  (spit "src/noon/doc/sample-pr.el"
        (with-out-str
          (clojure.pprint/pprint
           (->piano-roll
            (mk (cat s0 s2 s1 s3)
                (cat s0 s2)
                ($ (chans o1-
                          (tup _ d1 c1- _))))))))

  (spit "src/noon/doc/sample-pr.el"
        (with-out-str
          (clojure.pprint/pprint
           (->piano-roll
            (mk harmonic-minor
                dur2
                (cat I V IV I)
                (lh/align-contexts :d)
                ($ (chans [o1 (shuftup s0 s1 s2 s4)]
                          (par s0 s1 s2)))))))))

(do :org-guide

    (defn play-form? [x]
      (and (seq? x)
           (= 'play (first x))))

    (defn annotations? [x]
      "A map with exopressions as keys and strings as values denotes annotated snippets of code.
Each expression becomes a code block with the annotation as a side comment."
      (and (map? x)
           (every? string? (vals x))))

    (defn paragraph? [x]
      "a vector of string denotes a paragraph."
      (and (vector? x)
           (every? string? x)))

    (defn build-org-table [data-seq]
      (let [format-row (fn [row] (str "| " (str/join " | " row) " |"))
            formatted-data (map format-row data-seq)
            divider (str "|-" (str/join (repeat (dec (count (first data-seq))) "+-")) "|")]
        (str (first formatted-data)
             "\n" divider "\n"
             (str/join "\n" (rest formatted-data))
             "\n\n")))

    (def org-markup-builders
      {:ol (fn [xs lvl] (str (str/join "\n" (map-indexed (fn [i x] (str (inc i) ". " x)) xs))
                             "\n\n"))
       :ul (fn [xs lvl] (str (str/join "\n" (map (fn [x] (str "- " x)) xs))
                             "\n\n"))
       :* (fn [[title & content] lvl]
            ())
       :table (fn [xs lvl]
                (build-org-table xs))})

    (defn org-markup? [x]
      "Some special keywords are interpreted as markup builders when in first position of a vector."
      (and (seq? x)
           (= 'org (first x))))

    (defn section? [x]
      (and (vector? x)
           (keyword? (first x))))

    (defn pretty-str [x & [no-ending-newline]]
      (let [s (with-out-str (pprint/pprint x))]
        (if no-ending-newline
          (subs s 0 (dec (count s)))
          s)))

    (defn org-code-block [x & [comment]]
      (str (if (play-form? x)
             "#+begin_src clojure :proll\n"
             "#+begin_src clojure :pp\n")
           (if comment
             (str (pretty-str x :no-ending-newline)
                  " ; " comment "\n")
             (pretty-str x))
           "#+end_src\n\n"))

    (defn org-str [x level]
      (cond
        (string? x) (str x "\n\n")
        (paragraph? x) (str (str/join " " x) "\n\n")
        (org-markup? x) (do (print x)
                            ((get org-markup-builders (second x)) (drop 2 x) level))
        (section? x) (let [header (str (apply str (repeat (inc level) "*")) " " (name (first x)))
                           subblocks (map #(org-str % (inc level)) (next x))]
                       (str header "\n\n" (apply str subblocks)))
        (annotations? x) (str/join (map (fn [[code annotation]]
                                          (org-code-block code annotation))
                                        x))
        :else (org-code-block x)))

    (defn write-org-guide []
      (spit "./src/noon/doc/intro.org"
            (org-str intro/guide 0)))

    (comment
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
             (str/join "/" (cons src-path (str/split #"\." (second x))))))

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
                                      :ret (str ret prefix "[:" title)}
                                     (assoc state :ret
                                            (str ret "\n"
                                                 ;; closing
                                                 (str/join (repeat level " "))
                                                 (str/join (repeat (inc (- level headline-lvl)) "]"))
                                                 "\n\n"
                                                 ;; opening
                                                 (str/join (repeat headline-lvl " "))
                                                 "[:" title)
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

      (org->clj "src/noon/doc/guide.org"
                "src/noon/doc/guide.clj"))

    (write-org-guide))
