(ns noon.doc.utils
  (:require [noon.score :as n]
            [noon.harmony :as h]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(do :piano-roll

    "Support for emacs piano-roll mode"

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
       `x` can be a score or something that holds a :score entry in metadata."
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
          :harmony (harmonic-chunks score)})))

    (comment
      '(do (use 'noon.score)
           (require '[noon.lib.harmony :as lh]))
      (spit "src/noon/doc/sample-pr.el"
            (with-out-str
              (clojure.pprint/pprint
               (->piano-roll
                (mk (lin s0 s2 s1 s3)
                    (lin s0 s2)
                    (each (chans o1-
                                 (tup _ d1 c1- _))))))))

      (spit "src/noon/doc/sample-pr.el"
            (with-out-str
              (clojure.pprint/pprint
               (->piano-roll
                (mk harmonic-minor
                    dur2
                    (lin I V IV I)
                    (lh/align-contexts :d)
                    (each (chans [o1 (shuftup s0 s1 s2 s4)]
                                 (par s0 s1 s2))))))))))

(do :org->clj

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

    (comment (org->clj "src/noon/doc/examples.org"
                       "src/noon/doc/examples.clj"))

    (defn org-file->clojure-expressions [org-file]
      (loop [blocks [] block nil lines (str/split-lines (slurp org-file))]
        (if-let [[line & lines] (seq lines)]
          (cond (str/starts-with? line "#+begin_src clojure") (recur blocks "" lines)
                (str/starts-with? line "#+end_src") (recur (concat blocks (read-string (str "[" block "]")))
                                                           false lines)
                block (recur blocks (str block "\n" line) lines)
                :else (recur blocks block lines))
          blocks)))

    (defn org->test
      "Turn an org files containing noon examples into a test ns.
       top forms starting with 'play and 'let are assumed to be the one we froze."
      [org-file clj-file]
      (let [[ns-decl & expressions] (org-file->clojure-expressions org-file)
            grouped (group-by (fn [e] (contains? '#{play let} (first e)))
                              expressions)
            frozen (map (fn [e]
                          (if (= 'let (first e))
                            (list 'let (second e) (list 'is (cons 'noon.test/frozen (rest (nth e 2)))))
                            (list 'is (cons 'noon.test/frozen (rest e)))))
                        (get grouped true))
            [_ ns & ns-body] ns-decl
            enriched-body (-> (group-by first (filter seq? ns-body))
                              (update :require (fn [reqs] (map (fn [req]
                                                                 (concat req
                                                                         '([clojure.test :refer [deftest is]] [noon.test])))
                                                               reqs))))]
        (spit clj-file
              (with-out-str (mapv (fn [e] (clojure.pprint/pprint e) (println))
                                  (cons (list* 'ns (symbol (str (name ns) "-test"))
                                               "This file is generated from `src/noon/doc/examples.org`"
                                               (map first (vals enriched-body)))
                                        (concat (get grouped false)
                                                (list (list* 'deftest 'main frozen)))))))))

    (comment (org-file->clojure-expressions "src/noon/doc/examples.org")
             (org->test "src/noon/doc/examples.org"
                        "test/noon/doc/examples_test.clj")))
