(ns noon.parser
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [noon.constants :as constants]
            [noon.harmony :as h]))

(def parser
  (insta/parser (slurp (io/resource "noon.bnf"))))

(defn parse [s]
  (insta/parse parser s))

(comment :tries
         (parse "D")
         (parse "D6")
         (parse "Dm6")
         (parse "C#m7")
         (parse "Bbm7b5")
         (parse "m7")
         (parse "C#m7b13")
         (parse "bVImΔ9")
         (parse "bVIΔ9+")
         (parse "bVImΔ913")
         (parse "bVImΔ9.13")
         (parse "#IIm7b5sus4")
         (parse "IIø♮2")
         (parse "IIø2")
         (parse "ionianb2b3")
         (parse "dorian#4")
         (parse "ionian+#2")
         (parse "ionian#2"))

(defn root->pitch-class [natural-pitch-class alteration]
  (-> (constants/get-pitch-class natural-pitch-class)
      (update :c + (case alteration
                     :double-bemol -2 :bemol -1
                     :natural 0 :sharp 1 :double-sharp 2))))

(defn degree-offset [alteration degree]
  (root->pitch-class
   (case degree
     :one :C :two :D :three :E
     :four :F :five :G :six :A :sevent :B)
   alteration))

(defn degree-update [alteration degree]
  (let [offset (degree-offset alteration degree)
        minimal-offset (if (> (:c offset) 6)
                         (merge-with - offset {:d -7 :c -12})
                         offset)]
    (fn [harmonic-context]
      (update harmonic-context :origin
              (fn [o] (merge-with + o minimal-offset))))))

(defn root-update [natural-pitch-class alteration]
  (let [pitch-class (root->pitch-class natural-pitch-class alteration)
        candidates (iterate (fn [pc] (-> (update pc :d + 7) (update :c + 12))) pitch-class)]
    (fn [harmonic-context]
      (let [c-origin (get-in harmonic-context [:origin :c])]
        (assoc harmonic-context :origin
               (first (sort-by (fn [candidate] (abs (- (:c candidate) c-origin)))
                               candidates)))))))

(defn degree-alteration-update [alteration degree]
  (fn [harmonic-context]
    ()))

(defn base-structure-update [structure]
  (let [structure-update
        (case structure
          (:major :minor :diminished :augmented) (h/structure :triad)
          (:major-seventh
           :diminished-seventh
           :minor-seventh
           :dominant
           :half-diminished
           :minor-major-seventh) (h/structure :tetrad))
        degree-updates
        (mapv (fn [[alt deg]]
                (degree-alteration-update alt deg))
              (case structure
                :major [[:natural :third] [:natural :fifth]]
                :minor [[:bemol :third] [:natural :fifth]]
                :diminished [[:bemol :third] [:bemol :fifth]]
                :augmented [[:natural :third] [:sharp :fifth]]
                :major-seventh [[:natural :third] [:natural :fifth] [:natural :seventh]]
                :diminished-seventh [[:bemol :third] [:bemol :fifth] [:diminished :seventh]]
                :minor-seventh [[:bemol :third] [:natural :fifth] [:bemol :seventh]]
                :dominant [[:natural :third] [:natural :fifth] [:bemol :seventh]]
                :half-diminished [[:bemol :third] [:bemol :fifth] [:bemol :seventh]]
                :minor-major-seventh [[:bemol :third] [:natural :fifth] [:natural :seventh]]))]
    (reduce comp structure-update degree-updates)))

(defn structure-add [x]
  (fn [harmonic-context]
    (update harmonic-context :structure
            (fn [s] (vec (sort (into (set s) x)))))))

(defn structure-remove [x]
  (fn [harmonic-context]
    (update harmonic-context :structure
            (fn [s] (vec (sort (disj (set s) x)))))))

(defn structure-omission-update [omission]
  (structure-remove (case omission "1" 0 "3" 2 "5" 4)))

(defn structure-suspension-update [suspension]
  (comp (structure-remove 2)
        (structure-add (case suspension :sus2 1 :sus4 3))))

(defn structure-addition-update [alteration degree]
  (comp (degree-alteration-update alteration degree)
        (structure-add (case degree :second 1 :third 2 :fourth 3 :fifth 4 :sixth 5 :seventh 6))))

(defn parsed->update [[type & [[x1] [x2] :as content]]]
  (case type
    (:abstract-chord
     :abstract-mode
     :concrete-chord
     :concrete-mode
     :structure-modifiers
     :mode-alterations) (mapv parsed->update content)
    :degree (degree-update x1 x2)
    :root (root-update x1 x2)
    :base-structure (base-structure-update x1)
    :mode (h/scale x1)
    :structure-addition (structure-addition-update x1 x2)
    :structure-suspension (structure-suspension-update x1)
    :structure-omission (structure-omission-update x1)
    :altered-degree (degree-alteration-update x1 x2)
    :augmented-fifth (degree-alteration-update :sharp :fifth)
    :augmented-structure (structure-addition-update :sharp :fifth)))
