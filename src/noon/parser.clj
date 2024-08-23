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

(defn root->pitch-class [[_ [pitch-class] [alteration]]]
  (-> (constants/get-pitch-class pitch-class)
      (update :c + (case alteration
                     :double-bemol -2 :bemol -1
                     :natural 0 :sharp 1 :double-sharp 2))))

(defn degree->offset [[_ [alteration] [degree]]]
  (root->pitch-class
   [nil
    (case degree
      :one :C :two :D :three :E
      :four :F :five :G :six :A :sevent :B)
    alteration]))

(defn degree->update [tree]
  (let [offset (degree->offset tree)
        minimal-offset (if (> (:c offset) 6)
                         (merge-with - offset {:d -7 :c -12})
                         offset)]
    (fn [harmonic-context]
      (update harmonic-context :origin
              (fn [o] (merge-with + o minimal-offset))))))

(defn root->update [tree]
  (let [pitch-class (root->pitch-class tree)
        candidates (iterate (fn [pc] (-> (update pc :d + 7) (update :c + 12))) pitch-class)]
    (fn [harmonic-context]
      (let [c-origin (get-in harmonic-context [:origin :c])]
        (assoc harmonic-context :origin
               (first (sort-by (fn [candidate] (abs (- (:c candidate) c-origin)))
                               candidates)))))))

(defn mode->update [[_ [mode]]]
  (h/scale mode))

(defn degree-alteration-update [alteration degree]
  (fn [harmonic-context]
    ()))

(defn base-structure->update [[_ [structure]]]
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

(defn structure-suspension->update [[_]])

(defn structure-omission->update [[_]])

(defn altered-degree->update [[_ [alteration] [degree]]]
  (degree-alteration-update alteration degree))

(defn structure-addition-update [alteration degree]
  (comp (degree-alteration-update alteration degree)
        (fn [harmonic-context]
          (update harmonic-context :structure
                  (fn [s] (->> (case degree :second 1 :third 2 :fourth 3 :fifth 4 :sixth 5 :seventh 6)
                               (into (set s))
                               (sort)
                               (vec)))))))

(defn structure-addition->update [[_ [alteration] [degree]]]
  (structure-addition-update alteration degree))

(defn parsed->update [tree]
  (case (first tree)
    (:abstract-chord
     :abstract-mode
     :concrete-chord
     :concrete-mode
     :structure-modifiers
     :mode-alterations) (mapv parsed->update (next tree))
    :degree (degree->update tree)
    :root (root->update tree)
    :base-structure (base-structure->update tree)
    :mode (mode->update tree)
    :structure-addition (structure-addition->update tree)
    :structure-suspension (structure-suspension->update tree)
    :structure-omission (structure-omission->update tree)
    :altered-degree (altered-degree->update tree)
    :augmented-fifth (degree-alteration-update :sharp :fifth)
    :augmented-structure (structure-addition-update :sharp :fifth) ))
