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
         (parse "C#m7")
         (parse "m7")
         (parse "C#m7b13")
         (parse "bVImΔ9")
         (parse "bVImΔ913")
         (parse "bVImΔ9.13")
         (parse "#IIm7b5sus4")
         (parse "IIø♮2")
         (parse "ionianb2b3")
         (parse "dorian#4"))

(defn parsed-root->pitch-class [[_ [pitch-class] [alteration]]]
  (-> (constants/get-pitch-class pitch-class)
      (update :c + (case alteration
                     :double-bemol -2 :bemol -1
                     :natural 0 :sharp 1 :double-sharp 2))))

(defn parsed-degree->offset [[_ [alteration] [degree]]]
  (parsed-root->pitch-class
   [nil
    (case degree
      :one :C :two :D :three :E
      :four :F :five :G :six :A :sevent :B)
    alteration]))

(defn parsed-degree->harmonic-update [tree]
  (let [offset (parsed-degree->offset tree)
        minimal-offset (if (> (:c offset) 6)
                         (merge-with - offset {:d -7 :c -12})
                         offset)]
    (fn [harmonic-context]
      (update harmonic-context :origin
              (fn [o] (merge-with + o minimal-offset))))))

(defn parsed-root->harmonic-update [tree]
  (let [pitch-class (parsed-root->pitch-class tree)
        candidates (iterate (fn [pc] (-> (update pc :d + 7) (update :c + 12))) pitch-class)]
    (fn [harmonic-context]
      (let [c-origin (get-in harmonic-context [:origin :c])]
        (assoc harmonic-context :origin
               (first (sort-by (fn [candidate] (abs (- (:c candidate) c-origin)))
                               candidates)))))))

(defn parsed-mode->harmonic-update [[_ [mode]]]
  )

(defn parsed->update [[type & content :as tree]]
    (case type
      (:abstract-chord
       :abstract-mode
       :concrete-chord
       :concrete-mode
       :modifiers) (mapv parsed->update content)
      :degree (parsed-degree->harmonic-update tree)
      :root (parsed-root->harmonic-update tree)
      :base-structure (set-base-structure (first content))
      :mode (set-scale (first content))
      :structure-addition (structure-addition content)
      :structure-suspension (structure-suspension (first content))
      :structure-omission (structure-omission (first content))
      :modal-alteration (scale-alteration content)))
