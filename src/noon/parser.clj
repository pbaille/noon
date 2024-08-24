(ns noon.parser
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [noon.constants :as constants]
            [noon.harmony :as h]
            [noon.utils.misc :as u]
            [clojure.string :as str]))

(do :move-to-harmony

    (defn degree-alteration [scale-idx c-val]
      (fn [harmonic-context]
        (let [scale (:scale harmonic-context)
              scale-size (count scale)
              _ (when (>= scale-idx scale-size)
                  (u/throw* `degree-alteration " scale-idx out of bounds."))
              current-c-val (get scale scale-idx)]
          (if (= current-c-val c-val)
            harmonic-context
            (let [below-c-val (get scale (dec scale-idx))
                  above-c-val (if (< scale-idx scale-size) (get scale (inc scale-idx)))]
              (if (and (> c-val below-c-val)
                       (or (not above-c-val) (< c-val above-c-val)))
                (update harmonic-context :scale assoc scale-idx c-val)
                (u/throw* `degree-alteration " conflict, your alteration overlaps neighbour degrees.")))))))

    (defn structure-add [x]
      (fn [harmonic-context]
        (update harmonic-context :structure
                (fn [s] (vec (sort (conj (set s) x)))))))

    (defn structure-remove [x]
      (fn [harmonic-context]
        (update harmonic-context :structure
                (fn [s] (vec (sort (disj (set s) x))))))))

(do :parser
    (def parser
      (insta/parser (slurp (io/resource "noon.bnf"))))

    (defn parse [& xs]
      (insta/parse parser (str/join "." (map name xs)))))

(do :parsed-leaf-convertion

    (defn roman-degree->natural-pitch-class [v]
      (case v
        :one :C
        :two :D
        :three :E
        :four :F
        :five :G
        :six :A
        :seven :B))

    (defn scale-degree->natural-pitch-class [v]
      (case v
        :second :D
        :third :E
        :fourth :F
        :fifth :G
        :sixth :A
        :seventh :B))

    (defn alteration->chromatic-offset [v]
      (case v
        :double-bemol -2
        :bemol -1
        :natural 0
        :sharp 1
        :double-sharp 2))

    (defn scale-degree->scale-idx [v]
      (case v
        :second 1
        :third 2
        :fourth 3
        :fifth 4
        :sixth 5
        :seventh 6))

    (defn omission->removed-scale-idx [v]
      (case v
        :omit1 0
        :omit3 2
        :omit5 4))

    (defn suspension->added-scale-idx [v]
      (case v
        :sus2 1
        :sus4 3))

    (defn string-digit->scale-idx [v]
      (get {"1" 0
            "2" 1
            "3" 2
            "4" 3
            "5" 4
            "6" 5
            "7" 6} v)))

(do :parsed-tree->update

    (defn pitch-offset [natural-pitch-class alteration]
      (-> (constants/get-pitch-class natural-pitch-class)
          (update :c + (alteration->chromatic-offset alteration))))

    (defn degree-update [degree alteration]
      (let [offset (pitch-offset (roman-degree->natural-pitch-class degree) alteration)
            minimal-offset (if (> (:c offset) 6)
                             (merge-with - offset {:d 7 :c 12})
                             offset)]
        (fn [harmonic-context]
          (update harmonic-context :origin
                  (fn [o] (merge-with + o minimal-offset))))))

    (defn degree-alteration-update [degree alteration]
      (let [c-val (:c (-> (scale-degree->natural-pitch-class degree)
                          (pitch-offset alteration)))
            scale-idx (scale-degree->scale-idx degree)]
        (degree-alteration scale-idx c-val)))

    (defn structure-addition-update [degree alteration]
      (comp (degree-alteration-update degree alteration)
            (structure-add (scale-degree->scale-idx degree))))

    (defn base-structure-update [structure]
      (let [structure-update
            (case structure
              (:major
               :minor
               :diminished
               :augmented) (h/structure :triad)
              (:major-seventh
               :diminished-seventh
               :minor-seventh
               :dominant
               :half-diminished
               :minor-major-seventh) (h/structure :tetrad))
            degree-updates
            (mapv (fn [[degree alteration]]
                    (degree-alteration-update degree alteration))
                  (case structure
                    :major [[:third :natural] [:fifth :natural]]
                    :minor [[:third :bemol] [:fifth :natural]]
                    :diminished [[:third :bemol] [:fifth :bemol]]
                    :augmented [[:third :natural] [:fifth :sharp]]
                    :major-seventh [[:third :natural] [:fifth :natural] [:seventh :natural]]
                    :diminished-seventh [[:third :bemol] [:fifth :bemol] [:seventh :diminished]]
                    :minor-seventh [[:third :bemol] [:fifth :natural] [:seventh :bemol]]
                    :dominant [[:third :natural] [:fifth :natural] [:seventh :bemol]]
                    :half-diminished [[:third :bemol] [:fifth :bemol] [:seventh :bemol]]
                    :minor-major-seventh [[:third :bemol] [:fifth :natural] [:seventh :natural]]))]
        (reduce comp structure-update degree-updates)))

    (defn chain-update [xs]
      (let [[x & xs] (reverse xs)]
        (reduce comp x xs)))

    (defn parsed-tree->update
      [[type & [[x1] [x2] :as content]]]
      (case type
        (:abstract-chord
         :abstract-mode
         :concrete-chord
         :concrete-mode
         :structure-modifiers
         :mode-alterations) (chain-update (mapv parsed-tree->update content))
        :degree (degree-update x2 x1)
        :root (h/root (pitch-offset x1 x2))
        :base-structure (base-structure-update x1)
        :mode (h/scale x1)
        :structure-addition (structure-addition-update x2 x1)
        :structure-suspension (comp (structure-remove 2) (structure-add (suspension->added-scale-idx x1)))
        :structure-omission (structure-remove (omission->removed-scale-idx x1))
        :altered-degree (degree-alteration-update x2 x1)
        :augmented-fifth (degree-alteration-update :fifth :sharp)
        :augmented-structure (structure-addition-update :fifth :sharp)
        :structure-shorthand (h/structure (mapv string-digit->scale-idx content)))))

(comment :tries

         (defn ? [& xs]
           (parsed-tree->update (first (apply parse xs))))

         (defn ?? [& xs]
           ((apply ? xs)
            h/hc0))

         (comment
           (?? "aeolian")
           (?? "aeolianb2")
           (parse "D#aeolianb2")
           (?? "D#aeolianb2")
           (?? :Vlydian+#2)
           (?? :IIm)
           (?? :IImM713omit1)
           (parse :dorianb2 :s2467)
           (?? :dorianb2 :s2467))

         (comment
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
           (parse "ionian#2")))
