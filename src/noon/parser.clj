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
      (insta/parser (slurp (io/resource "noon.bnf"))
                    :allow-namespaced-nts true))

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
            degree-shift (if (> (:c offset) 6)
                           (- (:d offset) 7)
                           (:d offset))]
        (fn [{:as harmonic-context :keys [scale]}]
          (h/upd harmonic-context
                 (when (not= (get (:d offset) scale) (:c offset))
                   (degree-alteration (:d offset) (:c offset)))
                 (h/degree degree-shift)))))

    (defn degree-alteration-update [degree alteration]
      (let [c-val (:c (-> (scale-degree->natural-pitch-class degree)
                          (pitch-offset alteration)))
            scale-idx (scale-degree->scale-idx degree)]
        (degree-alteration scale-idx c-val)))

    (defn structure-addition-update [degree alteration]
      [(degree-alteration-update degree alteration)
       (structure-add (scale-degree->scale-idx degree))])

    (defn base-structure-update [structure]
      (let [type (keyword (namespace structure))
            structure-name (keyword (name structure))
            structure-update
            (case type
              :triad (h/structure :triad)
              :tetrad (h/structure :tetrad))
            degree-updates
            (mapv (fn [[degree alteration]]
                    (degree-alteration-update degree alteration))
                  (case structure-name
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
        (vec (cons structure-update degree-updates))))

    (defn parsed-tree->update
      [[type & [[x1] [x2] :as content]]]
      (h/->hc-update
       (case type
         (:mode
          :structure
          :structure/modifiers
          :mode/alterations) (mapv parsed-tree->update content)
         :degree (degree-update x2 x1)
         :root (h/root (pitch-offset x1 x2))
         :structure/base (base-structure-update x1)
         :mode/base (h/scale x1)
         :structure.modifier/degree (structure-addition-update x2 x1)
         :structure.modifier/omission (structure-remove (omission->removed-scale-idx x1))
         :mode.alteration/degree (degree-alteration-update x2 x1)
         :mode.alteration/augmented-fifth (degree-alteration-update :fifth :sharp)
         :structure.modifier/augmented (structure-addition-update :fifth :sharp)
         :structure/shorthand (h/structure (mapv string-digit->scale-idx content)))))

    (defn interpret [& xs]
      (h/->hc-update
       (mapv parsed-tree->update (apply parse xs)))))

(comment :tries

         (defn ?? [& xs]
           ((apply interpret xs)
            h/hc0))

         (comment
           (?? :m)
           (?? "aeolian")
           (?? "aeolianb2")
           (parse "D#aeolianb2")
           (?? "D#aeolianb2")
           (?? :Vlydian+#2)
           (?? :IIm)
           (?? :IImM713omit1)
           (parse :dorianb2 :s2467)
           (?? :dorianb2 :s2467)
           (parse :bIIIsus4)

           (?? :bIIIsus4)
           )

         (comment
           (parse "D")
           (parse :m)
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
