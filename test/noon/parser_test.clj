(ns noon.parser-test
  (:require [noon.parser :as p :refer [parse]]
            [clojure.test :refer [testing deftest is]]
            [noon.harmony :as h]))

(deftest parser

  (testing "chords"

    (testing "triads"
      (is (= (parse :m)
             (list [:abstract-chord [:base-structure [:minor]]])))

      (is (= (parse :o)
             (list [:abstract-chord [:base-structure [:diminished]]])))

      (is (= (parse "")
             (parse "M")
             (list [:abstract-chord [:base-structure [:major]]]))))

    (testing "root and degree"

      (is (= (parse "D")
             (list [:concrete-chord
                    [:root [:D] [:natural]]
                    [:base-structure [:major]]])))

      (is (= (parse "Bbbm")
             (list [:concrete-chord
                    [:root [:B] [:double-bemol]]
                    [:base-structure [:minor]]])))

      (is (= (parse "bII+")
             (list [:concrete-chord
                    [:degree [:flat] [:two]]
                    [:base-structure [:augmented]]])))

      (is (= (parse :#IVo)
             (list [:concrete-chord
                    [:degree [:sharp] [:four]]
                    [:base-structure [:diminished]]]))))

    (testing "tetrads"

      (is (= (parse :mΔ)
             (parse :mM7)
             (list [:abstract-chord
                    [:base-structure [:minor-major-seventh]]])))

      (is (= (parse :ø)
             (list [:abstract-chord
                    [:base-structure [:half-diminished]]])))

      (is (= (parse :o7)
             (list [:abstract-chord
                    [:base-structure [:diminished-seventh]]]))))

    (testing "modifiers"

      (is (= (parse :m7b5)
             (list [:abstract-chord
                    [:base-structure [:minor-seventh]]
                    [:structure-modifiers
                     [:structure-addition [:bemol] [:fifth]]]])))

      (is (= (parse :M79#11)
             (list [:abstract-chord
                    [:base-structure [:major-seventh]]
                    [:structure-modifiers
                     [:structure-addition [:natural] [:second]]
                     [:structure-addition [:sharp] [:fourth]]]])))

      (is (= (parse :Eb7sus4)
             (list [:concrete-chord
                    [:root [:E] [:bemol]]
                    [:base-structure [:dominant]]
                    [:structure-modifiers
                     [:structure-suspension [:sus4]]]])))

      (is (= (parse :Fomit3)
             (list [:concrete-chord
                    [:root [:F] [:natural]]
                    [:base-structure [:major]]
                    [:structure-modifiers [:structure-omission [:omit3]]]])))

      (is (= (parse :E7b913)
             (list [:concrete-chord
                    [:root [:E] [:natural]]
                    [:base-structure [:dominant]]
                    [:structure-modifiers
                     [:structure-addition [:bemol] [:second]]
                     [:structure-addition [:natural] [:sixth]]]])))

      (is (= (parse :CM7+)
             (parse :CΔ+)
             (list [:concrete-chord
                    [:root [:C] [:natural]]
                    [:base-structure [:major-seventh]]
                    [:structure-modifiers
                     [:augmented-structure]]])))))

  (testing "modes"

    (testing "basics"

      (is (= (parse :ionian)
             (list [:abstract-mode [:mode [:ionian]]])))

      (is (= (parse :Dionian)
             (list [:concrete-mode [:root [:D] [:natural]]
                    [:mode [:ionian]]])))

      (is (= (parse :Vlydian)
             (list [:concrete-mode
                    [:degree [:natural] [:five]]
                    [:mode [:lydian]]])))

      (is (= (parse :#IValtered)
             (list [:concrete-mode
                    [:degree [:sharp] [:four]]
                    [:mode [:altered]]]))))

    (testing "modifiers"

      (is (= (parse :Eblocrian2)
             (parse :Eb.locrian2)
             (list [:concrete-mode
                    [:root [:E] [:bemol]]
                    [:mode [:locrian]]
                    [:mode-alterations
                     [:altered-degree [:natural] [:second]]]])))

      (is (and (= (parse :lydian+#2)
                  (list [:abstract-mode
                         [:mode [:lydian]]
                         [:mode-alterations
                          [:augmented-fifth]
                          [:altered-degree [:sharp] [:second]]]]))
               (= (parse :lydian#5#2)
                  (list [:abstract-mode
                         [:mode [:lydian]]
                         [:mode-alterations
                          [:altered-degree [:sharp] [:fifth]]
                          [:altered-degree [:sharp] [:second]]]]))
               (= (parse :lydian#2+)
                  (list [:abstract-mode
                         [:mode [:lydian]]
                         [:mode-alterations
                          [:altered-degree [:sharp] [:second]]
                          [:augmented-fifth]]]))))

      (is (= (parse :alteredbb7)
             (parse :altered.bb7)
             (list [:abstract-mode
                    [:mode [:altered]]
                    [:mode-alterations
                     [:altered-degree [:double-bemol] [:seventh]]]]))))

    (testing "structure shorthand"

      (is (= (parse :aeolian.s2367)
             (list [:abstract-mode
                    [:mode [:aeolian]]
                    [:structure-shorthand "2" "3" "6" "7"]])))

      (is (= (parse :phrygian6.s2467)
             (list [:abstract-mode
                    [:mode [:phrygian]]
                    [:mode-alterations
                     [:altered-degree [:natural] [:sixth]]]
                    [:structure-shorthand "2" "4" "6" "7"]]))))))

(defn upd [& xs]
  (h/upd h/hc0
         (p/parsed-tree->update (first (apply parse xs)))))

(deftest interpretation

  (testing "root change"

    (is (= (upd :F)
           (upd :IV)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 38, :c 65}, :position {:t 0, :s 0, :d 0, :c 0}}))

    (testing "closest root"
      (is (= (upd :G)
             (upd :V)
             {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 32, :c 55}, :position {:t 0, :s 0, :d 0, :c 0}})))

    (is (= (upd :Eb)
           (upd :bIII)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 37, :c 63}, :position {:t 0, :s 0, :d 0, :c 0}})))

  (testing "chords"

    (is (= (upd :m)
           {:scale [0 2 3 5 7 9 11],
            :structure [0 2 4],
            :origin {:d 35, :c 60},
            :position {:t 0, :s 0, :d 0, :c 0}}))

    ;; this should not be this
    ;; moving by degree somehow preserve original scale
    ;; in this case it should be an error because E phrygian can't have sharp fifth without colliding with minor sixth
    (is (not (= (upd :E+)
                (upd :III+)
                {:scale [0 2 4 5 8 9 11], :structure [0 2 4], :origin {:d 37, :c 64}, :position {:t 0, :s 0, :d 0, :c 0}})))))
