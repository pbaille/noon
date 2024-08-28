(ns noon.parse.harmony-test
  (:require [noon.parse.harmony :as p :refer [parse]]
            [clojure.test :refer [testing deftest is]]
            [noon.harmony :as h]))

(deftest parser

  (testing "chords"

    (testing "triads"
      (is (= (parse :m)
             (list [:structure
                    [:structure/base [:triad/minor]]
                    [:structure/modifiers]])))

      (is (= (parse :o)
             (list [:structure
                    [:structure/base [:triad/diminished]]
                    [:structure/modifiers]])))

      (is (= (parse "M")
             (list [:structure
                    [:structure/base [:triad/major]]
                    [:structure/modifiers]]))))

    (testing "root and degree"

      (is (= (parse "D")
             (list [:root [:D] [:natural]])))

      (is (= (parse "Bbbm")
             (list [:root [:B] [:double-bemol]]
                   [:structure [:structure/base [:triad/minor]] [:structure/modifiers]])))

      (is (= (parse "bII+")
             (list [:degree [:bemol] [:two]]
                   [:structure
                    [:structure/base [:triad/augmented]]
                    [:structure/modifiers]])))

      (is (= (parse :#IVo)
             (list [:degree [:sharp] [:four]]
                   [:structure
                    [:structure/base [:triad/diminished]]
                    [:structure/modifiers]])))

      (testing "secondary degrees"
        (is (= (parse :V/II.7b9)
               (list [:secondary-degree
                      [:degree [:natural] [:five]]
                      [:degree [:natural] [:two]]]
                     [:structure
                      [:structure/base [:tetrad/dominant]]
                      [:structure/modifiers [:structure.modifier/degree [:bemol] [:second]]]])))))

    (testing "tetrads"

      (is (= (parse :mΔ)
             (parse :mM7)
             (list [:structure [:structure/base [:tetrad/minor-major-seventh]] [:structure/modifiers]])))

      (is (= (parse :ø)
             (list [:structure [:structure/base [:tetrad/half-diminished]] [:structure/modifiers]])))

      (is (= (parse :o7)
             (list [:structure [:structure/base [:tetrad/diminished-seventh]] [:structure/modifiers]]))))

    (testing "modifiers"

      (is (= (parse :m7b5)
             (list [:structure
                    [:structure/base [:tetrad/minor-seventh]]
                    [:structure/modifiers [:structure.modifier/degree [:bemol] [:fifth]]]])))

      (is (= (parse :M79#11)
             (list [:structure
                    [:structure/base [:tetrad/major-seventh]]
                    [:structure/modifiers
                     [:structure.modifier/degree [:natural] [:second]]
                     [:structure.modifier/degree [:sharp] [:fourth]]]])))

      (is (= (parse :Eb7sus4)
             (list [:root [:E] [:bemol]]
                   [:structure
                    [:structure/base [:tetrad/dominant]]
                    [:structure/modifiers
                     [:structure.modifier/omission [:omit3]]
                     [:structure.modifier/degree [:natural] [:fourth]]]])))

      (is (= (parse :Fomit3)
             (list [:root [:F] [:natural]]
                   [:structure [:structure/modifiers [:structure.modifier/omission [:omit3]]]])))

      (is (= (parse :E7b913)
             (list [:root [:E] [:natural]]
                   [:structure
                    [:structure/base [:tetrad/dominant]]
                    [:structure/modifiers
                     [:structure.modifier/degree [:bemol] [:second]]
                     [:structure.modifier/degree [:natural] [:sixth]]]])))

      (is (= (parse :CM7+)
             (parse :CΔ+)
             (list [:root [:C] [:natural]]
                   [:structure
                    [:structure/base [:tetrad/major-seventh]]
                    [:structure/modifiers [:structure.modifier/augmented]]])))

      (is (= (parse :V/II.7b9)
             (list [:secondary-degree
                    [:degree [:natural] [:five]]
                    [:degree [:natural] [:two]]]
                   [:structure
                    [:structure/base [:tetrad/dominant]]
                    [:structure/modifiers [:structure.modifier/degree [:bemol] [:second]]]])))

      (is (= (parse :V7b9omit1)
             (list [:degree [:natural] [:five]]
                   [:structure
                    [:structure/base [:tetrad/dominant]]
                    [:structure/modifiers
                     [:structure.modifier/degree [:bemol] [:second]]
                     [:structure.modifier/omission [:omit1]]]]))))

    (testing "structure-shorthand"

      (is (= (parse :s2367)
             (list [:structure/shorthand "2" "3" "6" "7"])))))

  (testing "modes"

    (testing "basics"

      (is (= (parse :ionian)
             (list [:mode [:mode/base [:ionian]]])))

      (is (= (parse :Dionian)
             (list [:root [:D] [:natural]]
                   [:mode [:mode/base [:ionian]]])))

      (is (= (parse :Vlydian)
             (list [:degree [:natural] [:five]]
                   [:mode [:mode/base [:lydian]]])))

      (is (= (parse :#IValtered)
             (list [:degree [:sharp] [:four]] [:mode [:mode/base [:altered]]]))))

    (testing "modifiers"

      (is (= (parse :Eblocrian2)
             (parse :Eb.locrian2)
             (list [:root [:E] [:bemol]]
                   [:mode
                    [:mode/base [:locrian]]
                    [:mode/alterations [:mode.alteration/degree [:natural] [:second]]]])))

      (is (and (= (parse :lydian+#2)
                  (list [:mode [:mode/base [:lydian]] [:mode/alterations [:mode.alteration/augmented-fifth] [:mode.alteration/degree [:sharp] [:second]]]]))
               (= (parse :lydian#5#2)
                  (list [:mode [:mode/base [:lydian]] [:mode/alterations [:mode.alteration/degree [:sharp] [:fifth]] [:mode.alteration/degree [:sharp] [:second]]]]))
               (= (parse :lydian#2+)
                  (list [:mode [:mode/base [:lydian]] [:mode/alterations [:mode.alteration/degree [:sharp] [:second]] [:mode.alteration/augmented-fifth]]]))))

      (is (= (parse :alteredbb7)
             (parse :altered.bb7)
             (list [:mode [:mode/base [:altered]] [:mode/alterations [:mode.alteration/degree [:double-bemol] [:seventh]]]]))))

    (testing "structure shorthand"

      (is (= (parse :aeolian.s2367)
             (list [:mode [:mode/base [:aeolian]]]
                   [:structure/shorthand "2" "3" "6" "7"])))

      (is (= (parse :phrygian6.s2467)
             (list [:mode [:mode/base [:phrygian]] [:mode/alterations [:mode.alteration/degree [:natural] [:sixth]]]]
                   [:structure/shorthand "2" "4" "6" "7"]))))))

(defn upd [& xs]
  (h/upd h/hc0
         (h/rebase (apply p/interpret xs))))

(deftest interpretation

  (testing "root change"

    (is (= (upd :F)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 38, :c 65}, :position {:t 0, :s -1, :d 0, :c 0}}))

    (testing "closest root"
      (is (= (upd :G)
             {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 32, :c 55}, :position {:t 0, :s 1, :d 1, :c 0}})))

    (is (= (upd :Eb)
           {:scale [0 2 4 5 7 9 11], :structure [0 2 4], :origin {:d 37, :c 63}, :position {:t 0, :s -1, :d 1, :c 0}})))

  (testing "degree change"
    (is (= (upd :IV)
           {:scale [0 2 4 6 7 9 11], :structure [0 2 4], :origin {:d 38, :c 65}, :position {:t 0, :s -1, :d 0, :c 0}}))

    (is (= (upd :bIII)
           {:scale [0 2 4 6 8 9 11], :structure [0 2 4], :origin {:d 37, :c 63}, :position {:t 0, :s -1, :d 1, :c 0}}))

    (testing "closest move"
      (is (= (upd :V)
             {:scale [0 2 4 5 7 9 10], :structure [0 2 4], :origin {:d 32, :c 55}, :position {:t 0, :s 1, :d 1, :c 0}}))))

  (testing "chords"

    (is (= (upd :m)
           {:scale [0 2 3 5 7 9 11], :structure [0 2 4], :origin {:d 35, :c 60}, :position {:t 0, :s 0, :d 0, :c 0}}))

    (is (= (upd :E+)
           {:scale [0 2 4 5 8 9 11], :structure [0 2 4], :origin {:d 37, :c 64}, :position {:t 0, :s -1, :d 0, :c 0}}))

    (is (= (upd :IIIM7)
           {:scale [0 1 4 5 7 8 11], :structure [0 2 4 6], :origin {:d 37, :c 64}, :position {:t 0, :s -2, :d 1, :c 0}}))

    (is (= (upd :IIImM7)
           {:scale [0 1 3 5 7 8 11], :structure [0 2 4 6], :origin {:d 37, :c 64}, :position {:t 0, :s -2, :d 1, :c 0}})))

  (testing "chaining"

    (is (= (upd :IIImelm :mM79 :omit1)
           {:scale [0 2 3 5 7 9 11], :structure [1 2 4 6], :origin {:d 37, :c 64}, :position {:t 0, :s -1, :d -1, :c -1}}))))
