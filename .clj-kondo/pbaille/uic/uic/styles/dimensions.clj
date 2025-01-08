(ns uic.styles.dimensions)

(defmacro define-space-mixin

  [nam _css-prefix short-name]

  `(do (defn ~nam
         ([x#])

         ([x# y#])

         ([t# r# b# l#]))

       (def ~short-name ~nam)

       ~@(for [[n dirs] [["" [:right :left :top :bottom]]
                         ["x" [:right :left]]
                         ["y" [:top :bottom]]
                         ["t" [:top]]
                         ["b" [:bottom]]
                         ["l" [:left]]
                         ["r" [:right]]]
               size (range 5)]
           `(def ~(symbol (str short-name n size))
              (~nam ~(reduce #(assoc %1 %2 `(uic.styles.dimensions/space ~size)) {} dirs))))))
