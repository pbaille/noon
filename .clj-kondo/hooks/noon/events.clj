(ns hooks.noon.events
  (:require [hooks.noon.constants :as constants]))

(defmacro import-wrap-harmony-update-constructors [& xs]
  `(do ~@(mapv (fn [x]
                 (macroexpand `(defn ~x ~'[& _] nil)))
               xs)))

(defmacro import-wrap-harmony-updates [& xs]
  `(do ~@(mapv (fn [x]
                 (macroexpand `(def ~x nil)))
               xs)))

(defmacro -def-durations []
  (cons 'do
        (concat (for [i (range 2 12)]
                  (list 'do
                        (list 'def (symbol (str "dur" i))
                              `(noon.updates/dur (noon.numbers/mul ~i)))
                        (list 'def (symbol (str "dur:" i))
                              `(noon.updates/dur (noon.numbers/div ~i)))))
                (for [n (range 2 12)
                      d (range 2 12)]
                  (list 'def (symbol (str "dur" n ":" d))
                        `(noon.updates/dur (noon.numbers/mul (/ ~n ~d))))))))

(defmacro -def-velocities []
  (cons 'do
        (for [i (range 1 13)]
          (let [v (int (* i (/ 127 12)))]
            (list 'def (symbol (str "vel" i))
                  `(noon.updates/vel ~v))))))

(defmacro -def-channels []
  (cons 'do
        (for [i (range 0 16)]
          (list 'def (symbol (str "chan" i))
                `(noon.updates/chan ~i)))))

(defmacro -def-tracks []
  (cons 'do
        (for [i (range 0 16)]
          (list 'def (symbol (str "track" i))
                `(noon.updates/track ~i)))))

(defmacro -def-wrapped [type wrapper]
  (let [entries (case type
                  :modes constants/modes
                  :structures constants/structures
                  :pitches constants/pitches)]
    (cons 'do (for [[k v] entries]
                (list 'def (symbol (name k)) (list (symbol "noon.updates" (name wrapper)) v))))))

(defmacro -def-steps [name prefix max f]
  (cons 'do
        (mapcat
         (fn [n]
           [(list 'def (symbol (str prefix n))
                  (list f n))
            (list 'def (symbol (str prefix n "-"))
                  (list f (list `- n)))])
         (range 1 max))))

(defmacro -def-shifts [name prefix max f]
  (cons 'do
        (mapcat
         (fn [n]
           [(list 'def (symbol (str prefix n))
                  (list f n))
            (list 'def (symbol (str prefix n "-"))
                  (list f (list `- n)))])
         (range 1 max))))

(defmacro -def-degrees []
  (cons 'do
        (concat (for [[n v] (map vector '[I II III IV V VI VII] (range))]
                  (list 'def n
                        (list 'noon.updates/degree v)))
                (for [[degree-sym degree-val] (map vector '[I II III IV V VI VII] (range))
                      [alteration-sym alteration-val] [["#" 'noon.updates/c1] ["b" 'noon.updates/c1-]]]
                  (let [[dn dv an av] [degree-sym degree-val alteration-sym alteration-val]]
                    (list 'def (symbol (str dn an))
                          `[(noon.updates/transpose ~av) (noon.updates/degree ~dv)]))))))
