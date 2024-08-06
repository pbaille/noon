(ns hooks.noon.score
  (:require hooks.noon.constants))

(defmacro lambda
  [arg & body]
  `(fn [~arg] ~@body))

(defmacro lambda_ [& body]
  `(fn [~'_] ~@body))

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
                        (list 'def (with-meta (symbol (str "dur" i))
                                     {:doc (str "Multiply event duration by " i)
                                      :tags [:event-update :alias :temporal]})
                              `(noon.score/dur (noon.score/mul ~i)))
                        (list 'def (with-meta (symbol (str "dur:" i))
                                     {:doc (str "Divide event duration by " i)
                                      :tags [:event-update :alias :temporal]})
                              `(noon.score/dur (noon.score/div ~i)))))
                (for [n (range 2 12)
                      d (range 2 12)]
                  (list 'def (with-meta (symbol (str "dur" n ":" d))
                               {:doc (str "Multiply event duration by " n "/" d)
                                :tags [:event-update :alias :temporal]})
                        `(noon.score/dur (noon.score/mul (/ ~n ~d))))))))

(defmacro -def-velocities []
  (cons 'do
        (for [i (range 1 13)]
          (let [v (int (* i (/ 127 12)))]
            (list 'def (with-meta (symbol (str "vel" i))
                         {:doc (str "Set event velocity to " v)
                          :tags [:event-update :alias]})
                  `(noon.score/vel ~v))))))

(defmacro -def-channels []
  (cons 'do
        (for [i (range 0 16)]
          (list 'def (with-meta (symbol (str "chan" i))
                       {:doc (str "Set event midi channel to " i)
                        :tags [:event-update :alias]})
                `(noon.score/chan ~i)))))

(defmacro -def-tracks []
  (cons 'do
        (for [i (range 0 16)]
          (list 'def (with-meta (symbol (str "track" i))
                       {:doc (str "Set event midi channel to " i)
                        :tags [:event-update :alias]})
                `(noon.score/track ~i)))))

(defmacro -def-wrapped [wrapper m]
  (cons 'do (for [[k v] (eval (symbol (str "hooks." (namespace m)) (name m)))]
              (list 'def
                    (with-meta (symbol (name k))
                      {:tags [:event-update :alias :harmonic]
                       :doc (str "Alias for " (list (symbol "noon.score" (name wrapper)) v))})
                    (list wrapper v)))))

(defmacro -def-steps [name prefix max f]
  (cons 'do
        (mapcat
         (fn [n]
           [(list 'def (with-meta (symbol (str prefix n))
                         {:doc (str "Step up "
                                    n " " name " " (if (> n 1) "steps" "step") ".")
                          :tags [:event-update :harmonic]})
                  (list f n))
            (list 'def (with-meta (symbol (str prefix n "-"))
                         {:doc (str "Step down " n " " name " " (if (> n 1) "steps" "step") ".")
                          :tags [:event-update :harmonic]})
                  (list f (list `- n)))])
         (range 1 max))))

(defmacro -def-shifts [name prefix max f]
  (cons 'do
        (mapcat
         (fn [n]
           [(list 'def (with-meta (symbol (str prefix n))
                         {:doc (str "Shift up "
                                    n " " name (when (> n 1) "s") ".")
                          :tags [:event-update :harmonic]})
                  (list f n))
            (list 'def (with-meta (symbol (str prefix n "-"))
                         {:doc (str "Shift down " n " " name (when (> n 1) "s") ".")
                          :tags [:event-update :harmonic]})
                  (list f (list `- n)))])
         (range 1 max))))

(defmacro -def-degrees []
  (cons 'do
        (concat (for [[n v] (map vector '[I II III IV V VI VII] (range))]
                  (list 'def (with-meta n
                               {:doc (str "Go to degree " n)
                                :tags [:event-update :harmonic]})
                        (list 'noon.score/degree v)))
                (for [[degree-sym degree-val] (map vector '[I II III IV V VI VII] (range))
                      [alteration-sym alteration-val] [["#" 'noon.score/c1] ["b" 'noon.score/c1-]]]
                  (let [[dn dv an av] [degree-sym degree-val alteration-sym alteration-val]]
                    (list 'def (with-meta (symbol (str dn an))
                                 {:doc (str "Go to degree " an dn)
                                  :tags [:event-update :harmonic]})
                          `[(noon.score/transpose ~av) (noon.score/degree ~dv)]))))))
