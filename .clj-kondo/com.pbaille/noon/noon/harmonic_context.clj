(ns noon.harmonic-context)

(defmacro defsteps [prefix max f]
  (cons 'do
        (mapcat
         (fn [_]
           [(list 'def (symbol (str prefix _)) (list f _))
            (list 'def (symbol (str prefix _ "-")) (list f (list `- _)))])
         (range 1 max))))
