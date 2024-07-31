(ns noon.constants)

(def alt-sym->alt-val
  {"bb" -2
   "b" -1
   "" 0
   "#" 1
   "x" 2})

(def alt-val->alt-sym
  {-2 "bb"
   -1 "b"
   0 ""
   1 "#"
   2 "x"})

(def natural-pitch-class-syms
  ["C" "D" "E" "F" "G" "A" "B"])

(defn diat-sufix [n]
  (or (get
       {0 "st"
        1 "nd"
        2 "rd"} n)
      "th"))

(def alt-syms1
  {-2  "bb" -1 "b" 0 "P" 1 "+" 2 "x"})

(def alt-syms2
  {-2 "o" -1 "m" 0 "M" 1 "+" 2 "x"})

(def deg-alt-type
  {0 alt-syms1
   1 alt-syms2
   2 alt-syms2
   3 alt-syms1
   4 alt-syms1
   5 alt-syms2
   6 alt-syms2})

(def major-scale-steps
  [2 2 1 2 2 2 1])

(do :pitches

    (def pitch-classes
      (into {}
            (for [[dval cval] (map-indexed vector (butlast (reductions + 0 major-scale-steps)))
                  [altstr altval] alt-sym->alt-val]
              (let [nstr (natural-pitch-class-syms dval)
                    sym (symbol (str nstr altstr))]
                [sym {:d dval :c (+ cval altval)}]))))

    (def pitches
      (->> pitch-classes
           (mapcat (fn [[sym pitch]]
                     (map (fn [oct]
                            [(symbol (str sym (- oct 5)))
                             (-> pitch
                                 (update :c + (* 12 oct))
                                 (update :d + (* 7 oct)))])
                          (range 13))))
           (filter (fn [[_ {c :c}]] (<= 0 c 127)))
           (into {})))

    #_(pitch-class->pitches (pitch-classes 'D#)))

(do :modes

    (def major-scale-vals [0 2 4 5 7 9 11])
    (def melodic-minor-scale-vals [0 2 3 5 7 9 11])
    (def harmonic-minor-scale-vals [0 2 3 5 7 8 11])
    (def harmonic-major-scale-vals [0 2 4 5 7 8 11])
    (def double-harmonic-scale-vals [0 1 4 5 7 8 11])

    (defn scale-modes [scale]
      (mapv
       (fn [idx]
         (let [head (get scale idx)
               based (map #(- % head) scale)]
           (vec (sort (map #(if (> 0 %) (+ 12 %) %) based)))))
       (range (count scale))))

    (def major-modes
      (zipmap [:ionian :dorian :phrygian :lydian :mixolydian :eolian :locrian]
              (scale-modes major-scale-vals)))

    (def melodic-minor-modes
      (zipmap [:melodic-minor :phrygian6 :lydian+ :lydianb7 :mixolydianb6 :locrian2 :superlocrian]
              (scale-modes melodic-minor-scale-vals)))

    (def harmonic-minor-modes
      (zipmap [:harmonic-minor :locrian6 :ionian+ :dorian+4 :phrygian3 :lydian+2 :superlocriano7]
              (scale-modes harmonic-minor-scale-vals)))

    (def harmonic-major-modes
      (zipmap [:harmonic-major :dorianb5 :phrygianb4 :lydianb3 :mixolydianb2 :lydian++2 :locriano7]
              (scale-modes harmonic-major-scale-vals)))

    (def double-harmonic-modes
      (zipmap [:double-harmonic :lydian+2+6 :ultraphrygian :hungarian :oriental :ionian++2 :ultralocrian]
              (scale-modes double-harmonic-scale-vals)))

    (def modes (merge major-modes
                      melodic-minor-modes
                      harmonic-minor-modes
                      harmonic-major-modes
                      double-harmonic-modes))

    (def mode-aliases
      {:ion :ionian
       :dor :dorian
       :phry :phrygian
       :lyd :lydian
       :mix :mixolydian
       :eol :eolian
       :loc :locrian
       :melm :melodic-minor
       :phry6 :phrygian6
       :lyd+ :lydian+
       :lydb7 :lydianb7
       :mix+4 :lydianb7
       :mixb6 :mixolydianb6
       :loc2 :locrian2
       :alt :superlocrian
       :harmm :harmonic-minor
       :loc6 :locrian6
       :ion+ :ionian+
       :dor+4 :dorian+4
       :phryM :phrgian3
       :lyd+2 :lydian+2
       :altdim :superlocriano7})

    (def degree-priority
      {:ionian [6 3 2 5 1 4]
       :dorian [5 2 6 1 4 3]
       :phrygian [1 4 3 6 2 5]
       :lydian [3 6 2 5 1 4]
       :mixolydian [6 3 2 5 1 4]
       :eolian  [5 1 4 2 6 3]
       :locrian [4 1 6 5 2 3]

       :melodic-minor [6 2 5 1 4 3]
       :phrygian6 [5 1 3 6 2 4]
       :lydian+ [4 6 2 3 5 1]
       :lydianb7 [3 6 2 5 1 4]
       :mixolydianb6 [5 6 2 1 4 3]
       :locrian2 [4 2 6 5 1 3]
       :superlocrian [3 6 5 2 4 1]

       :harmonic-minor [5 6 1 2 4 3]
       :locrian6 [4 5 1 6 2 3]
       :ionian+ [3 4 6 2 1 5]
       :dorian+4 [5 3 2 1 6 4]
       :phrygian3 [1 2 4 6 5 3]
       :lydian+2 [3 1 6 2 5 4]
       :superlocriano7 [3 6 1 4 5 2]

       :harmonic-major [5 6 3 2 4 1]
       :dorianb5 [4 5 2 3 1 6]
       :phrygianb4 [3 1 4 2 5 6]
       :lydianb3 [2 3 6 1 4 5]
       :mixolydianb2 [1 6 2 3 5 4]
       :lydian++2 [1 4 6 2 3 5]
       :locriano7 [6 4 1 2 3 5]

       ;; approximative ...
       :double-harmonic [1 5 6 2 3 4]
       :lydian+2+6 [3 1 5 6 2 4]
       :ultraphrygian [3 6 1 4 5 2]
       :hungarian [2 3 5 6 4 1]
       :oriental [1 2 5 4 6 3]
       :ionian++2 [1 3 4 6 2 5]
       :ultralocrian [3 6 2 1 4 5]}))

(def structures {:triad [0 2 4]
                 :sus2 [0 1 4]
                 :sus4 [0 3 4]
                 :sus6 [0 4 5]
                 :sus7 [0 4 6]
                 :tetrad [0 2 4 6]
                 :seventh [0 2 4 6]
                 :add2 [0 1 2 4]
                 :add4 [0 2 3 4]
                 :sixth [0 2 4 5]
                 :sus27 [0 1 4 6]
                 :sus47 [0 3 4 6]
                 :sus67 [0 4 5 6]})
