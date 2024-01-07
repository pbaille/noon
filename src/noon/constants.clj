(ns noon.constants
  (:require [noon.utils.misc :as u]))

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

    (defn chromatic-value->pitch [x]
      (let [oct (quot x 12)
            rem (rem x 12)
            matches (filter (fn [[_ v]] (= rem (:c v))) pitch-classes)
            sorted (sort-by (fn [[k _]]
                              [(count (name k))
                               (u/abs (alt-sym->alt-val (subs (name k) 1)))])
                            matches)]
        (if (seq matches)
          (-> (val (first sorted))
              (update :c + (* 12 oct))
              (update :d + (* 7 oct))))))

    (defn pitches_getter [table]
      (fn [x] (cond
                (number? x) (chromatic-value->pitch x)
                (map? x) (or ((set (vals table)) x) (recur (:c x)))
                (symbol? x) (get table x)
                (or (string? x) (keyword? x)) (get table (symbol (name x))))))

    (def get-pitch (pitches_getter pitches))
    (def get-pitch-class (pitches_getter pitch-classes))

    (defn pitch->pitch-class [p]
      (-> p
          (update :c rem 12)
          (update :d rem 7)))

    (defn pitch-class->pitches [pc]
      (->> (map (fn [oct]
                  (-> pc
                      (update :c + (* 12 oct))
                      (update :d + (* 7 oct))))
                (range 13))
           (filter (fn [{c :c}]
                     (<= 0 c 127)))))

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
              (scale-modes harmonic-major-scale-vals)))

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

    (defn mode? [x]
      (and (vector? x)
           (every? int? x)
           (= x (sort x))))

    (defn get-mode [x]
      (cond
        (keyword? x)
        (or (get modes x)
            (get modes (get mode-aliases x)))

        (or (symbol? x) (string? x))
        (get-mode (keyword (name x)))

        (mode? x) x)))

(def structs {:triad [0 2 4]
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

(defn struct-inversions [scale struct]
  (mapv
   (fn [idx]
     (let [head (get struct idx)
           based (map #(- % head) struct)]
       (vec (sort (map #(if (> 0 %) (+ (count scale) %) %) based)))))
   (range (count struct))))

(defn get-struct
  ([x]
   (cond (keyword? x) (get structs x)
         (or (symbol? x) (string? x)) (get-struct (keyword (name x)))
         (mode? x) x))
  ([x scale]
   (if-let [s (get-struct x)]
     (if (< (last s) (count scale))
       s
       (u/throw* "struct: " s "do not fit in scale: " scale)))))

(defn d->c [v]
  (nth (reductions + 0 (cycle major-scale-steps))
       v))

(assert (= 16 (d->c 9)))

(defn alt-sym [d c]
  (let [drem (second (u/divmod 7 d))
        amap (deg-alt-type drem)]
    (amap (- c (d->c d)))))
