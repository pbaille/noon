(ns noon.constants
  (:require [noon.utils.misc :as u]
            [clojure.math.combinatorics :as comb]))

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
       :ultralocrian [3 6 2 1 4 5]})

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

        (mode? x) x))

    (defn struct->mode-keyword [s]
      (some->> (seq modes)
               (filter (fn [[k s']] (= s s')))
               first
               key)))

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
















(comment :modes-categorisation-xp

         (require '[clojure.math.combinatorics :as comb])

         (letfn [(struct-map [modes]
                   (->> modes
                        (reduce (fn [ret [k s]]
                                  (reduce (fn [ret struct] (update ret struct (fnil conj []) k))
                                          ret (mapcat (partial comb/combinations s) (range 1 7))))
                                {})))]
           (def lvl->struct->modes
             [(struct-map major-modes)
              (struct-map (merge major-modes melodic-minor-modes))
              (struct-map (merge major-modes melodic-minor-modes harmonic-minor-modes))]))

         #_(lvl->struct->modes 0)

         (defn shortest-non-ambiguous-structs [mode lvl]
           (->> (get lvl->struct->modes lvl)
                (filter (fn [[s ms]] (= ms [mode])))
                (map key)
                (group-by count)
                (seq)
                (sort-by key)
                first
                val))

         #_(shortest-non-ambiguous-structs :lydian 1)

         (defn sort-struct-by-degree-priority [s lvl]
           (if (= 1 (count s))
             (vec s)
             (let [substructs (->> (sort-by (fn [d] (count ((lvl->struct->modes lvl) (list d)))) s)
                                   (map (fn [d] (sort (seq (disj (set s) d)))))
                                   (reverse))
                   less-ambiguous-substructs (->> (map (partial find (lvl->struct->modes lvl)) substructs)
                                                  (group-by (fn [[_ ms]] (count ms)))
                                                  (seq)
                                                  (sort-by key)
                                                  (first)
                                                  (val))]
               (let [substruct (key (first less-ambiguous-substructs))]
                 (conj (sort-struct-by-degree-priority substruct lvl)
                       (first (remove (set substruct) s)))))))

         #_(sort-struct-by-degree-priority (modes :lydian) 0)

         (def lvl2-modes (merge major-modes melodic-minor-modes harmonic-minor-modes))

         (def lvl2-degree-priority
           (map (fn [[m struct]] [m (sort-struct-by-degree-priority struct 2)])
                lvl2-modes))

         (def from-darkest
           (sort-by (fn [[m s]] (reduce + s))
                    lvl2-modes)))
