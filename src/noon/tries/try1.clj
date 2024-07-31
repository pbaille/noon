(ns noon.tries.try1
  (:use noon.score)
  (:require [noon.lib.melody :as m]
            [noon.lib.harmony :as h]
            [noon.lib.rythmn :as r]
            [noon.harmony :as nh]
            [noon.utils.multi-val :as mv]
            [noon.utils.misc :as u]
            [noon.utils.pseudo-random :as pr]
            [noon.constants :as nc]
            #_[noon.midi :as midi]
            [noon.vst.vsl :as vsl :refer [vsl]]
            [clojure.math.combinatorics :as comb]))

(comment :barry-harris

         (def barry-harris (scale [0 2 4 5 7 8 9 11]))

         (play barry-harris
               (tup d0 d3 d4 d7)
               (tup d0 d2)
               (rep 4 d1))

         (let [chord-tones [d0 d2 d4 d7]]
           (play barry-harris
                 (lin d0 d3)
                 (rep 8 (one-of d1- d1))
                 (each [(chans [(patch :pad-1-new-age) o1- vel3 (par* chord-tones)]
                            [(patch :ocarina) vel4 (shuftup* chord-tones) (each (maybe (tup (one-of d1 d1-) d0)))]
                            [(patch :vibraphone) vel5 o1 (ntup 6 [(one-of* chord-tones) (maybe o1) (maybe (tup d1- d0))])])
                     (maybe rev)])))

         (def barry-harris2 [barry-harris (structure [0 2 4 7])])

         (play barry-harris2
               (lin I VI VII IV)
               (h/align-contexts :d)
               (each (chans [(patch :brass) (par s0 s1 s2 s3)]
                         [(patch :acoustic-bass) o1- t-round]
                         [(patch :ethnic) o1 (shuftup s0 s1 s2 s3 s4 s5 s6)]))
               (rep 2 s1)
               (append (transpose c3)))

         (play barry-harris2
               (lin IV I)
               (h/align-contexts :d)
               (each (par s0 s1 s2 s3))
               (rep 4 (transpose c3))
               h/voice-led))

(comment :minor-progression
         (play (lin [I melodic-minor] [V phrygian3] [V phrygian3] [I melodic-minor]
                    [I phrygian3] [IV dorian] [II locrian] [IIb lydianb7])
               (dup 2)
               (lin {:section :a}
                    [{:section :b} (transpose c6)])
               (h/align-contexts :d)
               (parts {:section :a} (each (chans [(patch :vibraphone) (shuftup s0 s1 s2 s3 s4 s5)]
                                              [(patch :flute) o1 (shuftup s0 s1 s2 s3 s4 s5)]
                                              [(patch :acoustic-bass) o1- t-round]))
                      {:section :b} (each (chans [(patch :choir-aahs) vel4 (par s0 s1 s2)]
                                              [(patch :ocarina) vel4 s2- (shuftup s0 s2 s4)]
                                              [(patch :music-box) vel6 o1 (shuftup s0 s1 s2 s3 s4 s5 s6 s7 s8)]
                                              [(patch :acoustic-bass) o1- t-round])))
               (dup 2)))

(comment :one-five
         (play dur3
               (lin [I (scale :melm) (structure :tetrad)]
                    [V (scale :alt) (structure :sus47)])
               (append s1-)
               (append [(transpose c4-)
                        (parts (scale :melm) (scale :lydian)
                               (scale :alt) [(scale :mixolydianb2) (structure [1 5 9 10])])])
               (dup 2)
               (h/align-contexts :s)
               (let [below (one-of d1- s1-)
                     above (one-of d1 s1)
                     contours [[0 -1 1 0]
                               [0 1 -1 0]
                               [-1 0 1 0]
                               [1 0 -1 0]
                               [1 0 -1 0]
                               [-1 0 1 0]]
                     passings (mapv (partial mapv {0 _ -1 below 1 above}) contours)
                     rand-passing (one-of* (map tup* passings))
                     below-step (one-of d1- d3- d4-)
                     above-step (one-of d1 d3 d4)
                     rand-line (rup 4 (one-of below-step above-step))
                     rand-vel (fn [min max] {:velocity (fn [_] (+ min (rand-int (- max min))))})]
                 (each (chans [(patch :choir-aahs) vel4 (par s0 s1 s2 s3)
                            (h/drop 1)]
                           [(patch :acoustic-bass) t-round o1-]
                           [(shuftup s0 s1 s2 s3)
                            (each (one-of rand-passing rand-line))
                            (chans [(patch :vibraphone) (each (rand-vel 40 70)) (each (maybe vel0))]
                                   [(patch :flute)
                                    (each (rand-vel 60 80))
                                    o1
                                    (each (maybe vel0 [(chan inc) (patch :glockenspiel) vel4]))])])))))

(comment :symetric-modes
         (def symetric-modes {:half-whole (scale [0 1 3 4 6 7 9 10])
                              :whole-half (scale [0 2 3 5 6 8 9 11])
                              :whole (scale [0 2 4 6 8 10])
                              :augm-half (scale [0 3 4 7 8 11])
                              :half-augm (scale [0 1 4 5 8 9])
                              :messian3 (scale [0 2 3 4 6 7 8 10 11])
                              :messian4 (scale [0 1 2 5 6 7 8 11])
                              :messian5 (scale [0 1 5 6 7 11])
                              :messian6 (scale [0 2 4 5 6 8 10 11])
                              :messian7 (scale [0 1 2 3 5 6 7 8 9 11])})

         (play (symetric-modes :augm-half)
               (:two {:one (rup 8 (one-of d1 d1- d2 d2- d3 d3-))
                      :two (shuftup d1 d2 d3 d4 d5 d6 d7)})

               (patch :electric-piano-1)
               (rep 32 (one-of (each d3)
                               (each d3-)
                               (m/rotation 1/2)
                               (m/permutation :rand {:grade 2})
                               (m/contour :similar {:delta 0 :layer :d}))))

         (defn rand-structure [size]
           (ef_ (let [degree-count (-> _ :pitch :scale count)
                      degrees (first (mv/consume size (mv/mix* (range degree-count))))]
                  (update-score #{_} (structure (vec (sort degrees)))))))

         (def rand-degree
           (ef_ (let [scale-size (-> _ :pitch :scale count)
                      deg (rand-nth (range 1 scale-size))]
                  (update-score #{_} (degree (rand-nth [(- deg) deg]))))))

         (defn rand-tup [size]
           (ef_ (let [degree-count (-> _ :pitch :scale count)
                      degrees (first (mv/consume size (mv/mix* (range degree-count))))]
                  (update-score #{_} (tup* (mapv d-step degrees))))))

         (play (symetric-modes :half-whole)
               (rand-structure 3)
               (rep 3 rand-degree)
               (each (chans [vel4 h/simple-chord]
                         [(patch :music-box) o1 (rand-tup 7) (each (one-of vel0 vel4 vel6 vel7))]))
               (append [rev s2])
               (append (transpose c5))
               (append (between 0 1/3))))

(comment :sparkling-waves
         (play dur:4
               vel4
               (scale :lydian)
               (patch :music-box)
               (par s0 s2 s4)
               (rep 3 (each [{:mark (rand)} s1 {:velocity (div 1.1) :duration (mul 1.3)} (shuftup s2- s0 s2)])
                    :skip-first)
               (lin I [rev III] [o1- V] [rev o1- VII])
               (append [rev (transpose c3)])))

(comment :arvo-part
         (def connect-repetitions
           (sf_ (let [[e1 & todo] (sort-by :position _)]
                  (loop [[last-note & prev-notes :as ret] (list e1)
                         todo todo]
                    (if-let [[e & todo] (seq todo)]
                      (if (and (= (pitch-value e) (pitch-value last-note))
                               (= (dissoc e :position :duration :pitch)
                                  (dissoc last-note :position :duration :pitch)))
                        (recur (cons (update last-note :duration + (:duration e))
                                     prev-notes)
                               todo)
                        (recur (cons e ret) todo))
                      (set ret))))))

         (let [m-line (fn [size]
                        (rand-nth (vals {:up-to [(rep size d1-) rev]
                                         :up-from (rep size d1)
                                         :down-to [(rep size d1) rev]
                                         :down-from (rep size d1-)})))
               humanize (one-of vel4 vel5 vel6)
               base (shuffle (map vector
                                  [s0 s1 s2 (one-of s0 s1 s2)]
                                  (map m-line (shuffle (rand-nth (u/sums 12 4 [2 3 4 5]))))))]
           (play lydianb7
                 (lin* base)
                 (each (chans [(patch :piccolo) vel6 o1]
                           [(patch :flute) vel3 o1 d5-]
                           [(patch :accordion) vel4 d0]
                           [(patch :choir-aahs) s-floor humanize]
                           [(patch :choir-aahs) s-floor o1 s1 humanize]
                           [(patch :acoustic-bass) C-2 t-floor]))

                 ($by :channel connect-repetitions)
                 (append [rev (transpose c3-)])
                 (append dorian)
                 (dup 2))))

(comment :violin-fast-arpegio
         (play (dur 3/2)
               dorian
               (patch :violin)
               (lin I IV V I)
               (h/align-contexts :s)
               (each (ntup 2 (tup s0 s2 s4 s4 s2 s0)))
               (each (! (vel (mul (+ 0.9 (* (rand) 0.2))))))
               (append s1-)))

(comment :elements
         (let [r {:a (r/gen-tup 8 5)
                  :b (r/gen-tup 8 3)
                  :c (r/gen-tup 12 5)}
               m {:a (m/gen-tup :size 8)}
               h {:a [(scale :melm) (structure :tetrad) (lin I [IV dorian] [V (scale :alt) (structure :sus47)] I)]}]
           (play dur2
                 (h :a)
                 (h/align-contexts :d)
                 (each (chans [(patch :taiko-drum) o1-]
                           [(patch :woodblock) vel3 C2 (r :a) (maybe (m/rotation :rand))]
                           [(patch :acoustic-bas) o1- t-round]
                           [(patch :vibraphone) vel3 closed-chord]
                           [(patch :flute) (m :a) (shuftup d0 d3 d6)])))))

(comment :inner-urge

         (stop)

         (defn last-n-positions
           "Limit the score to the n latest positions found."
           [n]
           (sf_ (let [_ (->> (group-by :position _)
                             seq (sort-by key)
                             reverse (take n)
                             (map second) (reduce into #{}))]
                  (update-score _ (start-from (score-origin _))))))

         (let [n-bars 24
               choir [(patch :choir-aahs) vel5 (par> d3 d3 d3)]
               bass [(patch :acoustic-bass) C-2 t-round]
               lead-line (any-that (within-pitch-bounds? :C0 :C3)
                                   (rep 2 d3 :skip-first)
                                   (rep 2 d3- :skip-first)
                                   d4 d4-
                                   d1 d1-
                                   (rep 3 d2 :skip-first)
                                   (rep 3 d2- :skip-first))]
           (play (h/harmonic-zip
                  [(tup (lin (nlin 4 [(root :F#) locrian2])
                             (nlin 4 [(root :F) lydian])
                             (nlin 4 [(root :Eb) lydian])
                             (nlin 4 [(root :Db) lydian]))
                        [lydian
                         (lin* (map root [:E :Db :D :B :C :A :Bb :G]))])
                   (h/align-contexts :s)
                   (dupt 4)]
                  (tup (chans choir
                              bass
                              [(patch :music-box)
                               vel5 C1
                               (m/simple-tupline (* n-bars 10) lead-line)])
                       (chans choir
                              bass
                              [(patch :ocarina)
                               vel4 C1
                               (m/simple-tupline (* n-bars 24) lead-line)])
                       (chans choir
                              bass
                              [(patch :sawtooth)
                               (dur (/ 1 n-bars))
                               vel4 C1
                               (tup d0 d3 d6)
                               (tup d0 d4 d8)
                               (m/line (one-of (last-n-positions 10) (last-n-positions 7))
                                       (any-that (within-pitch-bounds? :C0 :C3)
                                                 (m/permutation {:grade 3})
                                                 #_(one-of (m/contour :rotation {:layer :d})
                                                           (m/contour :mirror {:layer :d})
                                                           (m/contour :similar {:delta 0 :layer :d}))
                                                 (one-of d1 d1-)
                                                 (one-of d2 d2-))
                                       (sf_ (> (score-duration _) 1))
                                       (trim 0 1))
                               (vel-humanize 5 [40 80])])
                       (chans [choir
                               (ntup (/ n-bars 2) same)
                               ($by :position [(! (one-of (r/gen-tup 8 3 :euclidean)
                                                          (r/gen-tup 8 3 :durations [2 3 4 5])))
                                               (sf_ (let [xs (-> (group-by :position _) seq sort vals)]
                                                      (reduce into #{} (map update-score xs (pr/shuffle [d0 d1 d1-])))))])]
                              bass)))
                 (adjust 180))))

(comment :grid
         (stop)
         (play dur3
               (nlin> 48 (one-of d1 d1-))
               (each (chans [(patch :aahs) vel5 (par s0 s1 s2 s3)]
                         [(patch :ocarina) (shuftup s0 s2 s4 s6) (shuftup d0 d3 d6) (tup _ rev)]
                         [(patch :acoustic-bass) t2-]))
               (h/grid dur3 tetrad
                       (lin [I lydian (structure [2 3 5 6])]
                            [IIb dorian (structure [1 2 3 6])]
                            [V mixolydian (structure [2 3 5 6])]
                            [Vb melodic-minor (structure [1 2 5 6])])
                       (rep 6 (transpose c2-))
                       (dup 2)
                       (h/align-contexts :d :static)))

         (play (ntup> 24 (one-of d1 d1-))
               (each (chans [(patch :aahs) vel5 (par s0 s1 s2 s3)]
                         [(patch :ocarina)
                          (one-of (mixtup s0 s2 s4 s6)
                                  (mixtup s0 s2 s4 s6))
                          (one-of (mixtup d0 d3 d6)
                                  (mixtup d0 d3 d6))
                          (vel-humanize 10 [40 80])
                          (tup _ rev)]
                         [(patch :acoustic-bass) t2-]))
               (h/grid tetrad
                       (tup [I lydian]
                            [IIb dorian]
                            [V mixolydian]
                            [Vb melodic-minor])
                       (each (h/modal-structure 4))
                       (rup 4 (transpose c2-))
                       (dupt 2)
                       (h/align-contexts :d :static))
               (adjust 60))

         (play (chans [(patch :aahs) vel5 (par s0 s1 s2 s3)]
                      [(patch :acoustic-bass) t2-])
               (h/grid (lin [I lydian (structure [2 3 5 6])]
                            [IIb dorian (structure [1 2 3 6])]
                            [V mixolydian (structure [2 3 5 6])]
                            [Vb melodic-minor (structure [1 2 5 6])])
                       (rep 2 (transpose c2-))
                       (dup 2)
                       (h/align-contexts :d :static)
                       (adjust 1))
               (parts (patch :acoustic-bass)
                      (each (tup (maybe o1) (one-of d4 d3-))))
               (adjust 32))

         (play (chans [(patch :aahs)
                       vel6
                       (rup 24 (any-that (within-pitch-bounds? :G-1 :G1)
                                         s2 s2- s3 s3-))
                       (each (par s0 s1 s2 s3))]
                      [(patch :acoustic-bass) t2-])
               (h/grid tetrad
                       (lin [I lydian (structure [2 3 5 6])]
                            [IIb dorian (structure [1 2 3 6])]
                            [V mixolydian (structure [2 3 5 6])]
                            [Vb melodic-minor (structure [1 2 5 6])])
                       (rep 2 (transpose c2-))
                       (dup 2)
                       (h/align-contexts :d :static)
                       (adjust 1))
               (parts (patch :acoustic-bass)
                      (each (tup (maybe o1) (one-of d4 d3-))))
               (adjust 32)))

(comment :zip-rythmn

         (play lydianb7
               (h/modal-structure 5)
               (chans
                [(patch :vibraphone)
                 (shuflin s0 s1 s2 s3 s4)
                 (nlin 4 (one-of s1 s2 s1- s2-))
                 (sf_ (let [rythmn (mk (nlin 2 (! (r/gen-tup 12 5 :shifted))) (append rev))]
                        (set (map (fn [r n]
                                    (merge n (select-keys r [:position :duration])))
                                  (sort-by :position rythmn)
                                  (sort-by :position _)))))]
                [(patch :woodblock) (r/gen-tup 12 5 :euclidean) (dup 4)]
                [(patch :tinkle-bell) (dup 4)]
                [(patch :metallic) (shuflin s0 s1 s2 s3) (each (par s0 s1 s2))]
                [(patch :acoustic-bass) t2- (dup 4)])
               (adjust 8)
               (append [(transpose c3-) s1 rev] _)))

(comment :target-notes
         "Building good rythmic melodies is not easy."
         "Here, I will try to start from target notes and fill the holes between them."

         (play eolian
               (lin s0 s2 s1 s0))

         "How to fill betwwen the notes of this simple line"

         (def fill-diatonically
           (sf_ (let [sorted (sort-by :position _)
                      couples (partition 2 1 sorted)]
                  (-> (reduce (fn [ret [a b]]
                                (let [va (pitch-value a)
                                      vb (pitch-value b)
                                      direction (if (> va vb) :down :up)
                                      cnt (loop [cnt 0 current (:pitch a)]
                                            (case direction
                                              :up (if (>= (nh/hc->chromatic-value current) vb)
                                                    cnt
                                                    (recur (inc cnt) (nh/upd current (nh/d-step 1))))
                                              :down (if (<= (nh/hc->chromatic-value current) vb)
                                                      cnt
                                                      (recur (inc cnt) (nh/upd current (nh/d-step -1))))))]
                                  (concat-score ret
                                                (update-score #{(assoc a :position 0)}
                                                     (rup cnt (case direction :up d1 :down d1-))))))
                              #{}
                              couples)
                      (conj (last sorted))))))

         (play eolian
               (lin s0 s2 s1 s0)
               fill-diatonically)

         "Let's generalise to other layers"

         (defn fill-line [layer]
           (sf_ (let [sorted (sort-by :position _)
                      couples (partition 2 1 sorted)]
                  (-> (reduce (fn [ret [a b]]
                                (let [va (pitch-value a)
                                      vb (pitch-value b)
                                      direction (if (> va vb) :down :up)
                                      [check increment] (case direction :up [>= 1] :down [<= -1])
                                      cnt (loop [cnt 0 current (:pitch a)]
                                            (if (check (nh/hc->chromatic-value current) vb)
                                              cnt
                                              (recur (inc cnt) (nh/upd current (nh/layer-step layer increment)))))]
                                  (concat-score ret
                                                (update-score #{(assoc a :position 0)}
                                                     (rup cnt (ef_ (update _ :pitch (nh/layer-step layer increment))))))))
                              #{}
                              couples)
                      (conj (last sorted))))))

         (play eolian
               (lin s0 s2 s1 s0)
               (fill-line :c))

         (play dur:2
               harmonic-minor
               tetrad
               (patch :orchestral-harp)
               (lin s0 s2 s2- s4 s4- s2 s2- s5-)
               (lin _ [(transpose c6) s2 rev])
               (lin _ s2 s2-)
               (fill-line :s))

         "Next step will be to have control over the number of notes between targets"

         (defn target [layer size _direction duration]
           (ef_ (into #{_} (map (fn [i]
                                  (-> (update _ :pitch (nh/layer-step layer (inc i)))
                                      (update :position - (* (inc i) duration))
                                      (assoc :duration duration)))
                                (range size)))))

         (play (lin _ [s2 (target :c 3 :up 1/4)]))

         "The problem here is that the precedent note overlaps the targeting notes"

         (defn connect [& sizes]
           (sf_ (let [sorted (sort-by :position _)]
                  (reduce (fn [s [n1 n2]]
                            (let [hcs (loop [sizes sizes]
                                        (if-let [[s & sizes] (seq sizes)]
                                          (or (nh/simplest-connection s (:pitch n1) (:pitch n2))
                                              (recur sizes))))
                                  duration (/ (:duration n1) (dec (count hcs)))]

                              (into s (map-indexed (fn [idx pitch]
                                                     (assoc n1
                                                            :pitch pitch
                                                            :position (+ (* idx duration) (:position n1))
                                                            :duration duration))
                                                   (butlast hcs)))))
                          #{(last sorted)} (partition 2 1 sorted)))))

         (play
          harmonic-minor
          (lin I [VI lydianb7] V IV [II phrygian3] [V eolian] [IIb lydian])
          (h/align-contexts :s)
          ($lin [(lin s0 s2 s2- s4) (maybe [rev s2])])
          (lin _ s1 s1- _)
          (chans [(patch :tango) (connect 5 3 2 1 0)]
                 [(patch :ocarina) vel6 s2 (connect 2 1 0)]
                 [(patch :acoustic-bass) o1- s2- (connect 1 0)]))

         (stop)

         "The `connect` function is now available in `noon.lib.melody`"

         (play
          harmonic-minor
          (lin I [VI lydianb7] V IV [II phrygian3] [V eolian] [IIb lydian])
          (h/align-contexts :s)
          ($lin [(lin s0 s2 s2- s4) (maybe [rev s2])])
          (lin _ s1 s1- _)
          (chans [(patch :tango) (m/connect 5 3 2 1 0)]
                 [(patch :ocarina) vel6 s2 (m/connect 2 1 0)]
                 [(patch :acoustic-bass) o1- s2- (m/connect 1 0)])))

(comment :infinite-climb-illusion

         (play dur6 dur2
               (patch :ocarina)
               (rup 36 c1)
               (sf_ (set (map-indexed (fn [i n] (let [vel (* 60 2 (/ (inc i) (count _)))
                                                      vel (if (> vel 60) (- 60 (- vel 60)) vel)]
                                                  (assoc n :velocity vel)))
                                      (sort-by :position _))))
               (par _
                    (m/rotation 1/3)
                    (m/rotation 2/3))
               (dup 4)))

(comment :garzone-triads

         "George Garzone has this interesting way of building lines"
         "https://youtu.be/dTIwWFa2Rnw?si=lhQUZKaa3SaL6O9R&t=2594"
         "The idea is to mix all triads that shares at least one note with the chord you are playing on"
         "Let's start with an hardcoded example on Cmajor"
         (let [maj [ionian triad]
               min [eolian triad]
               triads [[(root :C) maj]
                       [(root :E) maj]
                       [(root :C) min]
                       [(root :G) maj]
                       [(root :B) maj]
                       [(root :F) maj]
                       [(root :D) maj]
                       [(root :Eb) maj]

                       [(root :E) min]
                       [(root :G) min]
                       [(root :F) min]
                       [(root :B) min]
                       [(root :D) min]
                       [(root :Ab) maj]
                       [(root :A) min]
                       [(root :C#) min]]]
           (play
            (chans [(patch :aahs) (par s0 s1 s2)]
                   [(ntup> (* (count triads) 3)
                           (any-that (within-pitch-bounds? :C-1 :C1)
                                     s1 s1-))
                    (h/grid [(tup* (shuffle triads))
                             (h/align-contexts :s)])])
            (adjust 8)))

         "We got a taste of it but it is not really satisfying for"
         "Garzone is more precise regarding to how subsequent triads could be connected."
         "the last note of the triad you play can go either 1 semiton up or down"
         "from this note pick another triad but changing the inversion used"

         [:aborted '(let [main-triad [(root :C) ionian triad]
                          main-pitch-classes (set (map pitch-class-value (mk main-triad (par s0 s1 s2))))
                          all-triads (for [root' [:C :Db :D :Eb :E :F :Gb :G :Ab :A :Bb :B]
                                           kind [ionian eolian lydian+ superlocrian]]
                                       [(root root') kind])

                          triad->pitch-class-values (reduce (fn [ret t]
                                                              (assoc ret t (mapv pitch-class-value (mk t (par s0 s1 s2)))))
                                                            {} all-triads)

                          all-transitions (reduce (fn [ret [t vs]]
                                                    (assoc ret t (reduce (fn [ret [t' vs']]
                                                                           (update ret (- 6 (count (into #{} (concat vs vs'))) conj t')))
                                                                         {} triad->pitch-class-values)))
                                                  {} triad->pitch-class-values)

                          available-triads (filter (fn [u]
                                                     (some main-pitch-classes
                                                           (map pitch-class-value (mk u (par s0 s1 s2)))))
                                                   all-triads)

                          transitions (reduce (fn [ret triad]
                                                (assoc ret triad (filter (fn [t]) available-triads)))
                                              {} available-triads)
                          triad-line (loop [ret [] current main-triad triads available-triads]
                                       (if (seq triads)
                                         (let [])))]
                      (play (lin* (shuffle available-triads))
                            (h/align-contexts :s)
                            (each (tup s0 s1 s2))))]

         "We will no longer contrained available triads to ones that contains one note of the main triad."
         "We will only pick a random note of the current triad, move it 1 semitone up or down and pick another that contain this note."

         (let [all-triads (for [root' [:C :Db :D :Eb :E :F :Gb :G :Ab :A :Bb :B]
                                kind [ionian eolian lydian+ superlocrian]]
                            [(root root') kind])

               triad->pitch-class-values (reduce (fn [ret t]
                                                   (assoc ret t (mapv pitch-class-value (mk t (par s0 s1 s2)))))
                                                 {} all-triads)

               _all-transitions (reduce (fn [ret [t vs]]
                                          (assoc ret t (reduce (fn [ret [t' vs']]
                                                                 (update ret (- 6 (count (set (concat vs vs')))) conj t'))
                                                               {1 [] 2 [] 3 []} triad->pitch-class-values)))
                                        {} triad->pitch-class-values)

               pitch-class-value->triads (reduce (fn [ret [t vs]] (reduce #(update %1 %2 conj t) ret vs))
                                                 {} triad->pitch-class-values)
               triads (loop [current 0 ret []]
                        (if (> (count ret) 16)
                          ret
                          (let [picked (rand-nth (pitch-class-value->triads current))
                                pitch-class-values (mapv pitch-class-value (mk picked (par s0 s1 s2)))
                                inv (rand-nth [0 1 2])]
                            (recur (-> (get pitch-class-values inv)
                                       ((rand-nth [inc dec]))
                                       (mod 12))
                                   (conj ret [picked inv])))))]
           (play
            (lin* (map (fn [[triad inv]] (mk dur:2
                                             triad
                                             (chans [(patch :acoustic-bass) o1- t-round]
                                                    [(patch :electric-piano-1)
                                                     ({0 (tup s1 s2 s0)
                                                       1 (tup s0 s2 s1)
                                                       2 (tup s0 s1 s2)} inv)
                                                     (par _
                                                          [o1 rev])])))
                       triads))
            (lin _ s1 s1- _))))

(comment "bach prelude Cm pattern"

         (play harmonic-minor
               ($lin (lin I IV I V))
               (h/align-contexts :s)
               (lin _ s1)
               (each (chans (tup s2 [s1 (lin _ d1- _)] s0 [s1 (lin _ d1- _)])
                         (tup s3- [s2- (lin _ d1 _)] s1- [s2- (lin _ d1 _)])))
               (lin _ [(transpose c3) rev])
               (dup 2))

         (play harmonic-minor
               (eachlin (lin I IV I V))
               (h/align-contexts :s)
               (lin _ s1)
               (let [pat1 (tup s2 [s1 (lin _ d1- _)] s0 [s1 (lin _ d1- _)])
                     pat2 [pat1 (m/contour :mirror {:layer :s})]]
                 (each (chans [o1 pat1]
                           [s1- pat2]))))

         (play harmonic-minor
               dur2
               (lin _ (transpose c3) _)
               ($lin (lin I IV I V))
               (h/align-contexts :s)
               (let [br (lin _ (one-of d1 d1-) _)
                     pat1 (one-of (tup s2 [s1 br] s0 [s1 br])
                                  (tup [s1 br] s2 [s1 br] s0)
                                  (tup s0 [s1 br] s2 [s1 br])
                                  (tup [s1 br] s0 [s1 br] s2))
                     pat2 (one-of (tup s3- [s2- br] s1- [s2- br])
                                  (tup s1- [s2- br] s3- [s2- br]))]
                 (each (chans [o1 (patch :ocarina) vel8 pat1]
                           [(patch :vibraphone) pat2])))
               (dup 2)))

(comment "elliot Smith chords"

         (play dur2
               (lin [VI seventh]
                    [IV add2]
                    [I]
                    [III seventh (inversion 2)]
                    [VI seventh]
                    [IV add2]
                    (tup I [III seventh phrygian3])
                    [IV])
               (h/align-contexts :d)
               (each (chans [(patch :acoustic-bass) o1- t-round]
                         h/simple-chord)))

         (play (chans [(patch :electric-piano-1) (tup (shuftup s0 s1 s2 s3) (shuftup s2 s3 s4 s5))]
                      [(patch :acoustic-bass) o1- t-round])
               (dupt 8)
               (h/grid
                [(tup [VI seventh]
                      [IV add2]
                      [I]
                      [III seventh (inversion 2)]
                      [VI seventh]
                      [IV add2]
                      (tup I [III seventh phrygian3])
                      [IV])
                 (h/align-contexts :d)])
               (adjust 8)
               (dup 2)))

(comment "bartok harmony axis"
         (let [L- (transpose c5)
               L+ (transpose c5-)
               R- (transpose c3)
               R+ (transpose c3-)
               M (transpose c6)]
           (play (rep 8 [(one-of L- L+) (maybe R- R+ M) (one-of ionian eolian)])
                 (h/align-contexts :d)
                 (chans [(patch :aahs) (each (par s0 s1 s2))]
                        [(patch :ocarina) o1 (each (shuftup s2- s1- s0 s1 s2 s3))]
                        [(patch :acoustic-bass) o1-
                         t-round
                         (maybe s1 s1-)])
                 (lin _ s1 s1- _)))

         (def connect-repetitions
           (sf_ (let [[e1 & todo] (sort-by :position _)]
                  (loop [[last-note & prev-notes :as ret] (list e1)
                         todo todo]
                    (if-let [[e & todo] (seq todo)]
                      (if (and (= (pitch-value e) (pitch-value last-note))
                               (= (dissoc e :position :duration :pitch)
                                  (dissoc last-note :position :duration :pitch)))
                        (recur (cons (update last-note :duration + (:duration e))
                                     prev-notes)
                               todo)
                        (recur (cons e ret) todo))
                      (set ret))))))

         (let [L- (transpose c5)
               L+ (transpose c5-)
               R- (transpose c3)
               R+ (transpose c3-)
               M (transpose c6)
               tup1 (tup* (shuffle [s2- s1- s0 s1 s2 s3]))
               tup2 (tup* (shuffle [s2- s1- s0 s1 s2 s3]))]
           (play (rep 8 [(one-of L- L+) (maybe R- R+ M) (one-of ionian eolian)
                         (maybe dur2 dur:2)])
                 (h/align-contexts :d)
                 (chans [(patch :aahs)
                         (each [add2 (par s0 s1 s2 s3)])
                         connect-repetitions]
                        [(patch :ocarina) o1 add2 (each [(one-of tup1 tup2) (maybe rev)])]
                        [(patch :acoustic-bass) o1-
                         t-round
                         (maybe s1 s1-)])
                 (lin _ s1 s1- _)))

         (let [L- (transpose c5)
               _L+ (transpose c5-)
               R- (transpose c3)
               R+ (transpose c3-)
               M (transpose c6)

               base [(pr/rand-nth [R- R+ M]) (pr/rand-nth [ionian eolian])]
               rand-color [(maybe R- R+ M) (one-of ionian eolian)]
               tup1 (tup* (pr/shuffle [s2- s1- s0 s1 s2 s3]))
               tup2 (tup* (pr/shuffle [s2- s1- s0 s1 s2 s3]))]
           (play base
                 (lin _ [L- rand-color] rand-color [L- rand-color] _)
                 (lin _ M rev)
                 (h/align-contexts :d)
                 (chans [(patch :aahs)
                         (each [add2 (par s0 s1 s2 s3)])
                         connect-repetitions]
                        [(patch :ocarina) o1 add2 (each [(one-of tup1 tup2) (maybe rev)])]
                        [(patch :acoustic-bass) o1-
                         t-round
                         (maybe s1 s1-)])
                 (lin _ s1 [rev s1-] _)))

         (stop)
         (let [initial [{:harmonic-coords [0 0]} melodic-minor sixth]
               up [{:harmonic-coords (fn [[x y]] [x (mod (inc y) 3)])} (transpose c5)]
               down [{:harmonic-coords (fn [[x y]] [x (mod (dec y) 3)])} (transpose c5-)]
               left [{:harmonic-coords (fn [[x y]] [(mod (dec x) 4) y])} (transpose c3)]
               right [{:harmonic-coords (fn [[x y]] [(mod (inc x) 4) y])} (transpose c3-)]]
           (play initial
                 (lin> _ up left down)
                 (lin _ up)
                 (lin _ [rev left])
                 (lin _ [right right])
                 (h/align-contexts :d)
                 (chans [(patch :aahs) (structure [1 2 5 6]) (each (par s0 s1 s2 s3))]
                        (let [tup1 (mixtup s2- s1- s0 s1 s2 s3)
                              tup2 (mixtup s2- s1- s0 s1 s2 s3)]
                          [(patch :ocarina) o1 add2 (each [(one-of tup1 tup2) (maybe rev)])])
                        [(patch :acoustic-bass) o1-
                         t-round
                         (maybe s1 s1- s2-)])
                 (lin _ s1 [up s1-] up)))

         (stop)
         (let [initial [lydian seventh]
               up (transpose c5)
               down (transpose c5-)
               left (transpose c3)
               right (transpose c3-)]
           (play ;; grid
            [initial
             (lin> _ up left down)
             (each (maybe (degree 2) (degree -2)))
             (lin _ up)
             (lin _ [rev left])
             (lin _ [right right])
             (h/align-contexts :d)]
                 ;; voices
            (chans [(patch :aahs) (each (par s0 s1 s2 s3))]
                   #_[(patch :aahs) t-round (each (par d0 d3 d6 d9)) #_h/voice-led]
                   (let [tup1 [(structure [2 3 4 6]) (mixtup s3- s2- s1- s0 s1 s2 s3 s4)]
                         tup2 (mixtup d3- d2- d1- d0 d1 d2 d3 d4)]
                     [(patch :ocarina) o1 (each [(one-of tup1 tup2) (maybe rev)])])
                   [(patch :acoustic-bass) o2-
                    t-round
                    (each (probs {_ 3
                               (one-of s1- s2) 3
                               (tup _ (one-of s1- s2)) 1
                               (tup (one-of s1- s2) _) 1}))])
                 ;; why not ?
            (lin _ s1 [up s1-] up)
            (options :bpm 40 :xml true))))

(comment "try pseudo random"
         (pr/with-rand 12
           (play (shuftup d0 d1 d2 d3 d4))))

(comment "chords"

         (stop)
         (let [rand-color (fn [] (let [k (rand-nth [:lydian+ :lydian :ionian :dorian :melodic-minor :mixolydian :phrygian6])]
                                   [(scale k)
                                    (h/modal-structure 4)]))]
           (play dur2
                 (lin* (map (comp transpose c-step) (shuffle (range 12))))
                 (each (! (rand-color)))
                 (h/align-contexts :d :static)
                 (chans [(patch :aahs) (each (par s0 s2 s3 s5)) #_h/voice-led]
                        [(patch :vibraphone) o1 (each (par s0 s2 s3) (shuftup s0 s3) (tup s0 s1 s1-))
                         ($by :position (probs {vel0 2
                                                (one-of vel3 vel5 vel7) 8
                                                [vel3 (ntup> 4 [s1 (vel+ 15)])] 1}))]
                        [(patch :acoustic-bass) o1- t-round])))

         (defn possible-modes
           "given a chromatic degree (int between 0 an 11)
            return possible modes"
           [cd modal-lvl least-priority]
           (let [modes (nc/lvl->mode->degree-priority modal-lvl)
                 candidates (filter (fn [[_ s]] (-> (take least-priority s)
                                                    (set) (contains? cd)))
                                    modes)]
             candidates))

         (possible-modes 3 2 3)

         (play (patch :aahs)
               dur4
               (shuflin c0 c1 c2 c3)
               (m/contour :similar {:delta 4 :layer :c})
               (par o1 [c6- (m/contour :mirror {:layer :c})])
               ($by :position (sfn score (let [modal-lvl 1
                                               chord-size 4
                                               [min-pitch-val max-pitch-val] (h/pitch-values score)
                                               interval (mod (- max-pitch-val min-pitch-val) 12)
                                               [mode-kw prio] (rand-nth (possible-modes interval modal-lvl (dec chord-size)))
                                               partial-scale (cons 0 (take (dec chord-size) prio))
                                               structure' (nc/partial-scale->structure mode-kw partial-scale)
                                               closed (mk (dissoc (first score) :pitch) (origin min-pitch-val) (scale mode-kw) (structure structure') (par* (map s-step (range chord-size))))
                                               drops (filter (fn [drop] (= max-pitch-val (last (h/pitch-values drop)))) (h/drops closed))]
                                           (rand-nth drops))))
               ($by :position (chans _
                                     [(patch :contrabass) vel3 min-pitch o1-]
                                     [max-pitch
                                      (patch :ocarina)
                                      (mixtup s0 s1- s2- s3- s4- s5-)
                                      (tup _ s2- s1)
                                      #_(each (probs {_ 4 (tup _ [vel4 (maybe s2- s3-)]) 1}))]))
               (lin _ [rev c3])
               (lin _ [rev c3-])
               (options :bpm 30 :xml true :preview true)))

(comment "canon"

         "First thing would be to come up with a simple melodic motiv."
         "It will be based on a triad, with some decorating tones."

         "The skeleton could be something like"
         (play (shuftup s0 s1 s2))

         "We can start in 3/4"
         "The next step will be to decorate it."
         "Previously we've discussed the connect function that can do something like this"

         (play (shuftup s0 s1 s2)
               (m/connect 1))

         "But it is not really what we want"
         (def decorate
           (sf_ (let [sorted (sort-by :position _)]
                  (reduce (fn [s [n1 n2]]
                            (into s (update-score #{n1 n2} (maybe (m/connect 1)))))
                          #{(last sorted)} (partition 2 1 sorted)))))

         (noon {:play true :pdf true}
                      (mk dur2
                          (lin (shuftup s0 s1 s2 s3)
                               [(one-of s1 s1-) (shuftup s0 s1 s2 s3)])
                          decorate
                          (lin _ (s-shift 1) (s-shift -1) _)
                          (lin _ (s-shift 2))
                          (chans [(patch :ocarina) o1 (s-shift -1)]
                                 [(sf_ (shift-score _ 2))]
                                 [(patch :acoustic-bass) o2- (s-shift 1) (sf_ (shift-score _ 5))])
                          (h/grid dur2
                                  harmonic-minor
                                  (lin I IV VII I [IV melodic-minor VII] IV [V harmonic-minor VII] VII)
                                  (dup 4)
                                  (h/align-contexts :s))))

         (stop)
         (play harmonic-minor (tup s1 s2) (m/connect 1))
         (play harmonic-minor o1 (tup s0 s1- s2 s1) (sf_ (let [sorted (sort-by :position _)]
                                                           (reduce (fn [s [n1 n2]]
                                                                     (into s (update-score #{n1 n2} (m/connect 1))))
                                                                   #{(last sorted)} (partition 2 1 sorted)))))

         (play (shuftup s0 s1 s2 s3) decorate (lin _ vel0 (m/contour :similar {:delta 1 :layer :s}))))

(comment "vsl iac bus 1"

         (noon {:sequencer :bus1
                :play true}
               (mk (rup 128 (any-that (within-pitch-bounds? :C1 :C3)
                                      s1 s2 s3 s1- s2- s3-))
                   (chans (each (probs {_ 2
                                     vel0 1
                                     (shuftup s1- s0 s1 s2) 1}))
                          (each s1- (probs {_ 2
                                         vel0 1
                                         (shuftup s1- s0 s1) 1}))
                          (each [s2- o1- (probs {_ 2
                                              (shuftup s0 s2) 1})]))
                   (h/grid harmonic-minor
                           (tup I IV VII I [IV melodic-minor VII] IV [V harmonic-minor VII] VII)
                           (dupt 4)
                           (h/align-contexts :s))
                   (adjust {:duration 64})))

         (stop)

         (vsl/noon {:play true
                    :write true
                    :pdf true}
                   (mk (par [(vsl :violin1 :detache) (lin s0 [(vsl/patch :legato) (tup s1 s2 s3)] [(vsl/patch :pizzicato) (par [(vsl/patch :snap-pizzicato) _]
                                                                                                                               [(vsl :double-bass :pizzicato) o2- (tup s2 s1)])])]
                            [(vsl :flute1 :portato) o1 s- (lin s0 [(vsl/patch :legato) (tup s1 s2 s3)])])
                       (lin s0 s2 s1-)
                       (dup 4)))

         (noon {:tracks {0 :bus1 1 :bus2}
                 :play true}
                (mk vel10
                    (vsl/instrument :solo-cello)
                    (vsl/patch :pizzicato)
                    o1-
                    (shuftup d0 d3 d6)
                    (shuftup d0 d3 d6)
                    (dup 8))))

(comment "melody"

         "Try to implement diverse melodic passing notes things."

         "Mono harmony passing notes"

         (p (rep 6 s1)
            (m/connect 1))

         (p o1
            (rep 6 s1-)
            (m/connect 1))

         (= (mk (tup d1- d0))
            (mk (tup d1- d0)
                (m/connect 1)))

         (defn connect-with [f]
           (m/$connect (fn [from to]
                         (update-score #{(assoc from :position 0)}
                              [(lin _ [(ef_ (assoc _ :pitch (:pitch to)))
                                       f])
                               (adjust from)]))))

         (p (lin s0 s2 s4)
            (connect-with d1))

         (p (lin s0 s2 s4)
            (lin s0 s1 s2)
            (connect-with (tup d1- d1)))

         (p [eolian dur:2]
            (lin s0 s2 s4)
            (lin s0 s1 s2)
            (connect-with (shuflin d1 c1-)))

         "With chord changes"

         (p harmonic-minor
            (lin I VII)
            (nlin> 3 (transpose c3))
            (h/align-contexts :s)
            (dup 2)
            (each (ntup> 6 s1))
            (connect-with d1))

         "With parts and vsl"

         (vsl/noon {:play true}
                   (mk
                    ;; grid
                    [harmonic-minor
                     (lin I VII)
                     (nlin> 3 (transpose c3))
                     (h/align-contexts :s)
                     (dup 4)]
                    ;; parts
                    (par
                     ;; flute melody
                     [(vsl :flute-1 :staccato)
                      o1 vel4
                      (each (shuftup s0 s2 s4))
                      (connect-with (one-of d1- d1))]
                     ;; bass
                     [(vsl :solo-double-bass :pizzicato)
                      o1- t-round]
                     ;; viola comping
                     [(vsl :chamber-violas :pizzicato)
                      vel5
                      (each (one-of (tup s1 (par s2 s3) vel0)
                                 (tup vel0 s1 (par s2 s3))))])))

         (stop)

         "Targetting other chord/key"

         (defn connect-with2 [f]
           (m/$connect (fn [from to]
                         (let [{:keys [scale structure origin]} (:pitch from)
                               target-pitch (:pitch to)]
                           (update-score #{(assoc from :position 0)}
                                [(lin _ [(ef_ (assoc _ :pitch target-pitch))
                                         (rescale scale)
                                         (restructure structure)
                                         (reorigin origin)
                                         f])
                                 (adjust from)])))))

         (p (lin d0 [IIb mixolydian])
            (connect-with d1-))
         (p (lin d0 [IIb mixolydian])
            (connect-with2 d-floor))
         (p (lin d0 d-floor))

         "This is difficult... to be continued")

(comment "polarity"

         "This morning I was playing modal melodies on the flute, and experimenting with different polarity cycles."

         [0 0 1 0]
         [0 1 1 0]

         "0 can represent tonic and 1 dominant, whatever it means depending on the harmonic context."

         "let's take the phrygian mode as an example."

         [0 0 1 0 1 0 0 1]

         (p phrygian
            (lin I I VII I VII I VII VII)

            (mixlin s0 s2)
            (each (chans [(patch :acoustic-bass) o2- (maybe t-round)]
                      [(patch :ocarina) s2 (shuftup s0 s2 s4)]))
            (lin _ [rev (transpose c3-)])
            (parts (chan 1) (connect-with (one-of (one-of d1 d1-)
                                                  (shuflin (one-of s1 s1-) (one-of d1 d1-))))
                   (chan 0) (each (probs {(tup (one-of s1 s1-) _) 1
                                       _ 4}))))

         "Let's start simple"

         (let [id identity
               rev (fn [x] (mapv {0 1 1 0} x))
               _dup (fn [x] (vec (concat x x)))
               cat (fn [& xs] (fn [x] (vec (mapcat (fn [f] (f x)) xs))))
               acc (fn [n f] (apply comp (repeat n f)))
               each (fn [f] (fn [x] (vec (mapcat (comp f vector) x))))
               _scan (fn [size step f] (fn [x] (vec (mapcat f (partition size step x)))))
               >> (fn [& xs] (fn [x] (reduce #(%2 %1) x xs)))
               upd (fn [x f] (f x))]
           (upd [1]
                (>> (acc 3 (cat id rev))
                    (each (cat id rev id))

                    ))))

(comment "degree moves"
         "It seems that the degree that is under the current one can serve as kind of a dominant."
         (p dorian
            (nlin> 8 s1)
            [(patch :ocarina) (connect-with (degree -1))])

         (noon {:play true
                :tracks {0 :chorium}
                :pdf true}
               (mk dorian
                   dur4 o1 (lin _ (nlin> 3 s1-))
                   [(patch :ocarina) (connect-with (degree 1))]
                   (each (tup s0 s2))
                   (connect-with (degree 1))))

         (let [pol+ {:polarity 0}
               pol- {:polarity 1}
               invert-pol (each {:polarity (fn [x] (case x 0 1 1 0))})]
           (p lydianb7
              dur2
              (lin pol+ pol-)
              (lin _ invert-pol)
              (tup _ invert-pol)
              (rep 4 (transpose c3-))
              (h/align-contexts :s)
              (dup 2)
              (parts pol+ _
                     pol- (each (one-of (degree -1) (degree 1))))
              (chans [(patch :ocarina) (each [(one-of s0 s1) (shuftup s0 s1 s2 s3)]) (connect-with (one-of d1 d1-))]
                     [(patch :acoustic-bass) o1- (each (one-of s0 s1- s2-))])))

         (let [pol+ {:polarity 0}
               pol- {:polarity 1}
               invert-pol (each {:polarity (fn [x] (case x 0 1 1 0))})]
           (p (chans [(patch :ocarina)
                      s2- (ntup> 7 s1)
                      (shuftup [_ (connect-with d1)]
                               [rev s1- (connect-with d1-)])
                      (dupt 16)]
                     [(patch :acoustic-bass) (dupt 64) o2- t-round (each (maybe s2- s2))])
              (h/grid [phrygian3
                       (tup pol+ pol-)
                       (tup _ invert-pol)
                       (tup _ invert-pol)
                       (rup 4 (transpose c3-))
                       (h/align-contexts :s)
                       (dupt 2)
                       (parts pol+ _
                              pol- (each (degree -1)))])
              (adjust {:duration 64})))

         (first @history*))

(comment "happy birthday"

         "not too happy birthday"
         (p harmonic-minor
            (lin I V VII I [IV melodic-minor VII] IV I VII)
            (h/align-contexts :s)))

(comment "simple melodic development"

         "I would like to list/sort the most basic transformations that can be applied to a simple melodic fragment."
         "In order to create some really simple pieces relying on one or two simple motives"

         (let [m1 (tup s0 s1 s2)]

           "transposition"

           (p m1
              (lin _ vel0
                   s1 vel0
                   s1- vel0))

           "reversion"

           (p m1
              (lin _ vel0
                   rev))

           "mirror"

           (p (lin s0 s2 s1 s2)
              (lin _ vel0 (m/contour :mirror)))

           "mixing"

           (p dur:2
              m1
              (lin _ s1)
              (lin _ [s1 rev])
              (lin _ (degree -1) rev [(degree 3) s1-]))

           (stop)

           "dodecaphonic"

           (let [serie (mapv c-step (pr/shuffle (range 12)))]
             (p (chans [(patch :ocarina)
                        (lin* serie)
                        (tup _ rev (m/contour :mirror) [(m/contour :mirror) rev])]
                       [(patch :choir-aahs) (lin* serie) o1-])
                (lin* serie)))))

(comment "simple counterpoint"

         (let [perms (comb/permutations [0 1 2 3])
               complementary-map (reduce (fn [acc p]
                                           (assoc acc p (filter (fn [p'] (every? (fn [[a b]] (not= (mod a 3) (mod b 3)))
                                                                                 (map vector p p')))
                                                                perms)))
                                         {} perms)

               [base complements] (rand-nth (seq complementary-map))
               voice1 (rand-nth complements)
               voice2 (map (fn [a b] (first (filter (complement (set (map #(mod % 3) [a b]))) [0 1 2]))) base voice1)]

           (p (patch :electric-piano-1)
              (chans (lin* (map s-step base))
                     [o1- (lin* (map s-step voice1))]
                     [o1 (lin* (map s-step voice2))])
              [eolian
               (lin _ (degree -1))
               (lin _ s1)
               (lin _ [(degree 3) s1-])
               (lin _ (transpose c3-))]
              ($by :channel (connect-with (probs {void 5 d1 1 d1- 1})))))

         "this complementary util is interesting, but the way I get the third voice is not pretty."
         "How about introducing another level ?"

         (defn complementarity-tree

           ([structure-size sequence-size]
            (let [elements (range structure-size)
                  q (quot sequence-size structure-size)
                  r (rem sequence-size structure-size)
                  base (apply concat (repeat q elements))
                  partials (filter (fn [s] (= r (count s))) (comb/subsets elements))
                  permutations (mapcat (fn [p] (comb/permutations (concat base p))) partials)]
              (complementarity-tree [] structure-size (set permutations))))

           ([at structure-size perms]
            (if-let [perms
                     (some-> (if (seq at)
                               (filter (fn [p']
                                         (every? (fn [xs]
                                                   (apply distinct?
                                                          (map #(mod % structure-size) xs)))
                                                 (apply map vector p' at)))
                                       perms)
                               perms)
                             seq
                             set)]
              (->> perms
                   (map (fn [child]
                          [child
                           (complementarity-tree
                            (conj at child)
                            structure-size
                            (disj perms child))]))
                   (into {})))))

         (defn leaves-paths
           ([m] (leaves-paths m []))
           ([x at]
            (if (and (map? x) (not-empty x))
              (mapcat (fn [[k v]] (leaves-paths v (conj at k))) x)
              [at])))

         (stop)
         (let [[v1 v2 v3] (->> (complementarity-tree 3 3)
                               (leaves-paths)
                               (filter #(= 3 (count %)))
                               (rand-nth))]
           (p [dur3
               eolian
               (lin _ (degree -1))
               (lin _ s1)
               (lin _ [(degree 3) s1-])
               (lin _ [s1 (transpose c3-)])]
              (patch :electric-piano-1)
              (each (! (let [[v1 v2 v3] (shuffle [v1 v2 v3])]
                      (chans (tup* (map s-step v1))
                             [o1- (tup* (map s-step v2))]
                             [o1 (tup* (map s-step v3))]))))
              ($by :channel (connect-with (probs {void 5 d1 1 d1- 1})))))

         "could this complementarity-tree be used to for rythmn ?"

         (let [[[r1 r2 r3] [l1 l2 l3]] (->> (complementarity-tree 3 3)
                                            (leaves-paths)
                                            (filter #(= 3 (count %)))
                                            (shuffle))
               f (fn [r l] (tup* (map (fn [r l]
                                        [(s-step l)
                                         (case r
                                           0 _
                                           1 (tup [dur2 _] (one-of d1 d1-) _)
                                           2 (one-of (tup _ d1 d1- _)
                                                     (tup _ d1- d1 _)))])
                                      r l)))]
           (vsl/noon {:play true}
                     (mk [dur3
                          eolian
                          (lin _ (degree -1))
                          (lin _ s1)
                          (lin _ [(degree 3) s1-])
                          (lin _ [s1 (transpose c3-)])]
                         (each (! (let [[a b c] (shuffle [(f r1 l1) (f r2 l2) (f r3 l3)])]
                                 (chans
                                  ;[(vsl :flute-1 :staccato) vel3 o1 (s-shift 1) c]
                                  [(vsl :solo-violin-1 :pizzicato) o1 b]
                                  [(vsl :solo-viola :pizzicato) c]
                                  [(vsl :solo-cello-1 :pizzicato) o1- a])))))))

         "We miss meaninful connections between triad degrees, here we only do ornementation."
         "This is also a bit too monotonic."

         (stop)
         (let [[arpegios ornamentations harmonic-sequences]
               (->> (complementarity-tree 3 3)
                    (leaves-paths)
                    (filter #(= 3 (count %)))
                    (shuffle))

               choices {:harmony {0 _
                                  1 [(degree 3) (s-shift -1)]
                                  2 [(degree 4) (s-shift -2)]}
                        :arpegio {0 s0 1 s1 2 s2}
                        :ornamentation {0 void
                                        1 d1
                                        2 d1-}
                        :instruments {0 [vel8 (vsl :chamber-violins-1 :legato) o1]
                                      1 [vel7 (vsl :chamber-violas :legato)]
                                      2 [vel6 (vsl :chamber-cellos :legato) o1-]}}

               degrees (mapcat (fn [s]
                                 (map (choices :harmony) s))
                               harmonic-sequences)
               lines (map (fn [offset]
                            [(get-in choices [:instruments offset])
                             (lin* (map (fn [d a c]
                                          [d (tup* (map (fn [step orn] [(get-in choices [:arpegio step]) {:connection orn}]) a c))])
                                        degrees
                                        (drop offset (cycle arpegios))
                                        (drop offset (cycle ornamentations))))])
                          (range 3))]
           (vsl/noon {:pdf true
                      :play true}
                     (mk dur8
                         harmonic-minor
                         (par* lines)
                         (lin _ (transpose c3-))
                         ($by :channel (connect-with (sf_ (update-score _
                                                               (get-in choices [:ornamentation (:connection (first _))]))))))))

         (let [[arpegios ornamentations harmonic-sequences articulations]
               (->> (complementarity-tree 3 3)
                    (leaves-paths)
                    (filter #(= 3 (count %)))
                    (shuffle))

               choices {:harmony {0 _
                                  1 [lydian (transpose c4) (s-shift -1)]
                                  2 [(transpose c2-)]}
                        :arpegio {0 s0 1 s1 2 s2}
                        :ornamentation {0 void
                                        1 (lin vel0 d1)
                                        2 (lin d1- vel0)}
                        :instruments {0 [(vsl/instrument :chamber-violins-1) o1]
                                      1 [(vsl/instrument :chamber-violas)]
                                      2 [(vsl/instrument :chamber-cellos) o1-]}
                        :articulations {0 (vsl/patch :pizzicato)
                                        1 (vsl/patch :pizzicato)
                                        2 (vsl/patch :pizzicato)}}

               degrees (mapcat (fn [s]
                                 (map (choices :harmony) s))
                               harmonic-sequences)
               lines (map (fn [offset]
                            [(get-in choices [:instruments offset])
                             (lin* degrees)
                             (each (tup* (map (fn [d a c p]
                                             [d (tup* (map (fn [step orn p] [(one-of vel3 vel6 vel9) (get-in choices [:articulations p]) (get-in choices [:arpegio step]) {:connection orn}]) a c p))])
                                           degrees
                                           (drop offset (cycle arpegios))
                                           (drop (* 2 offset) (cycle (concat ornamentations arpegios)))
                                           (drop offset (cycle articulations)))))])
                          (range 3))]
           (vsl/noon {:pdf true
                      :play true}
                     (mk dur8
                         dur2
                         dorian
                         (par* (cons bass lines))
                         (lin _ (transpose c3-))
                         ($by :channel (connect-with (sf_ (update-score _
                                                               (get-in choices [:ornamentation (:connection (first _))])))))))))

(comment "scanning"

         (reset! options* {:bpm 60 :tracks {0 :chorium} :pdf true :play true})
         (noon (mk (patch :electric-piano-1)
                   dur2 eolian
                   (nlin> 4 s1)
                   (each (tup _ [s2 c1-] c1- _ s2 [s1 d1]))))

         "it could make sense to have some sort of scan/partition mapping operator"

         (defn scan [by chunk-size step-size f]
           (sfn s (let [chunks (partition chunk-size step-size (sort-by by s))]
                    (reduce (fn [s chunk] (into s (f chunk))) #{} chunks))))

         (noon (mk (patch :electric-piano-1)
                   dur2 eolian
                   (nlin> 4 s3)
                   (scan :position 2 1 (fn [[a b]]
                                         ((efit (tup _ [s2 c1-] c1- _ s2 [(efn e (assoc e :pitch (:pitch b))) d1]))
                                          a)))))

         (noon (mk (patch :electric-piano-1)
                   eolian
                   (nlin> 8 [(degree 4) s1-])
                   (scan :position 2 1 (fn [[a b]]
                                         (upd-in-place a (tup _ [s2 c1-] c1- _ s2 [(efn e (assoc e :pitch (:pitch b))) d1]))))))
         (p (patch :electric-piano-1)
            eolian
            (nlin> 6 s1)
            (each (tup _ c1- [s1 c1-] _)))

         "another approach"

         (defn swap-between [from to f]
           (sfn s (let [s' (update-score s [(trim from to) (in-place f)])]
                    (set (concat (update-score s (trim 0 from))
                                 s'
                                 (update-score s (trim to (score-duration s))))))))

         (noon (mk (nlin> 8 d1)
                   (swap-between 4 6 o1)))

         (defn scan2
           ([size f]
            (scan2 size size f))
           ([size step f]
            (sfn s (reduce (fn [s from]
                             (update-score s (swap-between from (+ from size) f)))
                           s (range 0 (score-duration s) step)))))

         (noon (mk (nlin> 8 d1)
                   (scan2 4 3 (tup _ d1 d1-)))))

(comment (noon {:pdf true
                :play true
                :tracks {0 :chorium}}
               (mk (nlin 2 (one-of d2 d4 d3- d1-))
                   (rep 4 d1-)
                   (chans [(patch :ocarina) o1 _]
                          [(patch :acoustic-bass) _
                           o1- {:position (add 2)}]
                          [(patch :electric-piano-1) d4 {:position (add 1)}]))))

(do :utils

    (defn p [& xs]
      (noon {:play true
             :tracks {0 :chorium}}
            (mk* xs)))

    (defn efit
      "build an efn using a sfn, constraining the result to the input event position and duration"
      [f]
      (ef_ (update-score #{(assoc _ :position 0)}
                [f (adjust _)])))

    (defn in-place
      "Turn the given update `u` into an update that reposition received score to position zero before applying `u` to it.
       The resulting score is then adjusted to its initial duration and shifted to its original position.
       This is useful when you need to scan update a score."
      [u]
      (sf_ (let [score-origin (score-origin _)
                 score-duration (- (score-duration _) score-origin)]
             (update-score (shift-score _ (- score-origin))
                  [u (adjust {:position score-origin :duration score-duration})]))))

    (defn upd-in-place [s u]
      (update-score (score s) (in-place u)))

    (defn connect-with [f]
      (m/$connect (fn [from to]
                    (let [ef (efit (lin _ [(ef_ (conj _ (find to :pitch)))
                                           f]))]
                      (ef from))))))
