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

(comment :elements
         (let [r {:a (r/gen-tup 8 5)
                  :b (r/gen-tup 8 3)
                  :c (r/gen-tup 12 5)}
               m {:a (m/gen-tup :d 4 3)}
               h {:a [(scale :melm) (structure :tetrad) (lin I [IV dorian] [V (scale :alt) (structure :sus47)] I)]}]
           (play dur2
                 (h :a)
                 (h/align-contexts :d)
                 (dup 2)
                 (append (transpose c3-))
                 (each (chans [(patch :taiko-drum) o1-]
                              [(patch :woodblock) vel3 C2 (r :c) (maybe (m/rotation :rand))]
                              [(patch :acoustic-bas) o1- t-round]
                              [(patch :vibraphone) vel3 h/simple-chord]
                              [(patch :flute) (m :a) (mixtup s0 s2 s4)])))))

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
          (m/$lin [(lin s0 s2 s2- s4) (maybe [rev s2])])
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
          (m/$lin [(lin s0 s2 s2- s4) (maybe [rev s2])])
          (lin _ s1 s1- _)
          (chans [(patch :tango) (m/connect 5 3 2 1 0)]
                 [(patch :ocarina) vel6 s2 (m/connect 2 1 0)]
                 [(patch :acoustic-bass) o1- s2- (m/connect 1 0)])))

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
                        (if (> (count ret) 32)
                          ret
                          (let [picked (rand-nth (pitch-class-value->triads current))
                                pitch-class-values (triad->pitch-class-values picked)
                                inv (rand-nth [0 1 2])]
                            (recur (-> (get pitch-class-values inv)
                                       ((rand-nth [inc dec]))
                                       (mod 12))
                                   (conj ret [picked inv])))))]
           (noon {:play true
                  :midi true
                  :filename "test/data/garzone1"}
            (mk (lin* (map (fn [[triad inv]] [dur:2
                                             triad
                                             (chans [(patch :acoustic-bass) o1- t-round]
                                                    [(patch :electric-piano-1)
                                                     ({0 (tup s1 s2 s0)
                                                       1 (tup s0 s2 s1)
                                                       2 (tup s0 s1 s2)} inv)
                                                     (par _
                                                          [o1 rev])])])
                           triads))
                (lin _ s1 s1- _))))

         (let [all-triads (for [root' [:C :Db :D :Eb :E :F :Gb :G :Ab :A :Bb :B]
                                kind [ionian eolian #_lydian+ #_superlocrian]]
                            [(root root') kind])

               triad->pitch-class-values (reduce (fn [ret t]
                                                   (assoc ret t (mapv pitch-class-value (mk t (par s0 s1 s2)))))
                                                 {} all-triads)

               pitch-class-value->triads (reduce (fn [ret [t vs]] (reduce #(update %1 %2 conj t) ret vs))
                                                 {} triad->pitch-class-values)
               triads (loop [triad (pr/rand-nth all-triads)
                             inver (pr/rand-nth [0 1 2])
                             ret []]
                        (if (> (count ret) 4)
                          ret
                          (let [pitch-class-values (triad->pitch-class-values triad)
                                connection (get pitch-class-values inver)
                                candidates (-> ((pr/rand-nth [inc dec]) connection)
                                               (mod 12)
                                               (pitch-class-value->triads))
                                next-triad (pr/rand-nth (remove (partial = triad) candidates))
                                next-inversion (pr/rand-nth (remove (partial = inver) [0 1 2]))]
                            (recur next-triad
                                   next-inversion
                                   (conj ret [triad (inversion inver)])))))]
           (play
            (lin* (map (fn [triad]
                         [dur4
                          triad
                          (chans [(patch :acoustic-bass) o1- t-round]
                                 [(patch :electric-piano-1)
                                  (par s0 s1 s2)
                                  #_(par _
                                         [o1 rev])])])
                       triads)))))

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
