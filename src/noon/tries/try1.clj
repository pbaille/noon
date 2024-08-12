(ns noon.tries.try1
  (:use noon.score)
  (:require [noon.lib.melody :as m]
            [noon.lib.harmony :as h]
            [noon.lib.rythmn :as r]
            [noon.utils.pseudo-random :as pr]))

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
