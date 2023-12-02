(ns noon.tries.try1
  (:use noon.score)
  (:require [noon.lib.melody :as m]
            [noon.lib.harmony :as h]
            [noon.lib.rythmn :as r]
            [noon.harmony :as nh]
            [noon.utils.multi-val :as mv]
            [noon.utils.misc :as u]))

(comment :barry-harris

         (def barry-harris (scale [0 2 4 5 7 8 9 11]))

         (play barry-harris
               (tup d0 d3 d4 d7)
               (tup d0 d2)
               (rep 4 d1))


         (let [chord-tones [d0 d2 d4 d7]]
           (play barry-harris
                  (cat d0 d3)
                  (rep 8 (one-of d1- d1))
                  ($ [(chans [(patch :pad-1-new-age) o1- vel3 (par* chord-tones)]
                             [(patch :ocarina) vel4 (shuftup* chord-tones) ($ (maybe (tup (one-of d1 d1-) d0)))]
                             [(patch :vibraphone) vel5 o1 (tupn 6 [(one-of* chord-tones) (maybe o1) (maybe (tup d1- d0))])])
                      (maybe rev)])))

         (def barry-harris2 [barry-harris (struct [0 2 4 7])])

         (play barry-harris2
               (cat I VI VII IV)
               (h/align-contexts :d)
               ($ (chans [(patch :brass) (par s0 s1 s2 s3)]
                         [(patch :acoustic-bass) o1- t-round]
                         [(patch :ethnic) o1 (shuftup s0 s1 s2 s3 s4 s5 s6)]))
               (rep 2 s1)
               (append (transpose c3)))

         (play barry-harris2
               (cat IV I)
               (h/align-contexts :d)
               ($ (par s0 s1 s2 s3))
               (rep 4 (transpose c3))
               h/voice-led))

(comment :minor-progression
         (play (cat [I melodic-minor] [V phrygian3] [V phrygian3] [I melodic-minor]
                    [I phrygian3] [IV dorian] [II locrian] [IIb lydianb7])
               (dup 2)
               (cat {:section :a}
                    [{:section :b} (transpose c6)])
               (h/align-contexts :d)
               (parts {:section :a} ($ (chans [(patch :vibraphone) (shuftup s0 s1 s2 s3 s4 s5)]
                                              [(patch :flute) o1 (shuftup s0 s1 s2 s3 s4 s5)]
                                              [(patch :acoustic-bass) o1- t-round]))
                      {:section :b} ($ (chans [(patch :choir-aahs) vel4 (par s0 s1 s2)]
                                              [(patch :ocarina) vel4 s2- (shuftup s0 s2 s4)]
                                              [(patch :music-box) vel6 o1 (shuftup s0 s1 s2 s3 s4 s5 s6 s7 s8)]
                                              [(patch :acoustic-bass) o1- t-round])))
               (dup 2)))

(comment :one-five
         (play dur3
               (cat [I (scale :melm) (struct :tetrad)]
                    [V (scale :alt) (struct :sus47)])
               (append s1-)
               (append [(transpose c4-)
                        (parts (scale :melm) (scale :lydian)
                               (scale :alt) [(scale :mixolydianb2) (struct [1 5 9 10])])])
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
                 ($ (chans [(patch :choir-aahs) vel4 (par s0 s1 s2 s3)
                            (h/drop 1)]
                           [(patch :acoustic-bass) t-round o1-]
                           [(shuftup s0 s1 s2 s3)
                            ($ (one-of rand-passing rand-line))
                            (chans [(patch :vibraphone) ($ (rand-vel 40 70)) ($ (maybe vel0))]
                                   [(patch :flute) ($ (rand-vel 60 80)) o1 ($ (maybe vel0 [(patch :glockenspiel) vel2 d3]))])])))))

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

               (rep 32 (one-of ($ d3)
                               ($ d3-)
                               (m/rotation 1/2)
                               (m/permutation :rand {:grade 2})
                               (m/contour :similar {:delta 0 :layer :d}))))

         (defn rand-struct [size]
           (ef_ (let [degree-count (-> _ :pitch :scale count)
                      degrees (first (mv/consume size (mv/mix* (range degree-count))))]
                  (upd #{_} (struct (vec (sort degrees)))))))

         (def rand-degree
           (ef_ (let [scale-size (-> _ :pitch :scale count)
                      deg (rand-nth (range 1 scale-size))]
                  (upd #{_} (degree (rand-nth [(- deg) deg]))))))

         (def closed-chord
           (ef_ (let [struct-size (-> _ :pitch :struct count)]
                  (upd #{_} (par* (mapv s-step (range struct-size)))))))

         (defn rand-tup [size]
           (ef_ (let [degree-count (-> _ :pitch :scale count)
                      degrees (first (mv/consume size (mv/mix* (range degree-count))))]
                  (upd #{_} (tup* (mapv d-step degrees))))))

         (play dur:2
               (symetric-modes :whole)
               (rand-struct 3)
               (rep 3 rand-degree)
               ($ (chans [vel4 closed-chord]
                         [(patch :music-box) o1 (rand-tup 5) ($ (one-of vel0 vel3 vel5 vel6))]))
               (append [rev s2])
               (append (transpose c5))
               (append (between 0 1/3))))

(comment :sparkling-waves
         (play dur:4
               vel4
               (scale :lydian)
               (patch :music-box)
               (par s0 s2 s4)
               (rep 3 ($ [{:mark (rand)} s1 {:velocity (div 1.1) :duration (mul 1.3)} (shuftup s2- s0 s2)])
                    :skip-first)
               (cat I [rev III] [o1- V] [rev o1- VII])
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
                 (cat* base)
                 ($ (chans [(patch :piccolo) vel6 o1]
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
               (cat I IV V I)
               (h/align-contexts :s)
               ($ (tupn 2 (tup s0 s2 s4 s4 s2 s0)))
               ($ (! (vel (mul (+ 0.9 (* (rand) 0.2))))))
               (append s1-)))

(comment :elements
         (let [r {:a (r/gen-tup 8 5)
                  :b (r/gen-tup 8 3)
                  :c (r/gen-tup 12 5)}
               m {:a (m/gen-tup :size 8)}
               h {:a [(scale :melm) (struct :tetrad) (cat I [IV dorian] [V (scale :alt) (struct :sus47)] I)]}]
           (play dur2
                 (h :a)
                 (h/align-contexts :d)
                 ($ (chans [(patch :taiko-drum) o1-]
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
                  (upd _ (start-from (score-origin _))))))

         (defn humanize-vel [n]
           (vel (fn [v] (-> (+ v (- (* n (rand)) (/ n 2)))
                            (min 127)
                            (max 0)))))

         (play dur2
               {:grid 1}
               (cat [{:section :A}
                     (cat (catn 4 [(root :F#) locrian2])
                          (catn 4 [(root :F) lydian])
                          (catn 4 [(root :Eb) lydian])
                          (catn 4 [(root :Db) lydian]))]
                    [{:section :B}
                     lydian
                     (cat* (map root [:E :Db :D :B :C :A :Bb :G]))])
               (rep 4 {:grid inc})
               (let [choir [(patch :choir-aahs) vel5 ($ (par> d3 d3 d3)) h/voice-led]
                     bass [(patch :acoustic-bass) ($ [C-2 t-round])]
                     lead-line (any-that (within-pitch-bounds? :C0 :C3)
                                         (rep 2 d3 :skip-first)
                                         (rep 2 d3- :skip-first)
                                         d4 d4-
                                         d1 d1-
                                         (rep 3 d2 :skip-first)
                                         (rep 3 d2- :skip-first))]
                 (parts {:grid 1} (chans choir
                                         bass
                                         (h/grid-zipped
                                          [(patch :music-box)
                                           vel5 C1
                                           (m/simple-line (* 24 10) lead-line)
                                           (adjust 48)]))
                        {:grid 2} (chans choir
                                         bass
                                         (h/grid-zipped
                                          [(patch :ocarina)
                                           vel4 C1
                                           (m/simple-line (* 24 24) lead-line)
                                           (adjust {:position 48 :duration 48})]))
                        {:grid 3} (chans choir
                                         bass
                                         (h/grid-zipped
                                          [(patch :sawtooth)
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
                                                   (sf_ (> (score-duration _) 48))
                                                   (trim 0 48))
                                           (adjust {:position 96 :duration 48})
                                           ($ (humanize-vel 10))]))
                        {:grid 4} (chans [choir ($by :position [(! (one-of (r/gen-tup 8 3 :euclidean)
                                                                           (r/gen-tup 8 3 :durations [2 3 4 5])))
                                                                (sf_ (let [xs (-> (group-by :position _) seq sort vals)]
                                                                       (reduce into #{} (map upd xs (shuffle [d0 d1 d1-])))))])]
                                         bass)))
               (start-from 96)))

(comment :grid
         (stop)
         (play dur3
               (catn> 48 (one-of d1 d1-))
               ($ (chans [(patch :aahs) vel5 (par s0 s1 s2 s3)]
                         [(patch :ocarina) (shuftup s0 s2 s4 s6) (shuftup d0 d3 d6) (tup _ rev)]
                         [(patch :acoustic-bass) t2-]))
               (h/grid dur3 tetrad
                       (cat [I lydian (struct [2 3 5 6])]
                            [IIb dorian (struct [1 2 3 6])]
                            [V mixolydian (struct [2 3 5 6])]
                            [Vb melodic-minor (struct [1 2 5 6])])
                       (rep 6 (transpose c2-))
                       (dup 2)
                       (h/align-contexts :d :static)))

         (play (chans [(patch :aahs) vel5 (par s0 s1 s2 s3)]
                      [(patch :acoustic-bass) t2-])
               (h/grid (cat [I lydian (struct [2 3 5 6])]
                            [IIb dorian (struct [1 2 3 6])]
                            [V mixolydian (struct [2 3 5 6])]
                            [Vb melodic-minor (struct [1 2 5 6])])
                       (rep 2 (transpose c2-))
                       (dup 2)
                       (h/align-contexts :d :static)
                       (adjust 1))
               (parts (patch :acoustic-bass)
                      ($ (tup (maybe o1) (one-of d4 d3-))))
               (adjust 32))

         (play (chans [(patch :aahs)
                       vel6
                       (rup 24 (any-that (within-pitch-bounds? :G-1 :G1)
                                         s2 s2- s3 s3-))
                       ($ (par s0 s1 s2 s3))]
                      [(patch :acoustic-bass) t2-])
               (h/grid tetrad
                       (cat [I lydian (struct [2 3 5 6])]
                            [IIb dorian (struct [1 2 3 6])]
                            [V mixolydian (struct [2 3 5 6])]
                            [Vb melodic-minor (struct [1 2 5 6])])
                       (rep 2 (transpose c2-))
                       (dup 2)
                       (h/align-contexts :d :static)
                       (adjust 1))
               (parts (patch :acoustic-bass)
                      ($ (tup (maybe o1) (one-of d4 d3-))))
               (adjust 32)))

(comment :zip-rythmn

         (play lydianb7
               (chans
                [(patch :vibraphone)
                 (shufcat d0 d1 d3 d4 d6)
                 (catn 4 (one-of d2 d2-))
                 (sf_ (let [rythmn (mk (catn 2 (! (r/gen-tup 12 5 :shifted))) (append rev))]
                        (set (map (fn [r n]
                                    (merge n (select-keys r [:position :duration])))
                                  (sort-by :position rythmn)
                                  (sort-by :position _)))))]
                [(patch :woodblock) (r/gen-tup 12 5 :euclidean) (dup 4)]
                [(patch :tinkle-bell) (dup 4)]
                [(patch :metallic) (shufcat d0 d3 d1- d4) ($ (par d0 d1 d4))]
                [(patch :acoustic-bass) t2- (dup 4)])
               (adjust 8)
               (append [(transpose c3-) s1 rev] _)))

(comment :target-notes
         "Building good rythmic melodies is not easy."
         "Here, I will try to start from target notes and fill the holes between them."

         (play eolian
               (cat s0 s2 s1 s0))

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
                                                (upd #{(assoc a :position 0)}
                                                     (rup cnt (case direction :up d1 :down d1-))))))
                              #{}
                              couples)
                      (conj (last sorted))))))

         (play eolian
               (cat s0 s2 s1 s0)
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
                                                (upd #{(assoc a :position 0)}
                                                     (rup cnt (ef_ (update _ :pitch (nh/layer-step layer increment))))))))
                              #{}
                              couples)
                      (conj (last sorted))))))

         (play eolian
               (cat s0 s2 s1 s0)
               (fill-line :c))

         (play dur:2
               harmonic-minor
               tetrad
               (patch :orchestral-harp)
               (cat s0 s2 s2- s4 s4- s2 s2- s5-)
               (cat _ [(transpose c6) s2 rev])
               (cat _ s2 s2-)
               (fill-line :s))

         "Next step will be to have control over the number of notes between targets"

         (defn target [layer size direction duration]
           (ef_ (into #{_} (map (fn [i]
                                  (-> (update _ :pitch (nh/layer-step layer (inc i)))
                                      (update :position - (* (inc i) duration))
                                      (assoc :duration duration)))
                                (range size)))))

         (play (cat _ [s2 (target :c 3 :up 1/4)]))

         "The problem here is that the precedent note overlaps the targeting notes"

         (defn connect [size]
           (sf_ (let [sorted (sort-by :position _)]
                  (reduce (fn [s [n1 n2]]
                            (let [duration (/ (:duration n1) (inc size))]
                              (into s (map-indexed (fn [idx pitch] (assoc n1 :pitch pitch :position (+ (* idx duration) (:position n1)) :duration duration))
                                                   (butlast (nh/simplest-connection size (:pitch n1) (:pitch n2)))))))
                          #{(last sorted)} (partition 2 1 sorted)))))

         (play dur2
               harmonic-minor
               tetrad
               (cat s0 s2 s2- s4 s4- s2 s2- s5-)
               (chans [(connect 5)]
                      [(patch :ocarina) o1])))

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
