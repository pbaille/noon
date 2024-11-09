(ns noon.doc.examples-test
  "This file is generated from `src/noon/doc/examples.org`"
  (:use noon.score)
  (:require [noon.lib.melody :as m]
            [noon.lib.harmony :as h]
            [noon.lib.rythmn :as r]
            [noon.harmony :as nh]
            [noon.utils.multi-val :as mv]
            [noon.utils.misc :as u]
            [noon.utils.pseudo-random :as pr]
            [noon.constants :as nc]
            [noon.midi :as midi]
            [noon.vst.vsl :as vsl :refer [vsl]]
            [clojure.math.combinatorics :as comb]
            [clojure.test :refer [deftest testing is]]
            [noon.test]))
(def fill-diatonically
  "A very low level way to connect subsequent notes diatonically using `noon.harmony` directly.\n   It feels too complicated for such a simple thing..."
  (sf_
    (let [sorted (sort-by :position _)
          couples (partition 2 1 sorted)]
      (-> (reduce
            (fn [ret [a b]]
              (let [va (pitch-value a)
                    vb (pitch-value b)
                    direction (if (> va vb) :down :up)
                    cnt (loop [cnt 0
                               current (:pitch a)]
                          (case direction
                            :up (if (>= (nh/hc->chromatic-value current) vb)
                                  cnt
                                  (recur (inc cnt)
                                         (nh/upd current (nh/d-step 1))))
                            :down (if (<= (nh/hc->chromatic-value current) vb)
                                    cnt
                                    (recur (inc cnt)
                                           (nh/upd current (nh/d-step -1))))))]
                (concat-score ret
                              (update-score #{(assoc a :position 0)}
                                            (rup cnt
                                                 (case direction
                                                   :up d1
                                                   :down d1-))))))
            #{}
            couples)
          (conj (last sorted))))))
(defn fill-line
  "This evolution of fill-diatonically let the user specify the harmonic layer.\n   It is still relying on `noon.harmony` which is not great."
  [layer]
  (sf_
    (let [sorted (sort-by :position _)
          couples (partition 2 1 sorted)]
      (-> (reduce
            (fn [ret [a b]]
              (let [va (pitch-value a)
                    vb (pitch-value b)
                    direction (if (> va vb) :down :up)
                    [check increment] (case direction
                                        :up [>= 1]
                                        :down [<= -1])
                    cnt (loop [cnt 0
                               current (:pitch a)]
                          (if (check (nh/hc->chromatic-value current) vb)
                            cnt
                            (recur (inc cnt)
                                   (nh/upd current
                                           (nh/layer-step layer increment)))))]
                (concat-score
                  ret
                  (update-score
                    #{(assoc a :position 0)}
                    (rup cnt
                         (ef_ (update _
                                      :pitch
                                      (nh/layer-step layer increment))))))))
            #{}
            couples)
          (conj (last sorted))))))
(defn target
  [layer size direction duration]
  (sfn score
       (->> score
            (map (fn [e]
                   (->> (range size)
                        (map (fn [i]
                               (-> (update e
                                           :pitch
                                           (nh/layer-step layer
                                                          (case direction
                                                            :up (inc i)
                                                            :down (- (inc i)))))
                                   (update :position - (* (inc i) duration))
                                   (assoc :duration duration))))
                        (into #{e}))))
            (merge-scores))))
(defn connect
  [& sizes]
  (sf_
    (let [sorted (sort-by :position _)]
      (reduce
        (fn [s [n1 n2]]
          (let [hcs (loop [sizes sizes]
                      (if-let [[s & sizes] (seq sizes)]
                        (or (nh/simplest-connection s (:pitch n1) (:pitch n2))
                            (recur sizes))))
                duration (/ (:duration n1) (dec (count hcs)))]
            (into s
                  (map-indexed (fn [idx pitch]
                                 (assoc n1
                                   :pitch pitch
                                   :position (+ (* idx duration) (:position n1))
                                   :duration duration))
                               (butlast hcs)))))
        #{(last sorted)}
        (partition 2 1 sorted)))))
(defn chromatic-double-passing
  [side]
  (sf_ (assert
         (= 1 (count _))
         (str
           (quote
             user-a1eaf59e-ba2d-4dbf-89d6-d981cdab28df/chromatic-double-passing)
           "works only on single note scores"))
       (let [target (first _)
             d-suroundings (nh/diatonic-suroundings (:pitch target))
             c-space (get d-suroundings
                          (case side
                            :up 1
                            :down 0))
             step (case side
                    :up 1
                    :down -1)]
         (update-score _
                       (if (= c-space 2)
                         (tup (d-step step) (c-step step) same)
                         (tup (d-step step)
                              (case side
                                :up c1-
                                :down d1)
                              same))))))
(defn interpose-with
  [f]
  (sf_ (if (m/line? _)
         (set (mapcat (fn [[a b]] (if b ((f a b)) a))
                (partition 2 1 nil (sort-by :position _)))))))
(defn interleaved
  [& xs]
  (sf_
    (let [scores (map (partial update-score _) xs)
          counts (map count scores)
          durations (map score-duration scores)]
      (assert (apply = counts)
              "interleaved scores should have same number of elements")
      (assert (apply = durations)
              "interleaved scores should have same duration")
      (assert (apply = (mapcat (partial map :duration) scores))
              "interleaved scores should have even durations")
      (let [duration (/ (first durations) (first counts))
            shift (/ duration (count scores))]
        (:score (reduce (fn [{:as state, :keys [at]} xs]
                          (-> state
                              (update :at + duration)
                              (update :score
                                      into
                                      (map-indexed (fn [i n]
                                                     (assoc n
                                                       :position (+ at
                                                                    (* i shift))
                                                       :duration shift))
                                                   xs))))
                  {:score #{}, :at 0}
                  (apply map vector (map sort-score scores))))))))
(defn interleaving
  [polarities a b]
  (loop [s []
         ps polarities
         a a
         b b]
    (if-let [[p & ps] (seq ps)]
      (let [[nxt a' b'] (case p
                          0 [(first a) (next a) b]
                          1 [(first b) a (next b)])]
        (recur (conj s nxt) ps a' b'))
      s)))
(defn rand-interleaving
  ([a b]
   (interleaving (pr/shuffle (concat (repeat (count a) 0) (repeat (count b) 1)))
                 a
                 b))
  ([a b & xs] (reduce rand-interleaving (rand-interleaving a b) xs)))
(defn interleavings
  [a b]
  (reduce (fn [ret perm] (conj ret (interleaving perm a b)))
    []
    (comb/permutations (concat (repeat (count a) 0) (repeat (count b) 1)))))
(u/defn* randomly-interleaved
         "randomly interleave the result of the given updates"
         [xs]
         (sf_
           (:score
             (reduce (fn [state n]
                       (-> state
                           (update :score conj (assoc n :position (:at state)))
                           (update :at + (:duration n))))
               {:at 0, :score #{}}
               (apply rand-interleaving
                 (map (fn [u] (sort-by :position (update-score _ u))) xs))))))
(defn n-firsts
  [n]
  (sf_ (->> (group-by :position _)
            (sort)
            (take n)
            (map second)
            (reduce into #{}))))
(defn connect-with
  "use `f` to connect subsequent notes of a score."
  [f]
  (connect-by :position
              (fn [chunk1 chunk2]
                (let [from (first chunk1)
                      to (first chunk2)]
                  (update-score #{(assoc from :position 0)}
                                [(lin _ [(ef_ (assoc _ :pitch (:pitch to))) f])
                                 (adjust from)])))))
(defn connect-with2
  [f]
  (connect-by :position
              (fn [chunk1 chunk2]
                (let [from (first chunk1)
                      to (first chunk2)]
                  (update-score #{(assoc from :position 0)}
                                [(lin _ [(repitch (event->pitch to)) f])
                                 (adjust from)])))))
(def decorate
  (sf_ (let [sorted (sort-by :position _)]
         (reduce (fn [s [n1 n2]]
                   (into s (update-score #{n1 n2} (maybe (m/connect 1)))))
           #{(last sorted)}
           (partition 2 1 sorted)))))
(def barry-harris (scale [0 2 4 5 7 8 9 11]))
(def barry-harris2 [barry-harris (structure [0 2 4 7])])
(def symetric-modes
  {:messian3 (scale [0 2 3 4 6 7 8 10 11]),
   :messian4 (scale [0 1 2 5 6 7 8 11]),
   :messian5 (scale [0 1 5 6 7 11]),
   :messian7 (scale [0 1 2 3 5 6 7 8 9 11]),
   :half-augm (scale [0 1 4 5 8 9]),
   :messian6 (scale [0 2 4 5 6 8 10 11]),
   :whole (scale [0 2 4 6 8 10]),
   :augm-half (scale [0 3 4 7 8 11]),
   :half-whole (scale [0 1 3 4 6 7 9 10]),
   :whole-half (scale [0 2 3 5 6 8 9 11])})
(defn rand-structure
  [size]
  (ef_ (let [degree-count (-> _
                              :pitch
                              :scale
                              count)
             degrees (first (mv/consume size (mv/mix* (range degree-count))))]
         ((structure (vec (sort degrees))) _))))
(def rand-degree
  (ef_ (let [scale-size (-> _
                            :pitch
                            :scale
                            count)
             deg (pr/rand-nth (range 1 scale-size))]
         ((degree (pr/rand-nth [(- deg) deg])) _))))
(defn rand-tup
  [size]
  (e->s event
        (let [degree-count (-> event
                               :pitch
                               :scale
                               count)
              degrees (first (mv/consume size (mv/mix* (range degree-count))))]
          (update-score #{event} (tup* (mapv d-step degrees))))))
(defn complementarity-tree
  ([structure-size sequence-size]
   (let [elements (range structure-size)
         q (quot sequence-size structure-size)
         r (rem sequence-size structure-size)
         base (apply concat (repeat q elements))
         partials (filter (fn [s] (= r (count s))) (comb/subsets elements))
         permutations (mapcat (fn [p] (comb/permutations (concat base p)))
                        partials)]
     (complementarity-tree [] structure-size (set permutations))))
  ([at structure-size perms]
   (if-let [perms (some-> (if (seq at)
                            (filter (fn [p']
                                      (every? (fn [xs]
                                                (apply distinct?
                                                  (map (fn* [%1]
                                                         (mod %1
                                                              structure-size))
                                                    xs)))
                                              (apply map vector p' at)))
                              perms)
                            perms)
                          seq
                          set)]
     (->> perms
          (map (fn [child] [child
                            (complementarity-tree (conj at child)
                                                  structure-size
                                                  (disj perms child))]))
          (into {})))))
(defn leaves-paths
  ([m] (leaves-paths m []))
  ([x at]
   (if (and (map? x) (not-empty x))
     (mapcat (fn [[k v]] (leaves-paths v (conj at k))) x)
     [at])))
(def GIANT_STEPS
  (let [II [II {:degree :II}]
        V [V {:degree :V}]
        I [I {:degree :I}]
        t1 same
        t2 (transpose c4-)
        t3 (transpose c4)
        s1 (lin [t1 I] [t2 (lin V I)] [t3 (lin V [dur2 I])] [t2 (lin II V)])
        II-V-I (lin II V [I dur2])]
    [tetrad
     (tup s1 [t2 s1] [t3 I dur2] [t2 II-V-I] II-V-I [t3 II-V-I] [t1 (lin II V)])
     (h/align-contexts :structural :static)]))
(def ESP_fullgrid
  (let [common (lin [VII superlocrian dur2]
                    [I lydian dur2]
                    [VII superlocrian dur2]
                    [VIIb lydian dur2]
                    [VI superlocrian]
                    [VIIb lydian]
                    [VII superlocrian]
                    (tup [I lydian] [VIIb lydianb7]))]
    (tup common
         (lin [VI dorian] [II lydianb7] [II dorian] [IIb lydianb7])
         common
         (lin [VIb lydianb7] [II dorian] (tup [VIb dorian] [IIb lydianb7]) I))))
(def CYCLIC_EPISODE
  (let [a1 [dorian (rep 4 (transpose c3))]
        a2 [dorian (rep 4 (transpose c3-))]
        b (lin [IV dorian] [V superlocrian (structure [2 3 5 6])])
        c (lin [V mixolydian sus47] [V phrygian sus27])
        d [dorian (append (transpose c3))]]
    [tetrad
     (tup [(root :Bb) a1]
          [(root :G) b]
          [(root :D) b]
          [(root :D) a2]
          [(root :G) c]
          [(root :Eb) d]) (dupt 4) (h/align-contexts :s :static)]))
(defn last-n-positions
  "Limit the score to the n latest positions found."
  [n]
  (sf_ (let [_ (->> (group-by :position _)
                    seq
                    (sort-by key)
                    reverse
                    (take n)
                    (map second)
                    (reduce into #{}))]
         (update-score _ (start-from (score-origin _))))))
(defn possible-modes
  "given a chromatic degree (int between 0 an 11)\n   return possible modes"
  [cd modal-lvl least-priority]
  (let [modes (nc/lvl->mode->degree-priority modal-lvl)
        candidates (filter (fn [[_ s]]
                             (-> (take least-priority s)
                                 (set)
                                 (contains? cd)))
                     modes)]
    candidates))
(deftest main
  (testing "Noon experiments"
    (testing "Harmonic experiments"
      (testing "Not too happy birthday"
        (is (noon.test/frozen*
              nil
              (noon.score/mk harmonic-minor
                             (lin I V VII I [IV melodic-minor VII] IV I VII)
                             (h/align-contexts :s)
                             (each (par (par s0 s1 s2) [o1 (shuftup s0 s1 s2)]))
                             (dup 4)))))
      (testing "epic lydian"
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              {:description "epic lydian sequence by minor thirds"}
              (h/harmonic-zip
                [lydian sus47 (tup* (map root [:C :Eb :F# :A])) (dupt 2)
                 (h/align-contexts :s)]
                (par [(chan 1) (patch :choir-aahs) vel3 (ntup 8 (par s0 s1 s2))]
                     [vel4
                      (let [s? (one-of s2- s1- s1 s2)]
                        (m/simple-tupline (* 16 16)
                                          (any-that
                                            (within-pitch-bounds? :C-1 :C2)
                                            (lin s? s?)
                                            [(shuflin s1 s2 s3 s4) (maybe rev)]
                                            (lin d1 d1- s0 s?)
                                            (lin d1- d1 s0 s?))))
                      (par [(chan 2) (patch :french-horn)]
                           [(chan 3) vel5 o2 (patch :flute)])]
                     [(chan 4) (patch :taiko-drum) vel2
                      (ntup 16 (lin dur3 [o1 vel4 dur2] dur3))]
                     [(chan 5) (patch :acoustic-bass) o2- (ntup 32 t0)]))
              (adjust 32)
              (nlin 4 (s-shift -1))))))
      (testing "Tritonal experiment"
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              {:description "tritonal chord sequence shifts by minor thirds"}
              (let [I (one-of [lydian+ (structure [2 3 4 5 6])]
                              [melodic-minor (structure [1 2 4 5 6])])
                    V (one-of [V mixolydian (structure [1 3 4 5 6])]
                              [V phrygian6 (structure [0 1 3 5 6])])
                    [B G Eb] (map root [:B :G :Eb])]
                [(tup [B V] [B I] [G V] [G I] [Eb V dur2] [Eb I dur2])
                 (rup 4 (transpose d2-)) (h/align-contexts :s :static)
                 (chans
                   [(patch :choir-aahs) vel3 (each (par s0 s1 s2 s3 s4))]
                   [(patch :vibraphone) vel5
                    (each (probs {(par s0 s1 s2 s3 s4) 1,
                                  (shuftup [dur2 (par s0 s2 s4)]
                                           [(one-of dur2 dur3) (par s1- s1 s3)])
                                    3}))]
                   [(patch :acoustic-bass) vel5
                    (each [tetrad o2- t0
                           (maybe (tup (one-of dur2 dur3) [dur2 o1-]))])]
                   [(patch :taiko-drum) vel3 (each (shuftup s0 s1 s2 s3 s4))
                    (each
                      (probs
                        {vel0 3, same 1, (one-of o1 o1-) 1, (tup t0 t1) 1}))]
                   [vel6
                    (h/grid-zipped
                      [(chans (patch :flute) [o1 (patch :piccolo)])
                       (ntup> (* 32 10)
                              (any-that (within-pitch-bounds? :C-2 :C2)
                                        s1
                                        s2
                                        s1-
                                        s2-
                                        s3
                                        s3-))]
                      (each (probs {vel0 1,
                                    same 4,
                                    (superpose (one-of s1 s2 s3)) 0})))])
                 (adjust 48)])))))
      (testing "simple I IV VII I"
        (is (noon.test/frozen* nil
                               (noon.score/mk (scale :harmonic-minor)
                                              (lin I IV VII I)
                                              (h/align-contexts :s)
                                              (each (tup s0 s1 s2)))))
        (is (noon.test/frozen* nil
                               (noon.score/mk (scale :harmonic-minor)
                                              (lin I IV VII I)
                                              (h/align-contexts :s)
                                              (lin s0 s1 s2-)
                                              (each [(tup s0 s2)
                                                     (each (tup s0 c1- s+ s0))])
                                              (append rev))))
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              dur2
              (scale :harmonic-minor)
              (lin I IV VII I)
              (h/align-contexts :s)
              (lin same (transpose c3) same)
              (chans
                [(patch :choir-aahs) vel4
                 (each [(par s0 s1 s2) (maybe (tup s0 s1-) (tup s0 s1))])]
                [(patch :ocarina) vel6
                 (each [(shuftup s0 s1 s2)
                        (each (one-of (tup s0 (shuflin (one-of c1- s-) s+) s0)
                                      (tup s0 c1- s0 (one-of s2- s2))))])]
                [(patch :kalimba) vel4 o2
                 (each [(shuftup s0 s1 s2)
                        (each (one-of vel0 (par s0 s2-) (shuftup s0 s1 s2)))])]
                [(patch :acoustic-bass) vel3 o2-])))))
      (testing "I.m.M7 VI.alt bVI.7.#11 bII.7.sus4"
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              (lin [I melodic-minor]
                   [VI superlocrian]
                   [VIb lydianb7]
                   [IIb mixolydian])
              (h/align-contexts :s)
              (dup 2)
              (each (chans
                      [(patch :vibraphone) vel6 t0 (par> d0 d3 d3 d3 d3)]
                      [(patch :acoustic-bass) vel6 t2-]
                      [(patch :taiko-drum) (shuftup vel3 vel5 [vel4 (dupt 2)])]
                      [(ntup> 9
                              (any-that (within-pitch-bounds? :G-1 :C2)
                                        d1-
                                        d1
                                        d3
                                        d3-
                                        d4
                                        d4-)) vel9
                       (chans (patch :flute) [o1- vel4 (patch :vibraphone)])]))
              (lin _ c6)
              (dup 2)))))
      (testing "simple I IV I V"
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              dur2
              (lin I IV I V)
              (h/align-contexts :s)
              (each (chans [(patch :woodblock) C0 (dupt 4)]
                           [(patch :tinkle-bell) C0
                            (r/gen-tup 12 5 {:durations [1 2 3]})]
                           [(patch :marimba) o1- (r/gen-tup 12 5 :euclidean)
                            (each (par s0 s2)) (each (one-of s0 s1 s1-))]
                           [(patch :acoustic-bass) t2- vel10
                            (r/gen-tup 12 5 :euclidean :shifted)]
                           [vel12 (patch :music-box) o1 (one-of s0 s1 s1-)
                            (shuftup s0 s1 s3)
                            (each (probs {[(par s0 s2) (maybe (tup s0 s1))] 3,
                                          [(tup s3 s1 (par s2 s0) s1-)] 2,
                                          [(tup d1- s0 d1 s0)
                                           (maybe (m/rotation 2))]
                                            1}))]))
              (dup 2)))))
      (testing "Elliot smith chords"
        (is (noon.test/frozen*
              nil
              (noon.score/mk dur2
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
                                          h/simple-chord)))))
        (is
          (noon.test/frozen* nil
                             (noon.score/mk
                               (chans [(patch :electric-piano-1)
                                       (tup (shuftup s0 s1 s2 s3)
                                            (shuftup s2 s3 s4 s5))]
                                      [(patch :acoustic-bass) o1- t-round])
                               (dupt 8)
                               (h/grid [(tup [VI seventh]
                                             [IV add2]
                                             [I]
                                             [III seventh (inversion 2)]
                                             [VI seventh]
                                             [IV add2]
                                             (tup I [III seventh phrygian3])
                                             [IV]) (h/align-contexts :d)])
                               (adjust 8)
                               (dup 2)))))
      (testing "Minor progression"
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              (lin [I melodic-minor]
                   [V phrygian3]
                   [V phrygian3]
                   [I melodic-minor]
                   [I phrygian3]
                   [IV dorian]
                   [II locrian]
                   [IIb lydianb7])
              (dup 2)
              (lin {:section :a} [{:section :b} (transpose c6)])
              (h/align-contexts :d)
              (parts {:section :a}
                     (each (chans
                             [(patch :vibraphone) (shuftup s0 s1 s2 s3 s4 s5)]
                             [(patch :flute) o1 (shuftup s0 s1 s2 s3 s4 s5)]
                             [(patch :acoustic-bass) o1- t-round]))
                     {:section :b}
                     (each (chans [(patch :choir-aahs) vel4 (par s0 s1 s2)]
                                  [(patch :ocarina) vel4 s2- (shuftup s0 s2 s4)]
                                  [(patch :music-box) vel6 o1
                                   (shuftup s0 s1 s2 s3 s4 s5 s6 s7 s8)]
                                  [(patch :acoustic-bass) o1- t-round])))
              (dup 2)))))
      (testing "I V"
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              dur3
              (lin [I (scale :melm) (structure :tetrad)]
                   [V (scale :alt) (structure :sus47)])
              (append s1-)
              (append [(transpose c4-)
                       (parts (scale :melm)
                              (scale :lydian)
                              (scale :alt)
                              [(scale :mixolydianb2) (structure [1 5 9 10])])])
              (dup 2)
              (h/align-contexts :s)
              (let [below (one-of d1- s1-)
                    above (one-of d1 s1)
                    contours [[0 -1 1 0] [0 1 -1 0] [-1 0 1 0] [1 0 -1 0]
                              [1 0 -1 0] [-1 0 1 0]]
                    passings (mapv (partial mapv {0 _, -1 below, 1 above})
                               contours)
                    rand-passing (one-of* (map tup* passings))
                    below-step (one-of d1- d3- d4-)
                    above-step (one-of d1 d3 d4)
                    rand-line (rup 4 (one-of below-step above-step))
                    rand-vel (fn [min max]
                               {:velocity
                                  (fn [_] (+ min (pr/rand-int (- max min))))})]
                (each (chans
                        [(patch :choir-aahs) vel4 (par s0 s1 s2 s3) (h/drop 1)]
                        [(patch :acoustic-bass) t-round o1-]
                        [(shuftup s0 s1 s2 s3)
                         (each (one-of rand-passing rand-line))
                         (chans [(patch :vibraphone) (each (rand-vel 40 70))
                                 (each (maybe vel0))]
                                [(patch :flute) (each (rand-vel 60 80)) o1
                                 (each (maybe vel0
                                              [(chan inc) (patch :glockenspiel)
                                               vel4]))])]))))))))
    (testing "Melodic experiments"
      (testing "Target notes"
        (is (noon.test/frozen* nil (noon.score/mk aeolian (lin s0 s2 s1 s0))))
        (is (noon.test/frozen*
              nil
              (noon.score/mk aeolian (lin s0 s2 s1 s0) fill-diatonically)))
        (is (noon.test/frozen*
              nil
              (noon.score/mk aeolian (lin s0 s2 s1 s0) (fill-line :c))))
        (is (noon.test/frozen* nil
                               (noon.score/mk dur:2
                                              harmonic-minor
                                              tetrad
                                              (patch :orchestral-harp)
                                              (lin s0 s2 s2- s4 s4- s2 s2- s5-)
                                              (lin _ [(transpose c6) s2 rev])
                                              (lin _ s2 s2-)
                                              (fill-line :s))))
        (is (noon.test/frozen* nil
                               (noon.score/mk
                                 (lin _
                                      [s2 (target :c 3 :up 1/4)]
                                      [s1- (target :d 3 :down 1/4)]
                                      [_ (target :c 3 :up 1/4)])
                                 (options {:filename "test/trash/target"}))))
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              harmonic-minor
              (lin I [VI lydianb7] V IV [II phrygian3] [V aeolian] [IIb lydian])
              (h/align-contexts :s)
              (m/$lin [(lin s0 s2 s2- s4) (maybe [rev s2])])
              (lin _ s1 s1- _)
              (chans [(patch :tango) (connect 5 3 2 1 0)]
                     [(patch :ocarina) vel6 s2 (connect 2 1 0)]
                     [(patch :acoustic-bass) o1- s2- (connect 1 0)]))))
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              harmonic-minor
              (lin I [VI lydianb7] V IV [II phrygian3] [V aeolian] [IIb lydian])
              (h/align-contexts :s)
              (m/$lin [(lin s0 s2 s2- s4) (maybe [rev s2])])
              (lin _ s1 s1- _)
              (chans [(patch :tango) (m/connect 5 3 2 1 0)]
                     [(patch :ocarina) vel6 s2 (m/connect 2 1 0)]
                     [(patch :acoustic-bass) o1- s2- (m/connect 1 0)])))))
      (testing "Passing notes"
        (testing "simple"
          (is (noon.test/frozen* nil
                                 (noon.score/mk dorian
                                                (rep 4 s1)
                                                (each (tup c1- s2 s1 s0))
                                                (tup _ rev)
                                                (rep 4 (transpose c3))
                                                (append rev))))
          (is (noon.test/frozen* nil
                                 (noon.score/mk dorian
                                                (rep 4 s1)
                                                (each (tup _ s2))
                                                (each (tup c1- d2 d1 d0)))))
          (is (noon.test/frozen*
                nil
                (noon.score/mk
                  melodic-minor
                  dur4
                  (append (transpose c3) (transpose c6) (transpose c3))
                  (dup 2)
                  (each (shuftup s0 s1 s2 s3 s4))
                  (each (tup _ (one-of s1 s2 s1- s2- s3 s3-)))
                  (each (one-of (tup c1- d2 d1 d0) (tup c1- s1- s0 s2))))))
          (is
            (noon.test/frozen*
              nil
              (noon.score/mk
                dur4
                (append (transpose c3) (transpose c6) (transpose c3))
                (each (one-of phrygian6 lydian melodic-minor))
                (dup 2)
                (each (chans
                        [(patch :acoustic-bass) t2- (tup _ s2 s1- _)]
                        [(patch :flute) vel8]
                        [(patch :vibraphone) vel4 (par s0 d4 d6 d8 d10 d12)]
                        [(patch :taiko-drum) (r/gen-tup 10 4 :euclidean)
                         (each [(one-of s0 s1 s1-) (one-of vel1 vel3 vel5)])]))
                (parts (chan 1)
                       [(each (shuftup s0 s1 s2 s3 s4))
                        (each (tup _ (one-of s1 s2 s1- s2- s3 s3-)))
                        (each (one-of (tup c1- d2 d1 d0)
                                      (tup c1- s1- s0 s2)
                                      (tup c1- s1- s2- s0)))
                        (each (one-of vel5 vel6 vel7 vel9))]))))
          (is (noon.test/frozen*
                nil
                (noon.score/mk melodic-minor
                               (shuflin s0 s1 s2 s3)
                               (each (let [step (one-of s1 s2 s3 s1- s2- s3-)
                                           ap (lin c1- d1 s1-)]
                                       (tup [_ ap] [step ap] _ step)))
                               (append c2- c2-))))
          (is (noon.test/frozen*
                nil
                (noon.score/mk melodic-minor
                               (lin (shuflin s0 s1 s2 s3)
                                    [{:passing true} (shuflin s0 s1 s2 s3)])
                               (each
                                 (let [step (one-of s1 s2 s3 s1- s2- s3-)
                                       ap (lin c1- d1)]
                                   (tup [_ ap] [step ap] _ step (par s2- s2))))
                               (append c4-)
                               (dup 2))))
          (is (noon.test/frozen* nil
                                 (noon.score/mk
                                   melodic-minor
                                   dur:3
                                   (shuflin s0 s2 s4)
                                   (each (one-of (shuftup _ c1- d1)
                                                 (shuftup _ d1 d1-)))
                                   (m/permutation :rand)
                                   (rep 3 (one-of (s-shift 1) (s-shift -1)))
                                   (rep 3 (transpose c3))
                                   (dup 2))))
          (is (noon.test/frozen*
                nil
                (noon.score/mk dorian+4
                               (lin I IV)
                               (m/$lin
                                 [(shuftup s0 s2 s4) (tup c1- _ d1)
                                  (m/permutation :rand)
                                  (rep 4 (one-of (s-shift 1) (s-shift -1)))])
                               (append (transpose c3))
                               (append (s-shift -1))))))
        (testing "intermediate"
          (is (noon.test/frozen*
                nil
                (noon.score/mk dur4
                               (rup 6 (one-of d4 d3-))
                               (each (tup (chromatic-double-passing :down)
                                          [d6
                                           (chromatic-double-passing :up)])))))
          (is (noon.test/frozen*
                nil
                (let [c-d+ (efn e
                                (if-let [p- (get-in (nh/neibourhood (:pitch e))
                                                    [:down :c])]
                                  (assoc e :pitch p-)
                                  (d1 e)))]
                  (noon.score/mk dur:4 (rep 14 d1) (each (tup c-d+ _)))))))
        (testing "interleaving"
          (is (noon.test/frozen* nil
                                 (noon.score/mk dur4
                                                (interleaved
                                                  (rup 8 d1 :skip-first)
                                                  (rup 8 d1- :skip-first)))))
          (is (noon.test/frozen*
                nil
                (let [up (one-of d1 s1)
                      down (one-of c1- d1- s1-)
                      rand-double-passing (one-of (tup up _ down _)
                                                  (tup down _ up _)
                                                  (tup down up down _)
                                                  (tup up down up _))]
                  (noon.score/mk harmonic-minor
                                 dur4
                                 (interleaved [(nlin 4 (shuftup s0 s1 s2 s3))
                                               (each rand-double-passing)]
                                              [(nlin 4 (shuftup s0 s1 s2 s3)) s2
                                               (each rand-double-passing)])))))
          (is
            (noon.test/frozen*
              nil
              (let [up (one-of d1 s1)
                    down (one-of c1- d1- s1-)
                    rand-double-passing (one-of (tup _ up down _)
                                                (tup _ down up _)
                                                (tup up _ down _)
                                                (tup down _ up _)
                                                (tup down up down _)
                                                (tup up down up _))]
                (noon.score/mk harmonic-minor
                               dur2
                               (randomly-interleaved
                                 [(chan 1) (nlin 4 (shuftup s0 s1 s2 s3))
                                  (each rand-double-passing)]
                                 [(chan 2) (nlin 4 (shuftup s0 s1 s2 s3)) s4-
                                  (each rand-double-passing)]
                                 [(chan 3) (nlin 4 (shuftup s0 s1 s2 s3)) s4
                                  (each rand-double-passing)]))))))
        (testing "experience 1"
          (is (noon.test/frozen* nil (noon.score/mk (rep 6 s1) (m/connect 1))))
          (is (noon.test/frozen* nil
                                 (noon.score/mk o1 (rep 6 s1-) (m/connect 1))))
          (is (noon.test/frozen* nil
                                 (noon.score/mk (lin s0 s2 s4)
                                                (connect-with d1))))
          (is (noon.test/frozen* nil
                                 (noon.score/mk (lin s0 s2 s4)
                                                (lin s0 s1 s2)
                                                (connect-with (tup d1- d1)))))
          (is (noon.test/frozen* nil
                                 (noon.score/mk [aeolian dur:2]
                                                (lin s0 s2 s4)
                                                (lin s0 s1 s2)
                                                (connect-with (shuflin d1
                                                                       c1-)))))
          (is (noon.test/frozen* nil
                                 (noon.score/mk harmonic-minor
                                                (lin I VII)
                                                (nlin> 3 (transpose c3))
                                                (h/align-contexts :s)
                                                (dup 2)
                                                (each (ntup> 6 s1))
                                                (connect-with d1))))
          (is (noon.test/frozen*
                nil
                (noon.score/mk
                  [harmonic-minor (lin I VII) (nlin> 3 (transpose c3))
                   (h/align-contexts :s) (dup 4)]
                  (par [(vsl :flute-1 :staccato) o1 vel4
                        (each (shuftup s0 s2 s4))
                        (connect-with (one-of d1- d1))]
                       [(vsl :solo-double-bass :pizzicato) o1- t-round]
                       [(vsl :chamber-violas :pizzicato) vel5
                        (each (one-of (tup s1 (par s2 s3) vel0)
                                      (tup vel0 s1 (par s2 s3))))])))))
        (testing "polarity"
          (is (noon.test/frozen*
                nil
                (noon.score/mk
                  phrygian
                  (lin I I VII I VII I VII VII)
                  (mixlin s0 s2)
                  (each (chans [(patch :acoustic-bass) o2- (maybe t-round)]
                               [(patch :ocarina) s2 (shuftup s0 s2 s4)]))
                  (lin _ [rev (transpose c3-)])
                  (parts (chan 1)
                         (connect-with (one-of (one-of d1 d1-)
                                               (shuflin (one-of s1 s1-)
                                                        (one-of d1 d1-))))
                         (chan 0)
                         (each (probs {(tup (one-of s1 s1-) _) 1, _ 4})))))))
        (testing "degree moves"
          (is (noon.test/frozen* nil
                                 (noon.score/mk dorian
                                                (nlin> 8 s1)
                                                [(patch :ocarina)
                                                 (connect-with (degree -1))])))
          (is (noon.test/frozen* nil
                                 (noon.score/mk dorian
                                                dur4
                                                o1
                                                (lin _ (nlin> 3 s1-))
                                                [(patch :ocarina)
                                                 (connect-with (degree 1))]
                                                (each (tup s0 s2))
                                                (connect-with (degree 1)))))
          (is
            (noon.test/frozen*
              nil
              (let [pol+ {:polarity 0}
                    pol- {:polarity 1}
                    invert-pol (each {:polarity (fn [x]
                                                  (case x
                                                    0 1
                                                    1 0))})]
                (noon.score/mk
                  lydianb7
                  dur2
                  (lin pol+ pol-)
                  (lin _ invert-pol)
                  (tup _ invert-pol)
                  (rep 4 (transpose c3-))
                  (h/align-contexts :s)
                  (dup 2)
                  (parts pol+ _ pol- (each (one-of (degree -1) (degree 1))))
                  (chans [(patch :ocarina)
                          (each [(one-of s0 s1) (shuftup s0 s1 s2 s3)])
                          (connect-with (one-of d1 d1-))]
                         [(patch :acoustic-bass) o1-
                          (each (one-of s0 s1- s2-))])))))
          (is
            (noon.test/frozen*
              nil
              (let [pol+ {:polarity 0}
                    pol- {:polarity 1}
                    invert-pol (each {:polarity (fn [x]
                                                  (case x
                                                    0 1
                                                    1 0))})]
                (noon.score/mk
                  (chans [(patch :ocarina) s2- (ntup> 7 s1)
                          (shuftup [_ (connect-with d1)]
                                   [rev s1- (connect-with d1-)]) (dupt 16)]
                         [(patch :acoustic-bass) (dupt 64) o2- t-round
                          (each (maybe s2- s2))])
                  (h/grid [phrygian3 (tup pol+ pol-) (tup _ invert-pol)
                           (tup _ invert-pol) (rup 4 (transpose c3-))
                           (h/align-contexts :s) (dupt 2)
                           (parts pol+ _ pol- (each (degree -1)))])
                  (adjust {:duration 64}))))))
        (testing "scanning"
          (is (noon.test/frozen* nil
                                 (noon.score/mk (patch :electric-piano-1)
                                                aeolian
                                                (nlin> 6 s1)
                                                (each (tup _ c1- [s1 c1-] _)))))
          (is (noon.test/frozen* nil
                                 (noon.score/mk
                                   (patch :electric-piano-1)
                                   dur2
                                   aeolian
                                   (nlin> 4 s1)
                                   (each (tup _ [s2 c1-] c1- _ s2 [s1 d1])))))
          (is
            (noon.test/frozen*
              nil
              (noon.score/mk
                (patch :electric-piano-1)
                dur2
                aeolian
                (nlin> 4 s3)
                (scan :position 2
                      1 (fn [[a b]]
                          (let [start (first a)
                                {target-pitch :pitch} (first b)]
                            (update-score
                              #{start}
                              (each (tup _
                                         [s2 c1-]
                                         c1-
                                         _
                                         s2
                                         [(ef_ (assoc _ :pitch target-pitch))
                                          d1])))))))))
          (is
            (noon.test/frozen*
              nil
              (noon.score/mk (patch :electric-piano-1)
                             aeolian
                             (nlin> 8 [(degree 4) s1-])
                             (scan :position 2
                                   1 (fn [[a b]]
                                       (let [start (first a)
                                             {target-pitch :pitch} (first b)]
                                         (update-score
                                           #{start}
                                           (in-place
                                             (tup _
                                                  [s2 c1-]
                                                  c1-
                                                  _
                                                  s2
                                                  [(ef_ (assoc _
                                                          :pitch target-pitch))
                                                   d1])))))))))
          (is (noon.test/frozen* nil
                                 (noon.score/mk (nlin> 8 d1)
                                                (only-between 4 6 o1))))
          (is (noon.test/frozen* nil
                                 (noon.score/mk (nlin> 8 d1)
                                                (scan> 4 3 (tup _ d1 d1-)))))))
      (testing "Canon"
        (is (noon.test/frozen* nil (noon.score/mk (shuftup s0 s1 s2))))
        (is (noon.test/frozen* nil
                               (noon.score/mk (shuftup s0 s1 s2)
                                              (m/connect 1))))
        (is
          (noon.test/frozen* nil
                             (noon.score/mk
                               dur2
                               (lin (shuftup s0 s1 s2 s3)
                                    [(one-of s1 s1-) (shuftup s0 s1 s2 s3)])
                               decorate
                               (lin _ (s-shift 1) (s-shift -1) _)
                               (lin _ (s-shift 2))
                               (chans [(patch :ocarina) o1 (s-shift -1)]
                                      [(sf_ (shift-score _ 2))]
                                      [(patch :acoustic-bass) o2- (s-shift 1)
                                       (sf_ (shift-score _ 5))])
                               (h/grid dur2
                                       harmonic-minor
                                       (lin I
                                            IV
                                            VII
                                            I
                                            [IV melodic-minor VII]
                                            IV
                                            [V harmonic-minor VII]
                                            VII)
                                       (dup 4)
                                       (h/align-contexts :s)))))))
    (testing "Concepts and techniques"
      (testing "Barry Harris"
        (is (noon.test/frozen* nil
                               (noon.score/mk barry-harris
                                              (tup d0 d3 d4 d7)
                                              (tup d0 d2)
                                              (rep 4 d1))))
        (is (noon.test/frozen*
              nil
              (let [chord-tones [d0 d2 d4 d7]]
                (noon.score/mk
                  barry-harris
                  (lin d0 d3)
                  (rep 8 (one-of d1- d1))
                  (each
                    [(chans [(patch :pad-1-new-age) o1- vel3 (par* chord-tones)]
                            [(patch :ocarina) vel4 (shuftup* chord-tones)
                             (each (maybe (tup (one-of d1 d1-) d0)))]
                            [(patch :vibraphone) vel5 o1
                             (ntup 6
                                   [(one-of* chord-tones) (maybe o1)
                                    (maybe (tup d1- d0))])]) (maybe rev)])))))
        (is (noon.test/frozen*
              nil
              (noon.score/mk barry-harris2
                             (lin I VI VII IV)
                             (h/align-contexts :d)
                             (each (chans [(patch :brass) (par s0 s1 s2 s3)]
                                          [(patch :acoustic-bass) o1- t-round]
                                          [(patch :ethnic) o1
                                           (shuftup s0 s1 s2 s3 s4 s5 s6)]))
                             (rep 2 s1)
                             (append (transpose c3)))))
        (is (noon.test/frozen* nil
                               (noon.score/mk barry-harris2
                                              (lin IV I)
                                              (h/align-contexts :d)
                                              (each (par s0 s1 s2 s3))
                                              (rep 4 (transpose c3))
                                              h/voice-led))))
      (testing "Symetric modes"
        (is (noon.test/frozen*
              nil
              (noon.score/mk (symetric-modes :augm-half)
                             (:two {:one (rup 8 (one-of d1 d1- d2 d2- d3 d3-)),
                                    :two (shuftup d1 d2 d3 d4 d5 d6 d7)})
                             (patch :electric-piano-1)
                             (rep 32
                                  (one-of (each d3)
                                          (each d3-)
                                          (m/rotation 1/2)
                                          (m/permutation :rand {:grade 2})
                                          (m/contour :similar
                                                     {:delta 0, :layer :d}))))))
        (is (noon.test/frozen* nil
                               (noon.score/mk
                                 (symetric-modes :half-whole)
                                 (rand-structure 3)
                                 (rep 3 rand-degree)
                                 (each (chans
                                         [vel4 h/simple-chord]
                                         [(patch :music-box) o1 (rand-tup 6)
                                          (each (one-of vel0 vel4 vel6 vel7))]))
                                 (append [rev s2])
                                 (append (transpose c5))
                                 (append (between 0 1/3))))))
      (testing "Arvo Part"
        (is
          (noon.test/frozen*
            nil
            (let [m-line (fn [size]
                           (pr/rand-nth (vals {:up-to [(rep size d1-) rev],
                                               :up-from (rep size d1),
                                               :down-to [(rep size d1) rev],
                                               :down-from (rep size d1-)})))
                  base (pr/shuffle (map vector
                                     [s0 s1 s2 (one-of s0 s1 s2)]
                                     (map m-line
                                       (pr/shuffle
                                         (pr/rand-nth
                                           (u/sums 12 4 [2 3 4 5]))))))]
              (noon.score/mk lydianb7
                             (lin* base)
                             (each (chans [(patch :piccolo) vel6 o1]
                                          [(patch :flute) vel3 o1 d5-]
                                          [(patch :accordion) vel4 d0]
                                          [(patch :choir-aahs) s-floor
                                           (vel-humanize 7 [40 80])]
                                          [(patch :choir-aahs) s-floor o1 s1
                                           (vel-humanize 7 [40 80])]
                                          [(patch :acoustic-bass) C-2 t-floor]))
                             m/connect-repetitions
                             (append [rev (transpose c3-)])
                             (append dorian)
                             (dup 2))))))
      (testing "Bartok harmony axis"
        (is (noon.test/frozen*
              nil
              (let [L- (transpose c5)
                    L+ (transpose c5-)
                    R- (transpose c3)
                    R+ (transpose c3-)
                    M (transpose c6)]
                (noon.score/mk
                  (rep 8
                       [(one-of L- L+) (maybe R- R+ M) (one-of ionian aeolian)])
                  (h/align-contexts :d)
                  (chans [(patch :aahs) (each (par s0 s1 s2))]
                         [(patch :ocarina) o1
                          (each (shuftup s2- s1- s0 s1 s2 s3))]
                         [(patch :acoustic-bass) o1- t-round (maybe s1 s1-)])
                  (lin _ s1 s1- _)))))
        (is
          (noon.test/frozen*
            nil
            (let [L- (transpose c5)
                  L+ (transpose c5-)
                  R- (transpose c3)
                  R+ (transpose c3-)
                  M (transpose c6)
                  tup1 (mixtup s2- s1- s0 s1 s2 s3)
                  tup2 (mixtup s2- s1- s0 s1 s2 s3)]
              (noon.score/mk
                (rep 8
                     [(one-of L- L+) (maybe R- R+ M) (one-of ionian aeolian)
                      (maybe dur2 dur:2)])
                (h/align-contexts :d)
                (chans [(patch :aahs) (each [add2 (par s0 s1 s2 s3)])
                        m/connect-repetitions]
                       [(patch :ocarina) o1 add2
                        (each [(one-of tup1 tup2) (maybe rev)])]
                       [(patch :acoustic-bass) o1- t-round (maybe s1 s1-)])
                (lin _ s1 s1- _)))))
        (is
          (noon.test/frozen*
            nil
            (let [L- (transpose c5)
                  _L+ (transpose c5-)
                  R- (transpose c3)
                  R+ (transpose c3-)
                  M (transpose c6)
                  base [(pr/rand-nth [R- R+ M]) (pr/rand-nth [ionian aeolian])]
                  rand-color [(maybe R- R+ M) (one-of ionian aeolian)]
                  tup1 (mixtup s2- s1- s0 s1 s2 s3)
                  tup2 (mixtup s2- s1- s0 s1 s2 s3)]
              (noon.score/mk
                base
                (lin _ [L- rand-color] rand-color [L- rand-color] _)
                (lin _ M rev)
                (h/align-contexts :d)
                (chans [(patch :aahs) (each [add2 (par s0 s1 s2 s3)])
                        m/connect-repetitions]
                       [(patch :ocarina) o1 add2
                        (each [(one-of tup1 tup2) (maybe rev)])]
                       [(patch :acoustic-bass) o1- t-round (maybe s1 s1-)])
                (lin _ s1 [rev s1-] _)))))
        (is
          (noon.test/frozen*
            nil
            (let [initial [{:harmonic-coords [0 0]} melodic-minor sixth]
                  up [{:harmonic-coords (fn [[x y]] [x (mod (inc y) 3)])}
                      (transpose c5)]
                  down [{:harmonic-coords (fn [[x y]] [x (mod (dec y) 3)])}
                        (transpose c5-)]
                  left [{:harmonic-coords (fn [[x y]] [(mod (dec x) 4) y])}
                        (transpose c3)]
                  right [{:harmonic-coords (fn [[x y]] [(mod (inc x) 4) y])}
                         (transpose c3-)]]
              (noon.score/mk initial
                             (lin> _ up left down)
                             (lin _ up)
                             (lin _ [rev left])
                             (lin _ [right right])
                             (h/align-contexts :d)
                             (chans [(patch :aahs) (structure [1 2 5 6])
                                     (each (par s0 s1 s2 s3))]
                                    (let [tup1 (mixtup s2- s1- s0 s1 s2 s3)
                                          tup2 (mixtup s2- s1- s0 s1 s2 s3)]
                                      [(patch :ocarina) o1 add2
                                       (each [(one-of tup1 tup2) (maybe rev)])])
                                    [(patch :acoustic-bass) o1- t-round
                                     (maybe s1 s1- s2-)])
                             (lin _ s1 [up s1-] up)))))
        (is
          (noon.test/frozen*
            nil
            (let [initial [lydian seventh]
                  up (transpose c5)
                  down (transpose c5-)
                  left (transpose c3)
                  right (transpose c3-)]
              (noon.score/mk
                [initial (lin> _ up left down)
                 (each (maybe (degree 2) (degree -2))) (lin _ up)
                 (lin _ [rev left]) (lin _ [right right]) (h/align-contexts :d)]
                (chans [(patch :aahs) (each (par s0 s1 s2 s3))]
                       (let [tup1 [(structure [2 3 4 6])
                                   (mixtup s3- s2- s1- s0 s1 s2 s3 s4)]
                             tup2 (mixtup d3- d2- d1- d0 d1 d2 d3 d4)]
                         [(patch :ocarina) o1
                          (each [(one-of tup1 tup2) (maybe rev)])])
                       [(patch :acoustic-bass) o2- t-round
                        (each (probs {_ 3,
                                      (one-of s1- s2) 3,
                                      (tup _ (one-of s1- s2)) 1,
                                      (tup (one-of s1- s2) _) 1}))])
                (lin _ s1 [up s1-] up)
                (options :bpm 40 :xml true))))))
      (testing "Simple counterpoint"
        (is
          (noon.test/frozen*
            nil
            (let [perms (comb/permutations [0 1 2 3])
                  complementary-map
                    (reduce (fn [acc p]
                              (assoc acc
                                p (filter (fn [p']
                                            (every? (fn [[a b]]
                                                      (not= (mod a 3)
                                                            (mod b 3)))
                                                    (map vector p p')))
                                    perms)))
                      {}
                      perms)
                  [base complements] (pr/rand-nth (seq complementary-map))
                  voice1 (pr/rand-nth complements)
                  voice2 (map (fn [a b]
                                (first (filter (complement
                                                 (set (map (fn* [%1] (mod %1 3))
                                                        [a b])))
                                         [0 1 2])))
                           base
                           voice1)]
              (noon.score/mk
                (patch :electric-piano-1)
                (chans (lin* (map s-step base))
                       [o1- (lin* (map s-step voice1))]
                       [o1 (lin* (map s-step voice2))])
                [aeolian (lin _ (degree -1)) (lin _ s1) (lin _ [(degree 3) s1-])
                 (lin _ (transpose c3-))]
                ($by :channel (connect-with (probs {void 5, d1 1, d1- 1})))))))
        (is (noon.test/frozen*
              nil
              (let [[v1 v2 v3] (->> (complementarity-tree 3 3)
                                    (leaves-paths)
                                    (filter (fn* [%1] (= 3 (count %1))))
                                    (pr/rand-nth))]
                (noon.score/mk
                  [dur3 aeolian (lin _ (degree -1)) (lin _ s1)
                   (lin _ [(degree 3) s1-]) (lin _ [s1 (transpose c3-)])]
                  (patch :electric-piano-1)
                  (each (! (let [[v1 v2 v3] (pr/shuffle [v1 v2 v3])]
                             (chans (tup* (map s-step v1))
                                    [o1- (tup* (map s-step v2))]
                                    [o1 (tup* (map s-step v3))]))))
                  ($by :channel
                       (connect-with (probs {void 5, d1 1, d1- 1})))))))
        (is
          (noon.test/frozen*
            nil
            (let [[[r1 r2 r3] [l1 l2 l3]] (->> (complementarity-tree 3 3)
                                               (leaves-paths)
                                               (filter (fn* [%1]
                                                         (= 3 (count %1))))
                                               (pr/shuffle))
                  f (fn [r l]
                      (tup* (map (fn [r l] [(s-step l)
                                            (case r
                                              0 _
                                              1 (tup [dur2 _] (one-of d1 d1-) _)
                                              2 (one-of (tup _ d1 d1- _)
                                                        (tup _ d1- d1 _)))])
                              r
                              l)))]
              (mk [dur3 aeolian (lin _ (degree -1)) (lin _ s1)
                   (lin _ [(degree 3) s1-]) (lin _ [s1 (transpose c3-)])]
                  (each (! (let [[a b c] (pr/shuffle [(f r1 l1) (f r2 l2)
                                                      (f r3 l3)])]
                             (chans [(vsl :solo-violin-1 :pizzicato) o1 b]
                                    [(vsl :solo-viola :pizzicato) c]
                                    [(vsl :solo-cello-1 :pizzicato) o1-
                                     a]))))))))
        (is
          (noon.test/frozen*
            nil
            (let [[arpegios ornamentations harmonic-sequences]
                    (->> (complementarity-tree 3 3)
                         (leaves-paths)
                         (filter (fn* [%1] (= 3 (count %1))))
                         (pr/shuffle))
                  choices {:harmony {0 _,
                                     1 [(degree 3) (s-shift -1)],
                                     2 [(degree 4) (s-shift -2)]},
                           :arpegio {0 s0, 1 s1, 2 s2},
                           :ornamentation {0 void, 1 d1, 2 d1-},
                           :instruments
                             {0 [vel8 (vsl :chamber-violins-1 :legato) o1],
                              1 [vel7 (vsl :chamber-violas :legato)],
                              2 [vel6 (vsl :chamber-cellos :legato) o1-]}}
                  degrees (mapcat (fn [s] (map (choices :harmony) s))
                            harmonic-sequences)
                  lines (map (fn [offset]
                               [(get-in choices [:instruments offset])
                                (lin* (map (fn [d a c]
                                             [d
                                              (tup* (map (fn [step orn]
                                                           [(get-in choices
                                                                    [:arpegio
                                                                     step])
                                                            {:connection orn}])
                                                      a
                                                      c))])
                                        degrees
                                        (drop offset (cycle arpegios))
                                        (drop offset (cycle ornamentations))))])
                          (range 3))]
              (mk dur8
                  harmonic-minor
                  (par* lines)
                  (lin _ (transpose c3-))
                  ($by :channel
                       (connect-with (sf_ (->> (get-in choices
                                                       [:ornamentation
                                                        (:connection (first
                                                                       _))])
                                               (update-score _)))))))))))
    (testing "Tunes"
      (testing "Autumn leaves"
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              {:title "Autumn Leaves"}
              vel3
              [tetrad
               (lin II
                    V
                    I
                    IV
                    VII
                    [III phrygian3]
                    [VI (lin [melodic-minor sixth] phrygian3)])
               (h/align-contexts :s) (dup 2)]
              (h/grid-zipped
                (nlin 16
                      (chans [(patch :acoustic-bass) o1- t-round]
                             [(patch :vibraphone) (par s0 s1 s2 s3)]
                             [(patch :electric-piano-1) vel2 o2 (par s0 s2 s4)
                              (shuftup s0 s2)]
                             [(patch :whistle) o1 vel5
                              (each [(shuftup s0 s1 s2 s3)
                                     (tup same (one-of s1 s1- s2 s2-))])])))))))
      (testing "Giant steps (John Coltrane)"
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              vel3
              (h/harmonic-zip
                [GIANT_STEPS (dupt 2)]
                (chans [(patch :acoustic-bass) o2- (each t-round)]
                       [(patch :electric-piano-1) (each (par s0 s1 s2 s3))]
                       [(patch :ocarina) vel5
                        (each (parts {:degree :II}
                                     (structure [0 3 4 6])
                                     {:degree :V}
                                     (structure [1 2 5 6])
                                     {:degree :I}
                                     (structure :tetrad)))
                        (ntup 32
                              [(one-of o1 o2) (! (rup (pr/rand-nth [5 6 7]) s1))
                               (tup (maybe (m/permutation 1/4))
                                    [(maybe rev) (one-of s1 s2 s2- s1-)])])]))
              m/connect-repetitions
              (adjust 32)))))
      (testing "ESP (Wayne Shorter)"
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              {:title "ESP", :composer "Wayne Shorter"}
              (h/harmonic-zip
                [tetrad
                 (tup [VII superlocrian dur2]
                      [I lydian dur2]
                      [VII superlocrian dur2]
                      [VIIb lydian dur2]
                      [VI superlocrian]
                      [VIIb lydian]
                      [VII superlocrian]
                      (tup [I lydian] [VIIb lydianb7])
                      [VI dorian]
                      [II lydianb7]
                      [II dorian]
                      [IIb lydianb7]) (h/align-contexts :s) (dupt 2)]
                [vel4
                 (chans [(patch :acoustic-bass) o2- t-round]
                        [(patch :electric-piano-1) vel3 o1-
                         (par> d0 d3 d3 d3 d3)]
                        [(patch :flute) vel6
                         (fill> (/ 1 (* 2 32 6))
                                (any-that (within-pitch-bounds? :C0 :C3)
                                          d4-
                                          d3-
                                          d1-
                                          d1
                                          d3
                                          d4))])])
              (adjust 32)
              (dup 2))))
        (is (noon.test/frozen*
              nil
              (noon.score/mk
                (h/harmonic-zip
                  [ESP_fullgrid (dupt 2) (h/align-contexts :s)]
                  (chans [(patch :electric-piano-1) o1- vel3
                          (voices> d0 d3 d3 d3 d3)]
                         [(patch :acoustic-bass) vel2 C-2 t-round]
                         [(patch :flute)
                          (fill> (/ 1 (* 6 64))
                                 (maybe (any-that* (within-pitch-bounds? :G-1
                                                                         :C2)
                                                   [d4- d3- d1- d1 d3 d4])))
                          (each (probs {void 1, same 5})) m/connect-repetitions
                          (vel-humanize 10 [30 70])]))
                (adjust 48)))))
      (testing "Cyclic episode (Sam Rivers)"
        (is
          (noon.test/frozen*
            nil
            (let [n-bars (* 4 16)
                  bass [(patch :acoustic-bass) (each t2-)]
                  vibe [(patch :vibraphone) vel5 t1 (each (par s0 s1 s2 s3))
                        h/voice-led]
                  lead1 (ntup> (* n-bars 12)
                               (any-that (within-pitch-bounds? :C0 :C3)
                                         d1
                                         d1-
                                         d3
                                         d3-
                                         d4
                                         d4-))
                  lead2 [(repeat-while
                           (within-time-bounds? 0 (* n-bars 10))
                           (append [start-from-last
                                    (any-that (within-pitch-bounds? :C-1 :C2)
                                              (rep 3 d3 :skip-first)
                                              (rep 3 d3- :skip-first)
                                              d1
                                              d1-)])) (adjust 1)]
                  lead4 [(tup (mixtup s0 s1 s2 s3) (mixtup s2 s3 s4 s5))
                         (rup n-bars
                              (probs {(m/permutation [0 1/2]) 2,
                                      (m/rotation :rand) 3,
                                      rev 1,
                                      (any-that* (within-pitch-bounds? :C0 :C3)
                                                 (map s-step (range -2 3)))
                                        5}))]]
              (noon.score/mk CYCLIC_EPISODE
                             (chans bass
                                    vibe
                                    [(h/grid-zipped lead4)
                                     (chans [(patch :flute) vel8 s2]
                                            [(patch :electric-piano-1) vel5])
                                     (each (probs {vel0 1, same 2}))])
                             (vel-humanize 0.15)
                             (adjust 64))))))
      (testing "Inner urge (Joe Henderson)"
        (is
          (noon.test/frozen*
            nil
            (let [n-bars 24
                  choir [(patch :choir-aahs) vel5 (par> d3 d3 d3)]
                  bass [(patch :acoustic-bass) C-2 t-round]
                  lead-line (any-that (within-pitch-bounds? :C0 :C3)
                                      (rep 2 d3 :skip-first)
                                      (rep 2 d3- :skip-first)
                                      d4
                                      d4-
                                      d1
                                      d1-
                                      (rep 3 d2 :skip-first)
                                      (rep 3 d2- :skip-first))]
              (noon.score/mk
                (h/harmonic-zip
                  [(tup (lin (nlin 4 [(root :F#) locrian2])
                             (nlin 4 [(root :F) lydian])
                             (nlin 4 [(root :Eb) lydian])
                             (nlin 4 [(root :Db) lydian]))
                        [lydian (lin* (map root [:E :Db :D :B :C :A :Bb :G]))])
                   (h/align-contexts :s) (dupt 4)]
                  (tup (chans choir
                              bass
                              [(patch :music-box) vel5 C1
                               (m/simple-tupline (* n-bars 10) lead-line)])
                       (chans choir
                              bass
                              [(patch :ocarina) vel4 C1
                               (m/simple-tupline (* n-bars 24) lead-line)])
                       (chans choir
                              bass
                              [(patch :sawtooth) (dur (/ 1 n-bars)) vel4 C1
                               (tup d0 d3 d6) (tup d0 d4 d8)
                               (m/line (one-of (last-n-positions 10)
                                               (last-n-positions 7))
                                       (any-that (within-pitch-bounds? :C0 :C3)
                                                 (m/permutation {:grade 3})
                                                 (one-of d1 d1-)
                                                 (one-of d2 d2-))
                                       (sf_ (> (score-duration _) 1))
                                       (trim 0 1)) (vel-humanize 5 [40 80])])
                       (chans [choir (ntup (/ n-bars 2) same)
                               ($by :position
                                    [(! (one-of
                                          (r/gen-tup 8 3 :euclidean)
                                          (r/gen-tup 8 3 :durations [2 3 4 5])))
                                     (sf_ (let [xs (-> (group-by :position _)
                                                       seq
                                                       sort
                                                       vals)]
                                            (reduce into
                                              #{}
                                              (map update-score
                                                xs
                                                (pr/shuffle [d0 d1 d1-])))))])]
                              bass)))
                (adjust 180)))))))
    (testing "snippets"
      (testing "violin fast arpegio"
        (is (noon.test/frozen*
              nil
              (noon.score/mk (dur 3/2)
                             dorian
                             (patch :violin)
                             (lin I IV V I)
                             (h/align-contexts :s)
                             (each (ntup 2 (tup s0 s2 s4 s4 s2 s0)))
                             (each (! (vel (mul (+ 0.9 (* (pr/rand) 0.2))))))
                             (append s1-)))))
      (testing "zip rythmn"
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              lydianb7
              (h/modal-structure 5)
              (chans
                [(patch :vibraphone) (shuflin s0 s1 s2 s3 s4)
                 (nlin 4 (one-of s1 s2 s1- s2-))
                 (sf_ (let [rythmn (mk (nlin 2 (! (r/gen-tup 12 5 :shifted)))
                                       (append rev))]
                        (set (map (fn [r n]
                                    (merge n
                                           (select-keys r
                                                        [:position :duration])))
                               (sort-by :position rythmn)
                               (sort-by :position _)))))]
                [(patch :woodblock) (r/gen-tup 12 5 :euclidean) (dup 4)]
                [(patch :tinkle-bell) (dup 4)]
                [(patch :metallic) (shuflin s0 s1 s2 s3) (each (par s0 s1 s2))]
                [(patch :acoustic-bass) t2- (dup 4)])
              (adjust 8)
              (append [(transpose c3-) s1 rev] _)))))
      (testing "Sparkling waves"
        (is (noon.test/frozen* nil
                               (noon.score/mk
                                 dur:4
                                 vel4
                                 (scale :lydian)
                                 (patch :music-box)
                                 (par s0 s2 s4)
                                 (rep 3
                                      (each [{:mark (pr/rand)} s1
                                             {:velocity (div 1.1),
                                              :duration (mul 1.3)}
                                             (shuftup s2- s0 s2)])
                                      :skip-first)
                                 (lin I [rev III] [o1- V] [rev o1- VII])
                                 (append [rev (transpose c3)])))))
      (testing "Modal chords"
        (is
          (noon.test/frozen*
            nil
            (let [rand-color (fn []
                               (let [k (pr/rand-nth [:lydian+ :lydian :ionian
                                                     :dorian :melodic-minor
                                                     :mixolydian :phrygian6])]
                                 [(scale k) (h/modal-structure 4)]))]
              (noon.score/mk
                dur2
                (lin* (map (comp transpose c-step) (pr/shuffle (range 12))))
                (each (! (rand-color)))
                (h/align-contexts :d :static)
                (chans [(patch :aahs) (each (par s0 s2 s3 s5))]
                       [(patch :vibraphone) o1
                        (each (par s0 s2 s3) (shuftup s0 s3) (tup s0 s1 s1-))
                        ($by :position
                             (probs {vel0 2,
                                     (one-of vel3 vel5 vel7) 8,
                                     [vel3 (ntup> 4 [s1 (vel+ 15)])] 1}))]
                       [(patch :acoustic-bass) o1- t-round])))))
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              (patch :aahs)
              dur4
              (shuflin c0 c1 c2 c3)
              (m/contour :similar {:delta 4, :layer :c})
              (par o1 [c6- (m/contour :mirror {:layer :c})])
              ($by :position
                   (sfn
                     score
                     (let [modal-lvl 1
                           chord-size 4
                           [min-pitch-val max-pitch-val] (h/pitch-values score)
                           interval (mod (- max-pitch-val min-pitch-val) 12)
                           [mode-kw prio] (pr/rand-nth (possible-modes
                                                         interval
                                                         modal-lvl
                                                         (dec chord-size)))
                           partial-scale (cons 0 (take (dec chord-size) prio))
                           structure'
                             (nc/partial-scale->structure mode-kw partial-scale)
                           closed (mk (dissoc (first score) :pitch)
                                      (origin min-pitch-val)
                                      (scale mode-kw)
                                      (structure structure')
                                      (par* (map s-step (range chord-size))))
                           drops (filter (fn [drop]
                                           (= max-pitch-val
                                              (last (h/pitch-values drop))))
                                   (h/drops closed))]
                       (pr/rand-nth drops))))
              ($by :position
                   (chans _
                          [(patch :contrabass) vel3 min-pitch o1-]
                          [max-pitch (patch :ocarina)
                           (mixtup s0 s1- s2- s3- s4- s5-) (tup _ s2- s1)]))
              (lin _ [rev c3])
              (lin _ [rev c3-])
              (options :bpm 30 :xml true :preview true)))))
      (testing "infinite climb"
        (is (noon.test/frozen*
              nil
              (noon.score/mk
                dur6
                dur2
                (patch :ocarina)
                (rup 36 c1)
                (sf_ (set (map-indexed
                            (fn [i n]
                              (let [vel (* 60 2 (/ (inc i) (count _)))
                                    vel (if (> vel 60) (- 60 (- vel 60)) vel)]
                                (assoc n :velocity vel)))
                            (sort-by :position _))))
                (par _ (m/rotation 1/3) (m/rotation 2/3))
                (dup 4)))))
      (testing "melodic development"
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              dorian
              (shuftup d0 d1 d2 d3 d4 d5 d6)
              (repeat-while
                (within-time-bounds? 0 8)
                (append (any-that
                          (within-pitch-bounds? :C0 :C3)
                          [(start-from-nth-last 1) (one-of d1- d1)]
                          [(start-from-nth-last 8) (m/permutation [0 1/4])]
                          [(start-from-nth-last 4) rev]
                          [(start-from-nth-last 4)
                           (m/contour :similar {:extent [-2 2], :layer :d})]))
                (trim 0 8))
              (each (probs {(one-of vel3 vel5 vel7 vel9) 6,
                            (superpose [(chan 2) (patch :vibraphone) vel8
                                        (one-of d3 d4)])
                              1,
                            (superpose [(chan 7) (patch :flute) vel8 o1]) 5}))
              (superpose
                (k (nlin 4 [(chan 5) (patch :acoustic-bass) t2- vel8 dur2])))
              (rep 4
                   (one-of [(d-shift -2) (transpose c3)]
                           [(d-shift 2) (transpose c3-)]
                           [(d-shift 1) (transpose c1-)]
                           [(d-shift -3) (transpose c6)]))
              (append (superpose (k (nlin 4
                                          [(patch :taiko-drum) (chan 3)
                                           (! [vel4 (maybe o1- d1)
                                               (r/gen-tup 7 3)])])
                                    (dup 8))))))))
      (testing "textures 1"
        (is (noon.test/frozen* nil
                               (noon.score/mk dur2
                                              lydian
                                              (patch :flute)
                                              (chans _ d3 d6 d9)
                                              (each [(dupt 24)
                                                     (each
                                                       (one-of vel1 vel3 vel6)
                                                       (probs {_ 6, d1 1}))])
                                              ($by :channel (maybe rev))
                                              (append (transpose c3-))
                                              (append (transpose c1-)))))
        (is (noon.test/frozen* nil
                               (noon.score/mk
                                 dur3
                                 lydian
                                 (chans [(patch :marimba) (lin _ c1)]
                                        [(patch :vibraphone) (lin d3 d2)]
                                        [(patch :celesta) (lin d6 d6)]
                                        [(patch :orchestral-harp) (lin d9 d9)])
                                 (append (transpose c2-))
                                 (dup 2)
                                 (each [(dupt 34)
                                        (each (one-of vel0 vel3 vel6 vel9)
                                              (probs {_ 4, o1 1}))]))))
        (is (noon.test/frozen*
              nil
              (noon.score/mk dur8
                             o2
                             (dupt 128)
                             (each (par> d4 d4 d4)
                                   (one-of vel0 vel1 vel2 vel3 vel4 vel5))))))
      (testing "Bach prelude Cm melodic pattern"
        (is (noon.test/frozen*
              nil
              (noon.score/mk
                harmonic-minor
                (m/$lin (lin I IV I V))
                (h/align-contexts :s)
                (lin _ s1)
                (each (chans
                        (tup s2 [s1 (lin _ d1- _)] s0 [s1 (lin _ d1- _)])
                        (tup s3- [s2- (lin _ d1 _)] s1- [s2- (lin _ d1 _)])))
                (lin _ [(transpose c3) rev])
                (dup 2))))
        (is (noon.test/frozen*
              nil
              (noon.score/mk
                harmonic-minor
                (m/$lin (lin I IV I V))
                (h/align-contexts :s)
                (lin _ s1)
                (let [pat1 (tup s2 [s1 (lin _ d1- _)] s0 [s1 (lin _ d1- _)])
                      pat2 [pat1 (m/contour :mirror {:layer :s})]]
                  (each (chans [o1 pat1] [s1- pat2]))))))
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk harmonic-minor
                           dur2
                           (lin _ (transpose c3) _)
                           (m/$lin (lin I IV I V))
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
                           (dup 2)))))
      (testing "Gradual melodic transformation"
        (is (noon.test/frozen*
              nil
              (noon.score/mk
                (chans [(patch :vibraphone) vel3
                        (ntup 4
                              [(one-of IV II VI) tetrad
                               (par [t2- vel5] s0 s1 s2 s3)])]
                       [(patch :ocarina) vel5 (shuftup d1 d2 d3 d4 d5)
                        (each (maybe (par d0 d3)))
                        (rup 16
                             (probs {(m/permutation :rand) 1,
                                     (m/rotation :rand) 3,
                                     (one-of* (map d-step (range -3 4))) 5}))])
                (adjust 10)
                (append [d2- (transpose c3)] [d2 (transpose c3-)] same))))))
    (testing "Usage"
      (testing "=noon.lib.rythmn/bintup="
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              dur6
              (lin [I dorian] [III mixolydian] [VIb lydian] [I lydian])
              (append> (transpose c1-) (transpose c1-) (transpose c1-))
              (dup 2)
              (h/align-contexts)
              (each
                (chans
                  [(patch :new-age) vel3 o1-
                   (par s0 s1 s2 s3 [o1 (par> d3 d3 d3 d3)])]
                  [(patch :taiko-drum) (r/gen-tup 9 3 :durations [2 3 4])
                   (each (one-of vel4 vel3) (maybe d3 d3-))]
                  [(patch :acoustic-bass) t-floor o1-
                   (r/gen-bintup 9 4 :euclidean :shifted) vel4
                   (vel-humanize 1/5)
                   (parts {:bintup 0}
                          (each (vel+ 20) (one-of s0 s1))
                          {:bintup 1}
                          (each (probs {vel0 2, (one-of d3- d4) 1})))]
                  [(r/gen-bintup 54 11 :shifted :euclidean)
                   (parts
                     {:bintup 0}
                     [(patch :electric-piano-1) sus4
                      (each vel3
                            (vel-humanize 1/10)
                            (one-of d2 d4 d6)
                            (probs {_ 3,
                                    [(one-of s0 s1 s2) (par s0 s1 s2)] 1}))]
                     {:bintup 1}
                     [(patch :marimba) vel4 (vel-humanize 1/5) (chan+ 1)
                      (each [(one-of d3 d5 d7) (maybe o1 (par _ d4))])])]))))))
      (testing "=noon.lib.harmony/grid="
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk dur3
                           (nlin> 48 (one-of d1 d1-))
                           (each (chans [(patch :aahs) vel5 (par s0 s1 s2 s3)]
                                        [(patch :ocarina) (shuftup s0 s2 s4 s6)
                                         (shuftup d0 d3 d6) (tup _ rev)]
                                        [(patch :acoustic-bass) t2-]))
                           (h/grid dur3
                                   tetrad
                                   (lin [I lydian (structure [2 3 5 6])]
                                        [IIb dorian (structure [1 2 3 6])]
                                        [V mixolydian (structure [2 3 5 6])]
                                        [Vb melodic-minor
                                         (structure [1 2 5 6])])
                                   (rep 6 (transpose c2-))
                                   (dup 2)
                                   (h/align-contexts :d :static)))))
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              (ntup> 24 (one-of d1 d1-))
              (each (chans [(patch :aahs) vel5 (par s0 s1 s2 s3)]
                           [(patch :ocarina)
                            (one-of (mixtup s0 s2 s4 s6) (mixtup s0 s2 s4 s6))
                            (one-of (mixtup d0 d3 d6) (mixtup d0 d3 d6))
                            (vel-humanize 10 [40 80]) (tup _ rev)]
                           [(patch :acoustic-bass) t2-]))
              (h/grid
                tetrad
                (tup [I lydian] [IIb dorian] [V mixolydian] [Vb melodic-minor])
                (each (h/modal-structure 4))
                (rup 4 (transpose c2-))
                (dupt 2)
                (h/align-contexts :d :static))
              (adjust 60))))
        (is (noon.test/frozen*
              nil
              (noon.score/mk (chans [(patch :aahs) vel5 (par s0 s1 s2 s3)]
                                    [(patch :acoustic-bass) t2-])
                             (h/grid (lin [I lydian (structure [2 3 5 6])]
                                          [IIb dorian (structure [1 2 3 6])]
                                          [V mixolydian (structure [2 3 5 6])]
                                          [Vb melodic-minor
                                           (structure [1 2 5 6])])
                                     (rep 2 (transpose c2-))
                                     (dup 2)
                                     (h/align-contexts :d :static)
                                     (adjust 1))
                             (parts (patch :acoustic-bass)
                                    (each (tup (maybe o1) (one-of d4 d3-))))
                             (adjust 32))))
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              (chans
                [(patch :aahs) vel6
                 (rup 24
                      (any-that (within-pitch-bounds? :G-1 :G1) s2 s2- s3 s3-))
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
              (adjust 32))))
        (is
          (noon.test/frozen*
            nil
            (noon.score/mk
              (rup
                128
                (any-that (within-pitch-bounds? :C1 :C3) s1 s2 s3 s1- s2- s3-))
              (chans (each (probs {_ 2, vel0 1, (shuftup s1- s0 s1 s2) 1}))
                     (each s1- (probs {_ 2, vel0 1, (shuftup s1- s0 s1) 1}))
                     (each [s2- o1- (probs {_ 2, (shuftup s0 s2) 1})]))
              (h/grid harmonic-minor
                      (tup I
                           IV
                           VII
                           I
                           [IV melodic-minor VII]
                           IV
                           [V harmonic-minor VII]
                           VII)
                      (dupt 4)
                      (h/align-contexts :s))
              (adjust {:duration 64})))))
      (testing "Vienna symphonic library"
        (is (noon.test/frozen*
              nil
              (mk (par [(vsl :chamber-violins-1 :detache)
                        (lin s0
                             [(vsl/patch :legato) (tup s1 s2 s3)]
                             [(vsl/patch :pizzicato)
                              (par [(vsl/patch :snap-pizzicato) _]
                                   [(vsl :solo-double-bass :pizzicato) o2-
                                    (tup s2 s1)])])]
                       [(vsl :flute-1 :portato) o1 s-
                        (lin s0 [(vsl/patch :legato) (tup s1 s2 s3)])])
                  (lin s0 s2 s1-)
                  (dup 4))))
        (is (noon.test/frozen* nil
                               (mk vel10
                                   (vsl/instrument :chamber-cellos)
                                   (vsl/patch :pizzicato)
                                   o1-
                                   (shuftup d0 d3 d6)
                                   (shuftup d0 d3 d6)
                                   (dup 8))))))))