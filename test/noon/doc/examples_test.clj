(ns
 noon.doc.examples-test
 "This file is generated from `src/noon/doc/examples.org`"
 (:use noon.score)
 (:require
  [noon.lib.harmony :as h]
  [noon.lib.melody :as m]
  [noon.lib.rythmn :as r]
  [noon.utils.pseudo-random :as pr]
  [clojure.test :refer [deftest is]]
  [noon.test]))

(swap! options* assoc :tracks {0 :chorium})

(def
 GIANT_STEPS
 (let
  [II
   [II {:degree :II}]
   V
   [V {:degree :V}]
   I
   [I {:degree :I}]
   t1
   same
   t2
   (transpose c4-)
   t3
   (transpose c4)
   s1
   (lin [t1 I] [t2 (lin V I)] [t3 (lin V [dur2 I])] [t2 (lin II V)])
   II-V-I
   (lin II V [I dur2])]
  [tetrad
   (tup
    s1
    [t2 s1]
    [t3 I dur2]
    [t2 II-V-I]
    II-V-I
    [t3 II-V-I]
    [t1 (lin II V)])
   (h/align-contexts :structural :static)]))

(defn
 fill
 [resolution f]
 (sf_
  (let
   [sdur (score-duration _) n (quot sdur resolution)]
   (assert
    (zero? (rem sdur resolution))
    "fill: resolution should be a multiple of score length ")
   (update-score _ (ntup n f)))))

(defn
 fill>
 [resolution f]
 (sf_
  (let
   [sdur (score-duration _) n (quot sdur resolution)]
   (assert
    (zero? (rem sdur resolution))
    "fill: resolution should be a multiple of score length ")
   (update-score _ (ntup> n f)))))

(def
 ESP_fullgrid
 (let
  [common
   (lin
    [VII superlocrian dur2]
    [I lydian dur2]
    [VII superlocrian dur2]
    [VIIb lydian dur2]
    [VI superlocrian]
    [VIIb lydian]
    [VII superlocrian]
    (tup [I lydian] [VIIb lydianb7]))]
  (tup
   common
   (lin [VI dorian] [II lydianb7] [II dorian] [IIb lydianb7])
   common
   (lin
    [VIb lydianb7]
    [II dorian]
    (tup [VIb dorian] [IIb lydianb7])
    I))))

(def s? (one-of s2- s1- s1 s2))

(def
 CYCLIC_EPISODE
 (let
  [a1
   [dorian (rep 4 (transpose c3))]
   a2
   [dorian (rep 4 (transpose c3-))]
   b
   (lin [IV dorian] [V superlocrian (structure [2 3 5 6])])
   c
   (lin [V mixolydian sus47] [V phrygian sus27])
   d
   [dorian (append (transpose c3))]]
  [tetrad
   (tup
    [(root :Bb) a1]
    [(root :G) b]
    [(root :D) b]
    [(root :D) a2]
    [(root :G) c]
    [(root :Eb) d])
   (dupt 4)
   (h/align-contexts :s :static)]))

(deftest
 main
 (is
  (noon.test/frozen
   dur2
   (lin I IV I V)
   (h/align-contexts :s)
   (each
    (chans
     [(patch :woodblock) C0 (dupt 4)]
     [(patch :tinkle-bell) C0 (r/gen-tup 12 5 {:durations [1 2 3]})]
     [(patch :marimba)
      o1-
      (r/gen-tup 12 5 :euclidean)
      (each (par s0 s2))
      (each (one-of s0 s1 s1-))]
     [(patch :acoustic-bass)
      t2-
      vel10
      (r/gen-tup 12 5 :euclidean :shifted)]
     [vel12
      (patch :music-box)
      o1
      (one-of s0 s1 s1-)
      (shuftup s0 s1 s3)
      (each
       (probs
        {[(par s0 s2) (maybe (tup s0 s1))] 3,
         [(tup s3 s1 (par s2 s0) s1-)] 2,
         [(tup d1- s0 d1 s0) (maybe (m/rotation 2))] 1}))]))
   (dup 2)))
 (is
  (noon.test/frozen
   dur2
   (scale :harmonic-minor)
   (lin I IV VII I)
   (h/align-contexts :s)
   (lin same (transpose c3) same)
   (chans
    [(patch :choir-aahs)
     vel4
     (each [(par s0 s1 s2) (maybe (tup s0 s1-) (tup s0 s1))])]
    [(patch :ocarina)
     vel6
     (each
      [(shuftup s0 s1 s2)
       (each
        (one-of
         (tup s0 (shuflin (one-of c1- s-) s+) s0)
         (tup s0 c1- s0 (one-of s2- s2))))])]
    [(patch :kalimba)
     vel4
     o2
     (each
      [(shuftup s0 s1 s2)
       (each (one-of vel0 (par s0 s2-) (shuftup s0 s1 s2)))])]
    [(patch :acoustic-bass) vel3 o2-])))
 (is
  (noon.test/frozen
   vel3
   (h/harmonic-zip
    [GIANT_STEPS (dupt 2)]
    (chans
     [(patch :acoustic-bass) o2- (each t-round)]
     [(patch :electric-piano-1) (each (par s0 s1 s2 s3))]
     [(patch :ocarina)
      vel5
      (each
       (parts
        {:degree :II}
        (structure [0 3 4 6])
        {:degree :V}
        (structure [1 2 5 6])
        {:degree :I}
        (structure :tetrad)))
      (ntup
       32
       [(one-of o1 o2)
        (! (rup (pr/rand-nth [5 6 7]) s1))
        (tup
         (maybe (m/permutation 1/4))
         [(maybe rev) (one-of s1 s2 s2- s1-)])])]))
   m/connect-repetitions
   (adjust 32)))
 (is
  (noon.test/frozen
   {:title "ESP", :composer "Wayne Shorter"}
   (h/harmonic-zip
    [tetrad
     (tup
      [VII superlocrian dur2]
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
      [IIb lydianb7])
     (h/align-contexts :s)
     (dupt 2)]
    [vel4
     (chans
      [(patch :acoustic-bass) o2- t-round]
      [(patch :electric-piano-1) vel3 o1- (par> d0 d3 d3 d3 d3)]
      [(patch :flute)
       vel6
       (fill>
        (/ 1 (* 2 32 6))
        (any-that
         (within-pitch-bounds? :C0 :C3)
         d4-
         d3-
         d1-
         d1
         d3
         d4))])])
   (adjust 32)
   (dup 2)))
 (is
  (noon.test/frozen
   (h/harmonic-zip
    [ESP_fullgrid (dupt 2) (h/align-contexts :s)]
    (chans
     [(patch :electric-piano-1) o1- vel3 (voices> d0 d3 d3 d3 d3)]
     [(patch :acoustic-bass) vel2 C-2 t-round]
     [(patch :flute)
      (fill>
       (/ 1 (* 6 64))
       (maybe
        (any-that*
         (within-pitch-bounds? :G-1 :C2)
         [d4- d3- d1- d1 d3 d4])))
      (each (probs {void 1, same 5}))
      m/connect-repetitions
      (vel-humanize 10 [30 70])]))
   (adjust 48)))
 (is
  (noon.test/frozen
   {:description "epic lydian sequence by minor thirds"}
   (h/harmonic-zip
    [lydian
     sus47
     (tup* (map root [:C :Eb :F# :A]))
     (dupt 2)
     (h/align-contexts :s)]
    (par
     [(chan 1) (patch :choir-aahs) vel3 (ntup 8 (par s0 s1 s2))]
     [vel4
      (m/simple-tupline
       (* 16 16)
       (any-that
        (within-pitch-bounds? :C-1 :C2)
        (lin s? s?)
        [(shuflin s1 s2 s3 s4) (maybe rev)]
        (lin d1 d1- s0 s?)
        (lin d1- d1 s0 s?)))
      (par
       [(chan 2) (patch :french-horn)]
       [(chan 3) vel5 o2 (patch :flute)])]
     [(chan 4)
      (patch :taiko-drum)
      vel2
      (ntup 16 (lin dur3 [o1 vel4 dur2] dur3))]
     [(chan 5) (patch :acoustic-bass) o2- (ntup 32 t0)]))
   (adjust 32)
   (nlin 4 (s-shift -1))))
 (is
  (noon.test/frozen
   {:description "tritonal chord sequence shifts by minor thirds"}
   (let
    [I
     (one-of
      [lydian+ (structure [2 3 4 5 6])]
      [melodic-minor (structure [1 2 4 5 6])])
     V
     (one-of
      [V mixolydian (structure [1 3 4 5 6])]
      [V phrygian6 (structure [0 1 3 5 6])])
     [B G Eb]
     (map root [:B :G :Eb])]
    [(tup [B V] [B I] [G V] [G I] [Eb V dur2] [Eb I dur2])
     (rup 4 (transpose d2-))
     (h/align-contexts :s :static)
     (chans
      [(patch :choir-aahs) vel3 (each (par s0 s1 s2 s3 s4))]
      [(patch :vibraphone)
       vel5
       (each
        (probs
         {(par s0 s1 s2 s3 s4) 1,
          (shuftup
           [dur2 (par s0 s2 s4)]
           [(one-of dur2 dur3) (par s1- s1 s3)])
          3}))]
      [(patch :acoustic-bass)
       vel5
       (each
        [tetrad o2- t0 (maybe (tup (one-of dur2 dur3) [dur2 o1-]))])]
      [(patch :taiko-drum)
       vel3
       (each (shuftup s0 s1 s2 s3 s4))
       (each
        (probs {vel0 3, same 1, (one-of o1 o1-) 1, (tup t0 t1) 1}))]
      [vel6
       (h/grid-zipped
        [(chans (patch :flute) [o1 (patch :piccolo)])
         (ntup>
          (* 32 10)
          (any-that
           (within-pitch-bounds? :C-2 :C2)
           s1
           s2
           s1-
           s2-
           s3
           s3-))]
        (each
         (probs {vel0 1, same 4, (superpose (one-of s1 s2 s3)) 0})))])
     (adjust 48)])))
 (is
  (noon.test/frozen
   {:title "Autumn Leaves"}
   vel3
   [tetrad
    (lin
     II
     V
     I
     IV
     VII
     [III phrygian3]
     [VI (lin [melodic-minor sixth] phrygian3)])
    (h/align-contexts :s)
    (dup 2)]
   (h/grid-zipped
    (nlin
     16
     (chans
      [(patch :acoustic-bass) o1- t-round]
      [(patch :vibraphone) (par s0 s1 s2 s3)]
      [(patch :electric-piano-1)
       vel2
       o2
       (par s0 s2 s4)
       (shuftup s0 s2)]
      [(patch :whistle)
       o1
       vel5
       (each
        [(shuftup s0 s1 s2 s3)
         (tup same (one-of s1 s1- s2 s2-))])])))))
 (let
  [n-bars
   (* 4 16)
   bass
   [(patch :acoustic-bass) (each t2-)]
   vibe
   [(patch :vibraphone) vel5 t1 (each (par s0 s1 s2 s3)) h/voice-led]
   lead1
   (ntup>
    (* n-bars 12)
    (any-that (within-pitch-bounds? :C0 :C3) d1 d1- d3 d3- d4 d4-))
   lead2
   [(repeat-while
     (within-time-bounds? 0 (* n-bars 10))
     (append
      [start-from-last
       (any-that
        (within-pitch-bounds? :C-1 :C2)
        (rep 3 d3 :skip-first)
        (rep 3 d3- :skip-first)
        d1
        d1-)]))
    (adjust 1)]
   lead4
   [(tup (mixtup s0 s1 s2 s3) (mixtup s2 s3 s4 s5))
    (rup
     n-bars
     (probs
      {(m/permutation [0 1/2]) 2,
       (m/rotation :rand) 3,
       rev 1,
       (any-that*
        (within-pitch-bounds? :C0 :C3)
        (map s-step (range -2 3)))
       5}))]]
  (is
   (noon.test/frozen
    CYCLIC_EPISODE
    (chans
     bass
     vibe
     [(h/grid-zipped lead4)
      (chans [(patch :flute) vel8 s2] [(patch :electric-piano-1) vel5])
      (each (probs {vel0 1, same 2}))])
    (vel-humanize 0.15)
    (adjust 64))))
 (is
  (noon.test/frozen
   (chans
    [(patch :vibraphone)
     vel3
     (ntup 4 [(one-of IV II VI) tetrad (par [t2- vel5] s0 s1 s2 s3)])]
    [(patch :ocarina)
     vel5
     (shuftup d1 d2 d3 d4 d5)
     (each (maybe (par d0 d3)))
     (rup
      16
      (probs
       {(m/permutation :rand) 1,
        (m/rotation :rand) 3,
        (one-of* (map d-step (range -3 4))) 5}))])
   (adjust 10)
   (append [d2- (transpose c3)] [d2 (transpose c3-)] same)))
 (is
  (noon.test/frozen
   dur6
   (lin [I dorian] [III mixolydian] [VIb lydian] [I lydian])
   (append> (transpose c1-) (transpose c1-) (transpose c1-))
   (dup 2)
   (h/align-contexts)
   (each
    (chans
     [(patch :new-age)
      vel3
      o1-
      (par s0 s1 s2 s3 [o1 (par> d3 d3 d3 d3)])]
     [(patch :taiko-drum)
      (r/gen-tup 9 3 :durations [2 3 4])
      (each (one-of vel4 vel3) (maybe d3 d3-))]
     [(patch :acoustic-bass)
      t-floor
      o1-
      (r/gen-bintup 9 4 :euclidean :shifted)
      vel4
      (vel-humanize 1/5)
      (parts
       {:bintup 0}
       (each (vel+ 20) (one-of s0 s1))
       {:bintup 1}
       (each (probs {vel0 2, (one-of d3- d4) 1})))]
     [(r/gen-bintup 54 11 :shifted :euclidean)
      (parts
       {:bintup 0}
       [(patch :electric-piano-1)
        sus4
        (each
         vel3
         (vel-humanize 1/10)
         (one-of d2 d4 d6)
         (probs {_ 3, [(one-of s0 s1 s2) (par s0 s1 s2)] 1}))]
       {:bintup 1}
       [(patch :marimba)
        vel4
        (vel-humanize 1/5)
        (chan+ 1)
        (each [(one-of d3 d5 d7) (maybe o1 (par _ d4))])])]))))
 (is
  (noon.test/frozen
   dur2
   lydian
   (patch :flute)
   (chans _ d3 d6 d9)
   (each
    [(dupt 24) (each (one-of vel1 vel3 vel6) (probs {_ 6, d1 1}))])
   ($by :channel (maybe rev))
   (append (transpose c3-))
   (append (transpose c1-))))
 (is
  (noon.test/frozen
   dur3
   lydian
   (chans
    [(patch :marimba) (lin _ c1)]
    [(patch :vibraphone) (lin d3 d2)]
    [(patch :celesta) (lin d6 d6)]
    [(patch :orchestral-harp) (lin d9 d9)])
   (append (transpose c2-))
   (dup 2)
   (each
    [(dupt 34)
     (each (one-of vel0 vel3 vel6 vel9) (probs {_ 4, o1 1}))])))
 (is
  (noon.test/frozen
   dur8
   o2
   (dupt 128)
   (each (par> d4 d4 d4) (one-of vel0 vel1 vel2 vel3 vel4 vel5)))))

