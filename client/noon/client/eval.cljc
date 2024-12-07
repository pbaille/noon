(ns noon.client.eval
  (:require
   [cljs.env :as env])
  #?(:cljs (:require [cljs.js]
                     [noon.client.user]
                     [shadow.cljs.bootstrap.browser :as boot])))

#?(:cljs (do (defonce compile-state-ref (env/default-compiler-env))

             (defn evaluate-string [expr cb & {:as opts}]
               (cljs.js/eval-str
                compile-state-ref
                (str expr)
                "[test]"
                (merge {:eval cljs.js/js-eval
                        :load (partial boot/load compile-state-ref)}
                       opts)
                cb))

             (defn simple-try []
               (boot/init compile-state-ref
                          {:path "/bootstrap"}
                          (fn []
                            (evaluate-string (str '(ns noon.user (:require [noon.score :as n]))
                                                  '(n/play-score (n/mk (let [x n/s2] (n/tup n/s0 n/s1 x)))))
                                             println))))

             (defn simple-try2 []
               (boot/init compile-state-ref
                          {:path "/bootstrap"}
                          (fn []
                            (evaluate-string (str "
(ns noon.client.user
  (:require
   [noon.lib.harmony :as h]
   [noon.lib.melody :as m]
   [noon.lib.rythmn :as r]
   [noon.score :refer [scan> reorigin d-step c-position one-of mirror voice+ vel layer-step reroot lt c-step add ! iter ntup> t-floor min-pitch tup append> fill> tracks* max-by s-floor voices e->s s-round sfn within-bounds? superpose* track nlin d-position vel+ play-score s-position in-place track+ each chan- chan lin* fork-with max-pitch event-scale dupt par* superpose> voices* efn mixtup* repitch min-by patch c-shift eq s-shift redegree same dup s+ d-ceil within-midi-pitch-bounds? vel0 par k* sub sf_ _ fit* fit superpose scale t-step inversion transpose shuftup append>* fst maybe mixlin chan+ nlin> fst-that t-shift shuflin* fill start-from chain* t-ceil shuftup* one-of* until rescale connect-by each* append* tup* chans shrink append parts tracks any-that* mk* voice par>* rep between cc fst* track- s-ceil within-time-bounds? lin pc adjust mixlin* gte fst-that* rev k layer-shift only-between s-step try-until voices> div probs rebase fork-with* rup void par> mk tup> mul shuflin lin>* d-floor voices>* any-that start-from-nth-last t-round d-shift structure s- mixtup root vel- tup>* dur degree gt zip e0 from restructure $by voice- chain within-pitch-bounds? superpose>* start-from-last repeat-while chans* lin> parts* trim d-round lte newrep ntup ef_ scan origin position maybe* Abb3 phrygian6 Ex-1 dur3:7 c20- dur10:2 dur:6 dur10:3 F#-1 B#-4 dur4:7 G#3 Cb-2 Db-3 Fbb5 ultralocrian E#-5 A#3 dur6:4 Bx-2 sus2 C#-4 D-4 D1 oriental o1- G1 track13 Gx1 C-4 Cx-4 F#2 A#-4 Bbb-5 c9- VI d9 Gb-5 sus47 track14 Ex1 s10 chan8 dur2:2 dur6:8 Dbb5 Fb4 dur7:4 superlocriano7 V chan4 E5 sus4 Ebb-3 A-2 Db-4 d21- III dur11:11 Bb2 dur2:6 Bb4 t7 o7- Dx-5 Ab-2 d3 C#-5 aeolian c16- t9- s1 c25 dur4:2 dur10:5 D#-3 D#-5 c32- D#2 V# s5 dur:9 chan7 B#-5 chan6 Cbb-1 c7 A1 track15 o2 E-1 d10 track6 E#-3 track4 Gb5 t1 D-1 dur8:4 track10 d15 C#0 dur3 chan11 s3 C#4 Cbb4 E-2 G#0 Cb-3 G#1 Ax-3 E#-2 A#1 c15- chan2 c12 t3- t11 Dbb-3 Ebb1 dur:5 vel10 chan5 Gb-2 d8- dur6:3 Gbb0 Fbb-2 d18- Gx-2 c30 II Fb-4 c26- hungarian vel9 F3 o5 Bx-1 d7- Abb1 s12 C-5 A4 s2 o4 vel2 dur11:8 Bb-1 dur8:11 F#-3 t5 s12- Gx-5 IIIb B#2 dur10:10 dur4:6 s7- t8 dur4:5 dur2:3 d19 Bx-3 C1 IV t6- c1- Dx0 dur11 D#-4 chan9 D4 dur10:7 Gb4 Gx2 d14 dur3:10 F#-4 D#4 C#3 d4- dur8:2 D#0 s9- o6- Eb2 c28- vel12 phrygianb4 C0 dur2:11 E2 Abb-3 C#5 Fb3 dur6:7 lydian++2 Gbb-4 Ab1 s4- Fb-1 d1 track7 Cbb1 vel6 Fb0 dur11:6 c2- Cb4 C#1 Dbb4 B#1 Fbb-3 s7 dur4 F#3 d2- lydian+2 s11 dur9:9 E-3 locrian c35- ionian+ G#-1 Gbb-1 dur6:6 dur10:4 t5- t4- dur9:3 Dx1 Cx-5 Fbb1 c24- Cx5 Bb-3 Cx-2 sixth c22- E#3 dur3:11 dur8:9 Bb1 chan0 c20 Fb5 Dx-3 Bb-2 G#-4 Gb1 Bx-5 E#5 add2 F#0 Eb-3 d16 G4 Ex5 harmonic-major F2 d2 lydian+2+6 c14- t7- Dbb3 s6 VII Cbb3 I c15 G-5 A-1 G2 dur7:3 G5 A#0 dur8:10 c25- Fbb3 o6 Fb1 dur6:2 E#-4 G#-5 s0 C5 Gbb5 dur9:5 Gx-1 Db5 G#4 dur4:10 Ib B#-3 Cbb-3 track2 o3 d6 d3- Eb5 A#-3 C2 Eb-2 s8 Fb-2 s8- E4 VII# dur10 o8 dur10:9 c11 vel11 c12- c31 III# dur8:8 Bbb-3 s10- B#-2 t4 double-harmonic Ex4 c14 c3 Ebb-1 c29- Ax1 c10 track0 D-2 vel1 locrian2 s6- Gx-3 Bx1 d0 Cbb5 dur9:11 c32 c18 F#5 ultraphrygian c17- c30- ionian G-4 d5 Cb0 dur7:9 sus67 IVb dur3:3 Bb-4 t1- Fx-1 t6 Ex3 A#2 t0 dur8:3 Ebb-2 track5 G-2 c7- vel5 D-3 Fbb0 c34 Bbb-4 chan1 D#1 dur7:8 Cb5 Abb-5 dur7:2 Ab3 F4 dur10:11 c6 F-5 dur3:5 Ab-1 d5- Ax2 C3 dur2:5 Ax4 B4 Ab2 Db0 c33- dur5:10 c27- B#3 Db1 D#5 Dbb0 Dx-2 vel3 dur11:10 Gx0 Dbb-1 sus6 lydianb7 c31- Bbb-1 c17 c10- c22 dur5:5 c21- dur5 d12- c4 dur11:3 Eb-4 Db-5 c13 Bb0 F0 t2 Cb3 Ab-3 Db4 c18- dur5:4 c36 superlocrian vel4 Fbb-1 Bbb-2 dur9:10 locriano7 Ax-1 Ebb-5 Cbb2 A2 Fx-4 Bb-5 F-2 dur5:6 E#2 A0 chan3 F-4 Bb3 dur5:2 t11- F5 tetrad A#-1 dur5:3 Fbb-5 Fb-3 VIIb B1 dur9:4 Fb-5 F1 t9 Ax-4 s2- d4 Gbb-5 Bx4 Gbb1 d20- o2- Cx4 Ebb3 t10- vel8 mixolydian Db2 d17- dur:7 Dbb-2 Gx3 dur:3 dorianb5 o1 dorian Cbb-2 Bx0 Eb3 Bx3 Fb2 G#-3 Dx3 Gb-4 c27 o4- Ex2 melodic-minor s4 B-4 I# Cx0 F#-5 F#4 c8- add4 G#2 c2 t8- Abb5 B0 Abb-2 Bbb0 c13- c9 dur3:8 Ax-5 s11- dur4:4 Bbb1 Cb-1 t12 c23- Fx2 Ax0 Gbb2 Ax-2 Ebb5 E#0 Ex-2 G-3 Eb0 Fx-3 B#-1 d12 Cx3 d16- dur3:6 dur2:10 dur11:2 dur7:7 dur9:7 A3 dur10:6 Ab-4 dur5:9 c35 dur:2 D#3 Fbb4 dur9:8 d6- c28 dur2:9 t10 o8- IIb Ebb4 Ax3 Ebb-4 c0 dur11:9 dur7:5 dur7 d8 dur:11 Eb4 d15- D5 Gbb4 triad c5- E1 harmonic-minor Cx2 Dx5 Dx-4 chan12 Gb0 dur8:5 dur11:4 dur:4 Ebb0 A#4 Gx4 Gbb-2 E#4 Db-2 Abb4 Bx-4 dur9 G#-2 d9- IV# Fx3 dur8:7 D#-2 E#-1 dur6:5 o7 c19- dur6 A#-5 D#-1 Vb c21 dur3:4 Abb2 dur3:2 lydian+ c6- Gb2 s1- o5- sus27 B-1 VI# Abb-4 B3 dur4:3 Dbb-4 track12 d1- t12- Fx1 Ex-3 d20 dur7:10 Bbb4 dorian+4 Gbb-3 Ex0 Cb2 c29 mixolydianb6 Cx-3 Dx2 B2 F#1 lydian c36- c1 dur3:9 d11 Eb-5 c8 dur4:11 dur6:10 c23 D2 A-4 Fx-2 track9 phrygian C-1 Cx1 s9 chan13 Ab-5 t2- vel7 dur2:8 E-4 sus7 B-5 o3- dur9:6 lydianb3 c33 chan10 Dbb-5 Ebb2 Cb1 Bbb3 Cbb-4 D-5 C#-1 d13 C-3 VIb Ab4 Abb0 Fbb2 dur5:7 dur2 D0 c19 Fx-5 C4 C#-2 A#-2 C-2 d11- dur11:7 dur10:8 dur5:8 d19- dur:10 track3 G-1 d10- d14- c11- Fx0 Fx4 D3 dur6:11 dur9:2 dur8:6 F#-2 Eb-1 A-3 Db-1 C#-3 G0 dur4:9 Ex-5 Fx5 Bx2 G3 dur:8 Eb1 Db3 Bbb2 Ex-4 Fbb-4 s3- E-5 C#2 Gbb3 dur7:11 Gb-1 d18 Cx-1 Cbb0 d17 dur2:4 Gb3 B#0 track8 track11 E3 c3- B-3 E0 Dbb1 F-3 Dx4 c16 ionian++2 c26 mixolydianb2 track1 c34- dur5:11 Cb-4 phrygian3 c5 B-2 c4- s5- d7 dur8 dur6:9 dur4:8 E#1 dur2:7 t3 II# F-1 Ab0 dur11:5 Abb-1 Gx-4 seventh d13- Dbb2 B#4 c24 d21 locrian6 A-5 chan15 Dx-1 dur7:6 Gb-3 chan14]]))"
                                                  '(play-score (mk (let [x s2] (tup s0 s1 x)))))
                                             println
                                             {:ns 'noon.client.user}))))))
