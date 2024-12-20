(ns noon.freeze
  (:require [noon.eval]
            #_[noon.utils.misc :refer [pretty-str]]
            [noon.utils.pseudo-random :as pseudo-random]
            [noon.freezer :refer [freezer]]
            #_(:cljs ["fs" :as fs]))
  #?(:cljs (:require-macros [noon.freeze :refer [freeze]])))

(def FREEZE_FILE "test/noon/freezer.cljc")

(defmacro freeze [form]
  (let [expr-hash (hash form)
        score-expr `(pseudo-random/with-rand 0
                      (noon.eval/eval-and-return '~form))]
    `(if-let [frozen# (get @freezer ~expr-hash)]
       (= frozen# (hash ~score-expr))
       (swap! freezer assoc ~expr-hash (hash ~score-expr)))))

#_(defn write-freezer []
  (let [spit-fn #?(:cljs (fn [file content]
                           (.writeFileSync fs file content "utf8"))
                   :clj spit)]
    (spit-fn FREEZE_FILE
             (str '(ns noon.freezer)
                  "\n"
                  (list 'def 'freezer
                        (list 'atom @freezer))))))

(comment
  (macroexpand '(freeze (play (tup s0 s1 s2))))
  (macroexpand '(freeze (play (tup s0 s1 s2))))
  (freeze (play (tup s0 s1 s3)))
  (swap! noon.output/options*
         assoc :mute false)
  (freeze
   (play (chans
          [(patch :aahs) vel6 (rup 24 (any-that (within-pitch-bounds? :G-1 :G1) s2 s2- s3 s3-)) (each (par s0 s1 s2 s3))]
          [(patch :acoustic-bass) t2-])
         (h/grid tetrad
                 (lin [I lydian (structure [2 3 5 6])]
                      [IIb dorian (structure [1 2 3 6])]
                      [V mixolydian (structure [2 3 5 6])]

                      [Vb melodic-minor (structure [1 2 5 6])])
                 (rep 2 (transpose c2-)) (dup 2) (h/align-contexts :d :static) (adjust 1))
         (parts (patch :acoustic-bass) (each (tup (maybe o1) (one-of d4 d3-)))) (adjust 32)))
  (freeze (noon.eval/play (one-of s0 s1 s2)))
  (freeze (play (symetric-modes :half-whole) (rand-structure 3) (rep 3 rand-degree) (each (chans [vel4 h/simple-chord] [(patch :music-box) o1 (rand-tup 6) (each (one-of vel0 vel4 vel6 vel7))])) (append [rev s2]) (append (transpose c5)) (append (between 0 1/3))))
  (freeze (def bus1 (noon.midi/get-output-device "Bus 1")))
  (noon.eval/eval 'midi/DEFAULT_EVENT)
  (freeze (play dur2 (chans [(patch :tinkle-bell) o1-] [(patch :woodblock) (r/durtup 2 1 1 4) (r/rotation :relative (/ -1 4))]) (dup 4)))
  (write-freezer)
  @freezer
  (reset! freezer {})
  (hash (:result (noon.eval/eval '(play s0))))
  (noon.eval/eval '(noon {:play true} (score (sf_ _)))))
