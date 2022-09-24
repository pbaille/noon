(ns noon.harmony
  (:refer-clojure :exclude [struct])
  (:require [noon.utils.misc :as u :refer [t t? defclosure]]
            [noon.utils.maps :as maps]
            [noon.constants :as constants]))

(do :impl

    (def additive-merge (partial merge-with +)))

(do :bidirectional-seq

    (defn bds
      "bidirectional lazy sequence"
      [seq mod]
      {:fw (flatten
            (map #(map (partial + (* mod %)) seq)
                 (range)))
       :bw (cons
            (first seq)
            (flatten
             (map
              #(reverse (map (partial + (* (- mod) %)) seq))
              (next (range)))))})

    (defn bds-get
      "access value at index idx, neg idxs go backward"
      [bds idx]
      (condp = (compare 0 idx)
        0 (first (:fw bds))
        1 (nth (:bw bds) (u/abs idx))
        -1 (nth (:fw bds) idx)))

    (defn bds-shift
      "shift a bidirectional seq by the given idx"
      [bds idx]
      (condp = (compare 0 idx)

        0 bds

        1 {:bw (drop (u/abs idx) (:bw bds))
           :fw (concat (reverse (take (inc (u/abs idx)) (:bw bds)))
                       (next (:fw bds)))}

        -1 {:fw (drop idx (:fw bds))
            :bw (concat (reverse (take (inc idx) (:fw bds)))
                        (next (:bw bds)))}))

    (defn bds-idx
      "return the index of the given value and the reminder in a vector: [idx rem]"
      [{:keys [fw bw] :as bds} v]

      (let [fw? (> v (first fw))

            taken (if fw?
                    (take-while #(>= v %) fw)
                    (take-while #(<= v %) bw))

            abs-idx (dec (count taken))

            idx (if fw?
                  abs-idx
                  (- abs-idx))]

        [idx ((if fw? + -)
              (u/dist v (last taken)))]))

    (defn bds-go
      "shift the bds to the given val, ignoring reminder"
      [bds val]
      (bds-shift bds (first (bds-idx bds val)))))

(do :ctx

    (def DEFAULT_HARMONIC_CONTEXT
      {:scale [0 2 4 5 7 9 11]
       :struct [0 2 4]
       :origin {:d 35 :c 60}
       :position {:t 0 :s 0 :d 0 :c 0}})

    (defn hc
      "harmonic context constructor"
      [& [spec]]
      (t :harmonic-context
         (merge DEFAULT_HARMONIC_CONTEXT spec)))

    (def hc? (t? :harmonic-context))

    (def hc0 (hc))

    (defn hc-seqs [{:keys [scale struct]}]
      {:scale (bds scale 12)
       :struct (bds struct (count scale))})

    (do :position

        (defclosure position
          "an update that reposition the harmonic context
           o: octave idx
           s: structural idx
           d: diatonic index
           c: chromatic index"
          ([] (position 0 0 0 0))
          ([t] (position t 0 0 0))
          ([t s] (position t s 0 0))
          ([t s d] (position t s d 0))
          ([t s d c]
           (fn [ctx]
             (assoc ctx :position
                    {:t t :s s :d d :c c}))))

        (do :absolute

            (def structural-position
              (partial position 0))
            (def diatonic-position
              (partial position 0 0))
            (def chromatic-position
              (partial position 0 0 0))

            (def tp position)
            (def sp structural-position)
            (def dp diatonic-position)
            (def cp chromatic-position))

        (defn tonic?
          "Does the given context is positioned exactly on the tonic layer ?"
          [ctx]
          (let [{:keys [s d c]} (:position ctx)]
            (if (and (zero? s) (zero? d) (zero? c))
              ctx)))

        (defn structural?
          "Does the given context is positioned exactly on the structural layer (or above) ?"
          [ctx]
          (let [{:keys [d c]} (:position ctx)]
            (if (and (zero? d) (zero? c))
              ctx)))

        (defn diatonic?
          "Does the given context is positioned exactly on the diatonic layer (or above) ?"
          [ctx]
          (if (zero? (:c (:position ctx)))
            ctx)))

    (do :views

        (def hc->pitch
          "given a context, compute the corresponding pitch"
          (memoize
           (fn [{:as ctx
                {:keys [t s d c]} :position}]
             (let [struct-size (count (:struct ctx))
                   {:keys [scale struct]} (hc-seqs ctx)
                   s (+ s (* t struct-size))
                   d (+ (bds-get struct s) d)
                   c (+ (bds-get scale d) c)]
               (additive-merge
                (:origin ctx)
                {:d d :c c})))))

        (def hc->chromatic-value (comp :c hc->pitch))
        (def hc->diatonic-value (comp :d hc->pitch))

        (defn chromatic-distance
          "return the chromatic distance between two context"
          [ctx1 ctx2]
          (u/dist (hc->chromatic-value ctx1)
                  (hc->chromatic-value ctx2)))

        (defn diatonic-distance
          "return the diatonic distance between two context"
          [ctx1 ctx2]
          (u/dist (hc->diatonic-value ctx1)
                  (hc->diatonic-value ctx2)))

        (defn distance [a b]
          [(chromatic-distance a b)
           (diatonic-distance a b)])

        (defn- closest
          "return the closest context to 'reference-ctx from the 'candidate-ctx list"
          [reference-ctx candidate-ctxs]
          (->> candidate-ctxs
               (sort-by (partial distance reference-ctx))
               (first)))

        (defn pitch->position
          [ctx p]
          (let [struct-size (count (:struct ctx))
                {:keys [c d]} (merge-with - p (:origin ctx))
                {:keys [struct scale]} (hc-seqs ctx)
                [s* d*] (bds-idx struct d)
                c* (- c (bds-get scale d))
                t* (quot s* struct-size)
                s** (rem s* struct-size)]
            (position t* s** d* c*))))

    (do :position-converters

        (do :upward

            (defn c->d
              "feed as much as possible of the c value into the d value"
              [{{:keys [t s d c]} :position :as ctx}]
              (let [{:keys [struct scale]} (hc-seqs ctx)
                    sv (bds-get struct s)
                    ds (bds-shift scale sv)
                    dv (bds-get ds d)
                    [d c] (bds-idx ds (+ dv c))]
                ((position t s d c) ctx)))

            (defn d->s
              "feed as much as possible of the d value into the s value"
              [{{:keys [t s d c]} :position :as ctx}]
              (let [{:keys [struct]} (hc-seqs ctx)
                    sv (bds-get struct s)
                    [s d] (bds-idx struct (+ sv d))]
                ((position t s d c) ctx)))

            (defn s->t
              "feed as much as possible of the s value into the o value"
              [{{:keys [t s d c]} :position :as ctx}]
              (let [struct-size (count (:struct ctx))
                    tonic-delta (quot s struct-size)]
                ((position (+ t tonic-delta)
                           (rem s struct-size)
                           d c)
                 ctx)))

            (defn d->t
              "feed as much as possible of the d value into the upward layers"
              [x]
              (-> x d->s s->t))

            (defn c->s
              "feed as much as possible of the c value into the d value and s value"
              [x]
              (-> x c->d d->s))

            (defn c->t
              "feed as much as possible of the c value to the upward layers"
              [x]
              (-> x c->s s->t)))

        (do :downward

            (defn t->s
              [{{:keys [t s d c]} :position :as ctx}]
              (let [struct-size (count (:struct ctx))]
                ((position 0 (+ s (* struct-size t)) d c)
                 ctx)))

            (defn s->d
              [{{:keys [t s d c]} :position :as ctx}]
              (let [{:keys [struct]} (hc-seqs ctx)]
                ((position t 0 (+ d (bds-get struct s)) c)
                 ctx)))

            (defn d->c
              [{{:keys [t s d c]} :position :as ctx}]
              (let [{:keys [scale]} (hc-seqs ctx)]
                ((position t s 0 (+ c (bds-get scale d)))
                 ctx)))

            (defn t->d [ctx]
              (-> ctx t->s s->d))

            (defn s->c [ctx]
              (-> ctx s->d d->c))

            (defn t->c [ctx]
              (-> ctx t->s s->c))

            (defn layer-idx [layer ctx]
              (let [[converter k]
                    (case layer
                      (:tonic :t) [identity :t]
                      (:structural :s) [t->s :s]
                      (:diatonic :d) [t->d :d]
                      (:chromatic :c) [t->c :c])]
                (get-in (converter ctx)
                        [:position k])))

            (comment :to-move

                     (layer-idx :s ((position 3 2 1 0) (hc)))
                     (layer-idx :d ((position 3 2 1 0) (hc)))
                     (layer-idx :d ((position 0 0 1 0) (hc)))


                     (t->s ((position 3 2 1 0) (hc)))
                     (s->d ((position 0 2 1 0) (hc)))))

        (defn tonic-trim
          "push as much as possible onto the tonic layer and trim the remaining"
          [ctx]
          (update (c->t ctx) :position assoc :s 0 :d 0 :c 0))

        (defn structural-trim
          "push as much as possible onto the structural layer and trim the remaining"
          [ctx]
          (update (c->s ctx) :position assoc :d 0 :c 0))

        (defn diatonic-trim
          "push as much as possible onto the diatonic layer and trim the remaining"
          [ctx]
          (update (c->d ctx) :position assoc :c 0))

        (defn trimmer
          [layer]
          (case layer
            (:tonic :t) tonic-trim
            (:structural :s) structural-trim
            (:diatonic :d) diatonic-trim
            (:chromatic :c) identity)))

    (do :intervals

        (defclosure interval
          "an update to shift the position of an harmonic context"
          ([t] (interval t 0 0 0))
          ([t s] (interval t s 0 0))
          ([t s d] (interval t s d 0))
          ([t s d c]
           (fn [ctx]
             (update ctx :position
                     additive-merge
                     {:t t :s s :d d :c c}))))

        (defclosure tonic-interval [t s d c]
          (let [i (interval t s d c)]
            (fn [ctx] (i (tonic-trim ctx)))))

        (defclosure structural-interval [s d c]
          (let [i (interval 0 s d c)]
            (fn [ctx] (i (structural-trim ctx)))))

        (defclosure diatonic-interval [d c]
          (let [i (interval 0 0 d c)]
            (fn [ctx] (i (diatonic-trim ctx)))))

        (defclosure chromatic-interval [c]
          (interval 0 0 0 c))


        (defclosure octave-interval [v]
          (interval v 0 0 0))

        (do :shifts

            (def structural-shift (partial interval 0))

            (def diatonic-shift (partial interval 0 0)))

        (do :tonic-interval-alternative

            "the issue with tonic interval occurs when struct do not contains the tonic."
            "In this case the first struct note is considered to be the tonic which do not make sense"
            "But we can shift the diatonic layer to put the ctx on the real tonic."

            (defn tonic-trim'
              "push as much as possible onto the tonic layer and trim the remaining"
              [ctx]
              (update (c->t ctx)
                      :position
                      assoc
                      :s 0
                      :d (- (get-in ctx [:struct 0]))
                      :c 0))

            (defclosure tonic-interval' [offset]
              (fn [ctx]
                (update-in (tonic-trim' ctx)
                           [:position :t] + offset)))

            (def ti' tonic-interval'))

        (do :shorthands

            (def ci chromatic-interval)

            (def oi octave-interval)

            (defn di [d & [c]]
              (diatonic-interval d (or c 0)))

            (defn si [s & [d c]]
              (structural-interval s (or d 0) (or c 0)))

            (defn ti [t & [s d c]]
              (tonic-interval t (or s 0) (or d 0) (or c 0)))

            (def s-shift structural-shift)
            (def d-shift diatonic-shift))

        (do :roundings

            (def tonic-round
              (fn [ctx]
                (or (tonic? ctx)
                    (closest ctx [((ti -1) ctx) (tonic-trim ctx) ((ti 1) ctx)]))))
            (def tonic-ceil
              (fn [ctx]
                (or (tonic? ctx)
                    (closest ctx [(tonic-trim ctx) ((ti 1) ctx)]))))
            (def tonic-floor
              (fn [ctx]
                (or (tonic? ctx)
                    (closest ctx [(tonic-trim ctx) ((ti -1) ctx)]))))

            (def structural-round
              (fn [ctx]
                (or (structural? ctx)
                    (closest ctx [((si -1) ctx) (structural-trim ctx) ((si 1) ctx)]))))
            (def structural-ceil
              (fn [ctx]
                (or (structural? ctx)
                    (closest ctx [(structural-trim ctx) ((si 1) ctx)]))))
            (def structural-floor
              (fn [ctx]
                (or (structural? ctx)
                    (closest ctx [(structural-trim ctx) ((si -1) ctx)]))))

            (def diatonic-round
              (fn [ctx]
                (or (diatonic? ctx)
                    (closest ctx [((di -1) ctx) (diatonic-trim ctx) ((di 1) ctx)]))))
            (def diatonic-ceil
              (fn [ctx]
                (or (diatonic? ctx)
                    (closest ctx [(diatonic-trim ctx) ((di 1) ctx)]))))
            (def diatonic-floor
              (fn [ctx]
                (or (diatonic? ctx)
                    (closest ctx [(diatonic-trim ctx) ((di -1) ctx)])))))

        (defn normalise
          "normalise the context position to its simplest form."
          [ctx]
          (let [cval (hc->chromatic-value ctx)
                {:as tctx {t :t} :position} (tonic-round (c->t ctx))
                tdelta (- cval (hc->chromatic-value tctx))
                {:as sctx {s :s} :position} (structural-round (c->s ((ci tdelta) tctx)))
                sdelta (- cval (hc->chromatic-value sctx))
                {:as dctx {d :d} :position} (diatonic-round (c->d ((ci sdelta) sctx)))
                c (- cval (hc->chromatic-value dctx))]
            ((position t s d c) ctx)))

        (comment :normalise-tries

                 (upd (hc)
                      (position 1 4 3 -2)
                      normalise)

                 (hc->chromatic-value
                  (upd (hc)
                       (position 1 4 3 -2)))

                 (hc->chromatic-value
                  (upd (hc)
                       (position 1 4 4 -2)
                       normalise)))

        (do :vars

            (doseq [i (range 1 37)]
              (eval (list 'def (symbol (str "ci" i)) `(ci ~i)))
              (eval (list 'def (symbol (str "ci" i "-")) `(ci ~(- i)))))
            (doseq [i (range 1 22)]
              (eval (list 'def (symbol (str "di" i)) `(di ~i)))
              (eval (list 'def (symbol (str "di" i "-")) `(di ~(- i)))))
            (doseq [i (range 1 13)]
              (eval (list 'def (symbol (str "si" i)) `(si ~i)))
              (eval (list 'def (symbol (str "si" i "-")) `(si ~(- i)))))
            (doseq [i (range 1 13)]
              (eval (list 'def (symbol (str "ti" i)) `(ti ~i)))
              (eval (list 'def (symbol (str "ti" i "-")) `(ti ~(- i)))))
            (doseq [i (range 1 9)]
              (eval (list 'def (symbol (str "oi" i)) `(oi ~i)))
              (eval (list 'def (symbol (str "oi" i "-")) `(oi ~(- i))))))

        )

    (do :update-constructors

        (defclosure origin
          "reset the origin of an harmonic context"
          [x]
          (if-let [p (constants/get-pitch x)]
            (fn [ctx] (assoc ctx :origin p))
            (u/throw* "cannot make a pitch from: " x)))

        (defclosure scale
          "reset the scale of an harmonic context"
          [x]
          (if-let [m (constants/get-mode x)]
            (fn [ctx] (assoc ctx :scale m))
            (u/throw* "cannot make a scale from: " x)))

        (defclosure struct
          "reset the struct of an harmonic context"
          [x]
          (if-let [s (constants/get-struct x)]
            (fn [ctx] (assoc ctx :struct s))
            (u/throw* "cannot make a struct from: " x)))

        (defclosure repitch
          "reposition the context based on the given pitch"
          [x]
          (if-let [p (constants/get-pitch x)]
            (fn [ctx] (normalise ((pitch->position ctx p) ctx)))
            (u/throw* "cannot make a pitch from: " x)))

        (declare upd)
        (defclosure rebase
          "Apply the given transformations while preserving pitch"
          [& fs]
          (fn [ctx]
            ((repitch (hc->pitch ctx)) (reduce upd ctx fs))))

        (def rescale (comp rebase scale))
        (def restruct (comp rebase struct))
        (def reorigin (comp rebase origin)))

    (do :update

        (defn ->hc-update [x]
          (cond
            (nil? x) identity
            (fn? x) x
            (hc? x) (fn [ctx] ((repitch (hc->pitch ctx)) x))
            :else (u/throw* "not an update " x)))

        (defn upd
          ([ctx x]
           ((->hc-update x) ctx))
          ([ctx x & xs]
           (reduce upd (upd ctx x) xs)))))

(do :extras

    (defclosure root
      "given a pitch class (name or map)
       reset the origin of the given context to the closest (to current origin) corresponding pitch"
      [pitch-class]
      (if-let [pc (constants/get-pitch-class pitch-class)]
        (fn [ctx]
          (let [chromatic-value (hc->chromatic-value ctx)]
            (->> (constants/pitch-class->pitches pc)
                 (sort-by (fn [{c :c}] (u/dist chromatic-value c)))
                 (first)
                 (origin)
                 (upd ctx))))
        (u/throw* "cannot make a pitch-class from: " pitch-class)))

    (defclosure degree
      "go to degree n (potentially negative) of the given hc
       preserving current position"
      [n]
      (fn [{:as ctx sc :scale}]
        #_(println "degree fn " ctx)
        (upd ctx
             (scale (get (constants/scale-modes sc)
                         (mod n (count sc))))
             (origin (hc->pitch ((diatonic-position n) ctx))))))

    (def reroot (comp rebase root))
    (def redegree (comp rebase degree))

    (defclosure transpose
      "transpose the current origin by the given update"
      [x]
      (fn [ctx]
        (assoc ctx :origin (hc->pitch (upd ctx (position 0 0 0 0) x)))))

    (defclosure hc+
      "merging with another context.
       scale, struct and origin are replaced, position is additioned."
      [ctx1]
      (fn [{:as ctx2 p2 :position}]
        (normalise
         (assoc (merge ctx2 ctx1)
                :position
                (additive-merge p2 (:position ctx1))))))

    (defn align
      "align context b on context a, rounding on the given layer
       it is useful when writing harmonic or melodic sequences that traverse several contexts"
      [layer a b]
      (let [ret (upd b (repitch (hc->pitch a)))]
        (case layer
          (:tonic :t) (tonic-round ret)
          (:structural :s) (structural-round ret)
          (:diatonic :d) (diatonic-round ret)
          (:chromtatic :c) ret)))

    ;; passing-tones

    (defn s+
      "melodic superior diatonic passing note"
      [ctx]
      (let [ctx+ (upd ctx (structural-interval 0 1 0))
            dist (chromatic-distance ctx ctx+)]
        (if (< dist 3)
          ctx+
          (upd ctx (structural-interval 0 1 (- 2 dist))))))

    (defn s-
      "melodic inferior diatonic passing note"
      [ctx]
      (let [ctx- (upd ctx (structural-interval 0 -1 0))
            dist (chromatic-distance ctx ctx-)]
        (if (< dist 3)
          ctx-
          (upd ctx (structural-interval 0 -1 (- dist 2))))))

    (def passings
      {:approach {:simple {:+ [s+ nil]
                           :- [s- nil]}

                  :double {:+ [s+ s- nil]
                           :- [s- s+ nil]}

                  :triple {:+ [s+ s- s+ nil]
                           :- [s- s+ s- nil]}}

       :broderie {:simple {:+ [nil s+ nil]
                           :- [nil s- nil]}

                  :double {:+ [nil s+ nil s- nil]
                           :- [nil s- nil s+ nil]}}})

    (def all-passings
      (map (fn [[p v]] (apply u/flagged (conj p v)))
           (u/hm-leaves passings)))

    (defn mirror [pitch]
      (fn [ctx]
        (let [pivot (constants/get-pitch pitch)
              p (hc->pitch ctx)
              delta (merge-with - p pivot)
              nxt-p (merge-with - pivot delta)]
          (upd ctx (repitch nxt-p)))))

    #_(upd (hc) (mirror :F0))
    #_(upd (hc) (di 1) (mirror :C0))

    )

(do :defs

    (u/hm->defs 'noon.harmony
                (u/map-vals struct constants/structs))

    (u/hm->defs 'noon.harmony
                (u/map-vals scale constants/modes))

    (u/hm->defs 'noon.harmony
                (u/map-vals origin constants/pitches)))

(comment :tries

         (upd (hc) (transpose di1))

         (hc->chromatic-value (upd (hc) ti2))

         (upd (hc) (reroot :D))

         (upd (hc)
              (diatonic-interval 2 1)
              round-structural
              (structural-interval 1 0 0)
              (degree 2)
              )

         (upd (hc) melm (origin Db0))

         (upd hc0 (degree 3))

         (upd hc0 altdim s-)
         (upd hc0 lyd+2 s+)

         (upd hc0 (scale alt) (root B-1))

         (scale melm)
         (hc melm))
