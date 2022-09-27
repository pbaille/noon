(ns noon.harmony
  (:refer-clojure :exclude [struct])
  (:require [noon.utils.misc :as u :refer [t t? defclosure]]
            [noon.utils.maps :as maps]
            [noon.constants :as constants]))

(do :impl

    (defn safe-add [a b] (+ (or a 0) (or b 0)))
    (def additive-merge (partial merge-with safe-add))
    (defn zero-or-nil? [x] (or (nil? x) (zero? x))))

(do :bidirectional-seq

    (defn bds
      "bidirectional lazy sequence"
      [seq mod]
      {:fw (mapcat #(map (partial + (* mod %)) seq)
                   (range))
       :bw (cons
            (first seq)
            (mapcat
             #(reverse (map (partial + (* (- mod) %)) seq))
             (next (range))))})

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
      (bds-shift bds (first (bds-idx bds val))))

    #_(take 10 (:bw (bds [0 2 4 5] 6)))
    )

(do :ctx

    (def POSITION_ZERO
      {:t 0 :s 0 :d 0 :c 0})

    (def DEFAULT_HARMONIC_CONTEXT
      {:scale [0 2 4 5 7 9 11]
       :struct [0 2 4]
       :origin {:d 35 :c 60}
       :position POSITION_ZERO})

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

        (defn position [& [t s d c]]
          (let [p (into {} (filter val {:t t :s s :d d :c c}))]
            (fn [ctx] (assoc ctx :position p))))

        (def s-position (partial position nil))
        (def d-position (partial position nil nil))
        (def c-position (partial position nil nil nil))

        (defn tonic?
          "Does the given context is positioned exactly on the tonic layer ?"
          [ctx]
          (let [{:keys [s d c]} (:position ctx)]
            (if (and (not s) (not d) (not c))
              ctx)))

        (defn structural?
          "Does the given context is positioned exactly on the structural layer (or above) ?"
          [ctx]
          (let [{:keys [d c]} (:position ctx)]
            (if (and (not d) (not c))
              ctx)))

        (defn diatonic?
          "Does the given context is positioned exactly on the diatonic layer (or above) ?"
          [ctx]
          (if (not (:c (:position ctx)))
            ctx)))

    (do :position-converters

        (do :upward

            (defn c->d
              "feed as much as possible of the c value into the d value"
              [{{:keys [s d c]} :position :as ctx}]
              (if-not c
                ctx
                (let [d (or d 0)
                      {:keys [struct scale]} (hc-seqs ctx)
                      ds (if s (bds-shift scale (bds-get struct s)) scale)
                      dv (bds-get ds d)
                      [d c] (bds-idx ds (+ dv c))]
                  (update ctx :position merge {:d d :c c}))))

            (defn d->s
              "feed as much as possible of the d value into the s value"
              [{{:keys [s d]} :position :as ctx}]
              (if-not d
                ctx
                (let [s (or s 0)
                      {:keys [struct]} (hc-seqs ctx)
                      sv (bds-get struct s)
                      [s d] (bds-idx struct (+ sv d))]
                  (update ctx :position merge {:s s :d d}))))

            (defn s->t
              "feed as much as possible of the s value into the o value"
              [{{:keys [t s]} :position :as ctx}]
              (if-not s
                ctx
                (let [struct-size (count (:struct ctx))
                      tonic-delta (quot s struct-size)]
                  (update ctx :position merge {:t (+ t tonic-delta) :s (rem s struct-size)}))))

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
              [{:as ctx {:keys [t]} :position}]
              (if-not t
                ctx
                (let [struct-size (count (:struct ctx))]
                  (update ctx :position
                          (fn [{:as p :keys [t]}]
                            (-> (dissoc p :t)
                                (update :s safe-add (* struct-size t))))))))

            (defn s->d
              [{:as ctx {:keys [s]} :position}]
              (if-not s
                ctx
                (let [{:keys [struct]} (hc-seqs ctx)]
                  (update ctx :position
                          (fn [p]
                            (-> (dissoc p :s)
                                (update :d safe-add (bds-get struct s))))))))

            (defn d->c
              [{:as ctx {:keys [d]} :position}]
              (if-not d
                ctx
                (let [{:keys [scale]} (hc-seqs ctx)]
                  (update ctx :position
                          (fn [p]
                            (-> (dissoc p :d)
                                (update :c safe-add (bds-get scale d))))))))

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

                     (layer-idx :s (upd (hc) (position 3 2 1 0)))
                     (layer-idx :d (upd (hc) (position 3 2 1 0)))
                     (layer-idx :d (upd (hc) (position 0 0 1 0)))


                     (t->s (upd (hc) (position 3 2 1 0)))
                     (s->d (upd (hc) (position 0 2 1 0))))))

    (do :views

        (def hc->pitch
          "given a context, compute the corresponding pitch"
          (memoize
           (fn [ctx]
             (let [dctx (t->d ctx)
                   cctx (d->c dctx)]
               (additive-merge
                (:origin ctx)
                {:d (get-in dctx [:position :d])
                 :c (get-in cctx [:position :c])})))))

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

    (do :intervals

        (do :intervals

            (defn t-trim [{:as ctx p :position}]
              (update (c->t ctx) :position dissoc :s :d :c))

            (defn t-step [n]
              (fn [ctx]
                (-> (t-trim ctx)
                    (update-in [:position :t] safe-add n))))

            (defn s-trim [{:as ctx p :position}]
              (update (c->s ctx) :position dissoc :d :c))

            (defn s-step [n]
              (fn [ctx]
                (-> (s-trim ctx)
                    (update-in [:position :s] safe-add n))))

            (defn d-trim [{:as ctx p :position}]
              (update (c->d ctx) :position dissoc :c))

            (defn d-step [n]
              (fn [ctx]
                (-> (d-trim ctx)
                    (update-in [:position :d] safe-add n))))

            (defn c-step [n]
              (fn [ctx] (update-in ctx [:position :c] safe-add n)))

            (defn layer-shift [l]
              (fn [n]
                (fn [ctx]
                  (if-let [v (get-in ctx [:position l])]
                    (update-in ctx [:position l] + n)
                    ctx))))

            (def t-shift (layer-shift :t))
            (def s-shift (layer-shift :s))
            (def d-shift (layer-shift :d))
            (def c-shift (layer-shift :c))

            #_(upd (hc)
                 (t-step 2)
                 (d-step 2)
                 (s-step 1))
            )

        (do :roundings

            (def t-round
              (fn [ctx]
                (or (tonic? ctx)
                    (closest ctx [((t-step -1) ctx) (t-trim ctx) ((t-step 1) ctx)]))))
            (def t-ceil
              (fn [ctx]
                (or (tonic? ctx)
                    (closest ctx [(t-trim ctx) ((t-step 1) ctx)]))))
            (def t-floor
              (fn [ctx]
                (or (tonic? ctx)
                    (closest ctx [(t-trim ctx) ((t-step -1) ctx)]))))

            (def s-round
              (fn [ctx]
                (or (structural? ctx)
                    (closest ctx [((s-step -1) ctx) (s-trim ctx) ((s-step 1) ctx)]))))
            (def s-ceil
              (fn [ctx]
                (or (structural? ctx)
                    (closest ctx [(s-trim ctx) ((s-step 1) ctx)]))))
            (def s-floor
              (fn [ctx]
                (or (structural? ctx)
                    (closest ctx [(s-trim ctx) ((s-step -1) ctx)]))))

            (def d-round
              (fn [ctx]
                (or (diatonic? ctx)
                    (closest ctx [((d-step -1) ctx) (d-trim ctx) ((d-step 1) ctx)]))))
            (def d-ceil
              (fn [ctx]
                (or (diatonic? ctx)
                    (closest ctx [(d-trim ctx) ((d-step 1) ctx)]))))
            (def d-floor
              (fn [ctx]
                (or (diatonic? ctx)
                    (closest ctx [(d-trim ctx) ((d-step -1) ctx)])))))

        (defn normalise
          "normalise the context position to its simplest form."
          [ctx]
          (let [cval (hc->chromatic-value ctx)
                {:as tctx {t :t} :position} (t-round (c->t ctx))
                tdelta (- cval (hc->chromatic-value tctx))
                {:as sctx {s :s} :position} (s-round (c->s ((c-step tdelta) tctx)))
                sdelta (- cval (hc->chromatic-value sctx))
                {:as dctx {d :d} :position} (d-round (c->d ((c-step sdelta) sctx)))
                c (- cval (hc->chromatic-value dctx))
                position (into {} (filter val {:t t :s s :d d :c c}))]
            (assoc ctx :position position)))

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

            (def t0 t-trim)
            (def s0 s-trim)
            (def d0 d-trim)
            (def c0 (c-step 0))

            (doseq [i (range 1 37)]
              (eval (list 'def (symbol (str "c" i)) `(c-step ~i)))
              (eval (list 'def (symbol (str "c" i "-")) `(c-step ~(- i)))))
            (doseq [i (range 1 22)]
              (eval (list 'def (symbol (str "d" i)) `(d-step ~i)))
              (eval (list 'def (symbol (str "d" i "-")) `(d-step ~(- i)))))
            (doseq [i (range 1 13)]
              (eval (list 'def (symbol (str "s" i)) `(s-step ~i)))
              (eval (list 'def (symbol (str "s" i "-")) `(s-step ~(- i)))))
            (doseq [i (range 1 13)]
              (eval (list 'def (symbol (str "t" i)) `(t-step ~i)))
              (eval (list 'def (symbol (str "t" i "-")) `(t-step ~(- i)))))
            (doseq [i (range 1 9)]
              (eval (list 'def (symbol (str "o" i)) `(t-shift ~i)))
              (eval (list 'def (symbol (str "o" i "-")) `(t-shift ~(- i))))))

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

        (declare upd)

        (defclosure repitch
          "reposition the context based on the given pitch"
          [x]
          (if-let [p (constants/get-pitch x)]
            (fn [ctx] (normalise (upd ctx (pitch->position ctx p))))
            (u/throw* "cannot make a pitch from: " x)))
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
             (origin (hc->pitch (upd ctx (d-position n)))))))

    (def reroot (comp rebase root))
    (def redegree (comp rebase degree))

    (defclosure transpose
      "transpose the current origin by the given update"
      [x]
      (fn [ctx]
        (assoc ctx :origin (hc->pitch (upd ctx POSITION_ZERO x)))))

    (defn position+ [ctx p]
      (reduce
       (fn [ctx [k v]] (upd ctx ((layer-shift k) v)))
       ctx
       p))

    (defclosure hc+
      "merging with another context.
       scale, struct and origin are replaced, position is additioned."
      [ctx1]
      (fn [ctx2]
        (normalise
         (position+ (merge ctx2 (dissoc ctx1 :position))
                    (:position ctx1)))))

    (defn align
      "align context b on context a, rounding on the given layer
       it is useful when writing harmonic or melodic sequences that traverse several contexts"
      [layer a b]
      (let [ret (upd b (repitch (hc->pitch a)))]
        (case layer
          (:tonic :t) (t-round ret)
          (:structural :s) (s-round ret)
          (:diatonic :d) (d-round ret)
          (:chromtatic :c) ret)))

    ;; passing-tones

    (defn s+
      "melodic superior diatonic passing note"
      [ctx]
      (let [ctx+ (upd ctx s0 d1)
            dist (chromatic-distance ctx ctx+)]
        (if (< dist 3)
          ctx+
          (upd ctx s0 d1 (c-step (- 2 dist))))))

    (defn s-
      "melodic inferior diatonic passing note"
      [ctx]
      (let [ctx- (upd ctx s0 d1-)
            dist (chromatic-distance ctx ctx-)]
        (if (< dist 3)
          ctx-
          (upd ctx s0 d1- (c-step (- dist 2))))))

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

         (position+ POSITION_ZERO p2)
         (upd (hc) (hc+ (upd (hc) t1 d3)))

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

         (upd hc0 superlocriano7 s-)
         (upd hc0 lydian+2 s+)

         (upd hc0 (scale alt) (root B-1))

         (scale melm)
         (hc melm))

(comment :positions

         (upd (hc) t2 s2)
         (upd (hc) t2 s3 t0)
         (upd (hc) t2 s2 (d-shift 3)))
