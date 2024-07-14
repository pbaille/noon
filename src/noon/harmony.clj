(ns noon.harmony
  (:refer-clojure :exclude [struct])
  (:require [noon.utils.misc :as u :refer [t t?]]
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
      [{:keys [fw bw] :as _bds} v]

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
                  (update ctx :position merge {:t (+ (or t 0) tonic-delta) :s (rem s struct-size)}))))

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
                (let [struct (:struct ctx)
                      struct-size (count struct)
                      contains-tonic? (zero? (first struct))]
                  (update ctx :position
                          (fn [{:as p :keys [t]}]

                            (if contains-tonic?
                              (-> (dissoc p :t)
                                  (update :s safe-add (* struct-size t)))
                              (-> (dissoc p :t)
                                  (update :s safe-add (* struct-size t))
                                  (update :d safe-add (- (first struct))))))))))

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

            (defn down-to-layer [layer ctx]
              (case layer
                (:tonic :t) ctx
                (:structural :s) (t->s ctx)
                (:diatonic :d) (t->d ctx)
                (:chromatic :c) (t->c ctx)))

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
                     (s->d (upd (hc) (position 0 2 1 0)))
                     (t->s (upd (hc) superlocrian (struct [2 3 5 6]) (t-step 0))))))

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

            (defn t-trim [{:as ctx}]
              (update (c->t ctx) :position dissoc :s :d :c))

            (defn t-step [n]
              (fn [ctx]
                (-> (t-trim ctx)
                    (update-in [:position :t] safe-add n))))

            (defn s-trim [{:as ctx}]
              (update (c->s ctx) :position dissoc :d :c))

            (defn s-step [n]
              (fn [ctx]
                (-> (s-trim ctx)
                    (update-in [:position :s] safe-add n))))

            (defn d-trim [{:as ctx}]
              (update (c->d ctx) :position dissoc :c))

            (defn d-step [n]
              (fn [ctx]
                (-> (d-trim ctx)
                    (update-in [:position :d] safe-add n))))

            (defn c-step [n]
              (fn [ctx] (update-in ctx [:position :c] safe-add n)))

            (defn layer-step [layer n]
              (case layer
                (:tonic :t) (t-step n)
                (:structural :s) (s-step n)
                (:diatonic :d) (d-step n)
                (:chromatic :c) (c-step n)))

            (defn layer-shift [l]
              (fn [n & [forced]]
                (fn [ctx]
                  (cond
                    forced (update-in ctx [:position l] safe-add n)
                    (get-in ctx [:position l]) (update-in ctx [:position l] + n)
                    :else ctx))))

            (def t-shift (layer-shift :t))
            (def s-shift (layer-shift :s))
            (def d-shift (layer-shift :d))
            (def c-shift (layer-shift :c))

            #_(upd (hc)
                   (t-step 2)
                   (d-step 2)
                   (s-step 1)))

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
                      (d-position 10)
                      normalise)

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

            (defmacro defsteps [prefix max f]
              (cons 'do
                    (mapcat
                     (fn [_]
                       [(list 'def (symbol (str prefix _)) (list f _))
                        (list 'def (symbol (str prefix _ "-")) (list f (list `- _)))])
                     (range 1 max))))

            (defsteps "c" 37 c-step)
            (defsteps "d" 22 d-step)
            (defsteps "s" 13 s-step)
            (defsteps "t" 13 t-step)
            (defsteps "o" 9 (fn [i] (t-shift i :forced)))))

    (do :update-constructors

        (defn origin
          "reset the origin of an harmonic context"
          [x]
          (if-let [p (constants/get-pitch x)]
            (fn [ctx] (assoc ctx :origin p))
            (u/throw* "cannot make a pitch from: " x)))

        (defn scale
          "reset the scale of an harmonic context"
          [x]
          (if-let [m (constants/get-mode x)]
            (fn [ctx] (assoc ctx :scale m))
            (u/throw* "cannot make a scale from: " x)))

        (defn struct
          "reset the struct of an harmonic context"
          [x]
          (if-let [s (constants/get-struct x)]
            (fn [ctx] (assoc ctx :struct s))
            (u/throw* "cannot make a struct from: " x)))

        (declare upd)

        (defn repitch
          "reposition the context based on the given pitch"
          [x]
          (if-let [p (constants/get-pitch x)]
            (fn [ctx] (normalise (upd ctx (pitch->position ctx p))))
            (u/throw* "cannot make a pitch from: " x)))
        (defn rebase
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

    (defn root
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

    (defn degree
      "go to degree n (potentially negative) of the given hc
       preserving current position"
      [n]
      (fn [{:as ctx sc :scale}]
        #_(println "degree fn " ctx)
        (upd ctx
             (scale (get (constants/scale-modes sc)
                         (mod n (count sc))))
             (origin (hc->pitch (upd ctx (d-position n)))))))

    (defn inversion
      "go to inversion n (potentially negative) of the given hc
       preserving current position"
      [n]
      (fn [{:as ctx sc :scale st :struct}]
        (let [new-origin (upd ctx (s-position n))
              new-scale (get (constants/scale-modes sc)
                             (mod (-> (s->d new-origin) :position :d) (count sc)))
              new-struct (get (constants/struct-inversions sc st)
                              (mod n (count st)))]
          (upd ctx
               (scale new-scale)
               (struct new-struct)
               (origin (hc->pitch new-origin))))))

    (comment
      (constants/struct-inversions [0 2 4 5 7 9 11] [0 1 2 4])
      (upd hc0 (inversion 2)))

    (def reroot (comp rebase root))
    (def redegree (comp rebase degree))

    (defn transpose
      "transpose the current origin by the given update"
      [x]
      (fn [ctx]
        (assoc ctx :origin (hc->pitch (upd ctx POSITION_ZERO x)))))

    (defn position+ [ctx p]
      (reduce
       (fn [ctx [k v]] (upd ctx ((layer-shift k) v)))
       ctx
       p))

    (defn hc+
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

    (defn mirror [pitch]
      (fn [ctx]
        (let [pivot (constants/get-pitch pitch)
              p (hc->pitch ctx)
              delta (merge-with - p pivot)
              nxt-p (merge-with - pivot delta)]
          (upd ctx (repitch nxt-p)))))

    (comment
      (upd (hc) (mirror :F0))
      (upd (hc) (di 1) (mirror :C0)))

    (do :passings
        (defn s+
          "melodic superior diatonic passing note"
          [ctx]
          (let [ctx+ (upd ctx s0 d1)
                dist (chromatic-distance ctx ctx+)]
            (if (< dist 3)
              ctx+
              (upd ctx+ (c-step (- 2 dist))))))

        (defn s-
          "melodic inferior diatonic passing note"
          [ctx]
          (let [ctx- (upd ctx s0 d1-)
                dist (chromatic-distance ctx ctx-)]
            (if (< dist 3)
              ctx-
              (upd ctx- (c-step (- dist 2))))))

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

        (defn lowest-layer [{p :position}]
          (cond
            (:c p) :c
            (:d p) :d
            (:s p) :s
            (:t p) :t))

        (defn resolution-layer [ctx]
          (get {:s :t :d :s :c :d}
               (lowest-layer ctx)
               :t))

        (defn tension-layer [ctx]
          (get {:t :s :s :d :d :c}
               (lowest-layer ctx)
               :c))

        (defn decorate-upward [ctx]
          (upd ctx (layer-step (lowest-layer ctx) 1)))

        (defn decorate-downward [ctx]
          (upd ctx (layer-step (lowest-layer ctx) -1)))

        (defn resolve-upward [ctx]
          (upd ctx (layer-step (resolution-layer ctx) 1)))

        (defn resolve-downward [ctx]
          (upd ctx (layer-step (resolution-layer ctx) -1)))

        (defn tense-upward [ctx]
          (upd ctx (layer-step (tension-layer ctx) 1)))

        (defn tense-downward [ctx]
          (upd ctx (layer-step (tension-layer ctx) -1)))

        (defn diatonic-equivalent?
          "the current position is equivalent to a diatonic one. "
          [ctx]
          (zero? (chromatic-distance ctx (d-round ctx))))

        (defn structural-equivalent?
          "the current position is equivalent to a structural one. "
          [ctx]
          (zero? (chromatic-distance ctx (s-round ctx))))

        (defn tonic-equivalent?
          "the current position is equivalent to a tonic one. "
          [ctx]
          (zero? (chromatic-distance ctx (t-round ctx))))

        (defn neibourhood
          [ctx]
          {:up {:c (let [ctx' (upd ctx c1)] (if-not (diatonic-equivalent? ctx') ctx'))
                :d (let [ctx' (upd ctx d1)] (if-not (structural-equivalent? ctx') ctx'))
                :s (let [ctx' (upd ctx s1)] (if-not (tonic-equivalent? ctx') ctx'))
                :t (upd ctx t1)}
           :down {:c (let [ctx' (upd ctx c1-)] (if-not (diatonic-equivalent? ctx') ctx'))
                  :d (let [ctx' (upd ctx d1-)] (if-not (structural-equivalent? ctx') ctx'))
                  :s (let [ctx' (upd ctx s1-)] (if-not (tonic-equivalent? ctx') ctx'))
                  :t (upd ctx t1-)}})

        (defn diatonic-suroundings
          "return the chromatic distances of the surroundings diatonic degrees
       [c-dist-downward c-dist-upward]"
          [ctx]
          [(chromatic-distance (upd ctx d1-) ctx)
           (chromatic-distance (upd ctx d1) ctx)]))

    (do :connections
        (defn connections
          "For each layer, computes the ctxs between hc1 and hc2
returns a map of kind {layer intermediate-ctxs}
layer: :t | :s | :d | :c
intermediate-ctxs: sorted ctxs that are between hc1 and hc2 on the corresponding layer."
          [hc1 hc2]
          (let [v1 (hc->chromatic-value hc1)
                v2 (hc->chromatic-value hc2)
                ascending (< v1 v2)
                in-bounds? (fn [x] (if ascending
                                     (> v2 (hc->chromatic-value x))
                                     (< v2 (hc->chromatic-value x))))
                passings (fn [layer]
                           (loop [current hc1 ret []]
                             (if (in-bounds? current)
                               (recur (upd current (layer-step layer (if ascending 1 -1)))
                                      (conj ret current))
                               (rest ret))))]
            {:c (passings :c)
             :d (passings :d)
             :s (passings :s)
             :t (passings :t)}))

        (defn chromatic-connection [hc1 hc2]
          (let [v1 (hc->chromatic-value hc1)
                v2 (hc->chromatic-value hc2)
                ascending (< v1 v2)
                in-bounds? (fn [x] (if ascending
                                     (> v2 (hc->chromatic-value x))
                                     (< v2 (hc->chromatic-value x))))]
            (loop [current (normalise hc1) ret []]
              (if (in-bounds? current)
                (recur (upd current (c-step (if ascending 1 -1)) c->t)
                       (conj ret current))
                (conj ret (normalise hc2))))))

        (defn simplest-connection
          "return a sequence of harmonic contexts representing a melodic line between `hc1` and `hc2` with `size` intermediate contexts.
           intermediate contexts are selected on lowset layer in priority."
          [size hc1 hc2]
          (if (zero? size)
            [hc1 hc2]
            (let [v1 (hc->chromatic-value hc1)
                  v2 (hc->chromatic-value hc2)
                  ascending (< v1 v2)
                  chrom-line (chromatic-connection hc1 hc2)
                  passing-notes (butlast (rest chrom-line))
                  max-size (count passing-notes)
                  split-by (fn [f xs] (reduce (fn [[a b] x] (if (f x) [(conj a x) b] [a (conj b x)]))
                                              [[] []] xs))
                  [t-passings xs] (split-by tonic-equivalent? passing-notes)
                  [s-passings xs] (split-by structural-equivalent? xs)
                  [d-passings c-passings] (split-by diatonic-equivalent? xs)
                  prio-passings (concat (reverse t-passings)
                                        (reverse s-passings)
                                        (reverse d-passings)
                                        (reverse c-passings))

                  return (fn [xs] (let [xs (sort-by hc->chromatic-value xs)]
                                    (conj (vec (cons (first chrom-line) (if ascending xs (reverse xs))))
                                          (last chrom-line))))]
              (cond (= max-size size) chrom-line
                    (> max-size size) (return (take size prio-passings))
                    :else nil))))

        (comment
          (neibourhood (upd (hc)
                            (position 0 0 0 1)))
          (connections (upd (hc) (position 2 1))
                       (upd (hc) (position 2 3)))
          (chromatic-connection
           (upd (hc) (position 2 1))
           (upd (hc) (position 2 3)))

          (simplest-connection
           3
           (upd (hc) (position 2 1))
           (upd (hc) (position 3 2)))

          (= (upd (hc)
                  (position 0 0 6)
                  normalise)

             (upd (hc)
                  (position 0 2 2)
                  normalise)

             (upd (hc)
                  (position 0 2 2)
                  normalise)

             (upd (hc)
                  (position 1 0 -1 0))))))

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
