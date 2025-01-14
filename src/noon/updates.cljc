(ns noon.updates
  "Score updates and update-builders"
  (:refer-clojure :exclude [iter])
  (:require [noon.score :as score :refer [sfn sf_]]
            [noon.events :as events :refer [ef_]]
            [noon.numbers :as numbers]
            [noon.constants :as constants]
            [noon.utils.pseudo-random :as pr]
            [noon.utils.chance :as g]
            [noon.utils.misc :as u :refer [f_ t defn*]]
            [noon.utils.maps :as maps]
            [noon.harmonic-context :as h]
            [noon.vst.index :as vst])
  #?(:cljs (:require-macros [noon.updates :refer [!]])))

(do :event-updates

    (def ^{:tags [:event-update]} e0
      (t :event-update identity))

    (defn dur
      "Builds a :duration event-update based on `x`."
      {:tags [:event-update :temporal]}
      [x]
      (events/map->efn {:duration x}))

    (do :velocity
        (defn vel
          "Builds a :velocity event-update based on `x`."
          {:tags [:event-update]}
          [x]
          (ef_ (update _ :velocity
                       (fn [v] (numbers/->7bits-natural (maps/value-merge v x))))))

        (defn vel+
          "Builds an event update that adds `n` to :velocity value"
          {:tags [:event-update]}
          [n] (vel (numbers/add n)))

        (defn vel-
          "Builds an event update that substract `n` to :velocity value"
          {:tags [:event-update]}
          [n] (vel (numbers/sub n)))

        (defn vel-humanize
          "Build an event update that humanize the :velocity value.
           please refer to the `noon.score/humanize` doc."
          [max-step & [bounds]]
          (let [f (numbers/humanize {:bounds bounds :max-step max-step})]
            (ef_ (update _ :velocity f))))

        (def ^{:tags [:event-update]} vel0
          (vel 0)))

    (do :channel

        (defn chan
          "Builds a :velocity event-update based on `x`."
          {:tags [:event-update]}
          [x]
          (ef_ (update _ :channel
                       (fn [v] (numbers/->4bits-natural (maps/value-merge v x))))))

        (defn chan+
          "Builds an event-update that increase channel number by `x`"
          {:tags [:event-update]}
          [x] (chan (numbers/add x)))

        (defn chan-
          "Builds an event-update that decrease channel number by `x`"
          {:tags [:event-update]}
          [x] (chan (numbers/sub x))))

    (do :tracks

        (defn track
          "Builds an event-update that set the track number number to `x`"
          {:tags [:event-update]}
          [x]
          (ef_ (update _ :track
                       (fn [t] (numbers/->16bits-natural (maps/value-merge t x))))))

        (defn track+
          "Builds an event-update that increase track number number by `x`"
          {:tags [:event-update]}
          [x] (track (numbers/add x)))

        (defn track-
          "Builds an event-update that increase track number number by `x`"
          {:tags [:event-update]}
          [x] (track (numbers/sub x))))

    (do :voice

        (defn voice
          "Builds an event-update that set the voice number number to `x`"
          {:tags [:event-update]}
          [x]
          (ef_ (update _ :voice
                       (fn [v] (numbers/->4bits-natural (maps/value-merge v x))))))

        (defn voice+
          "Builds an event-update that increase track number number by `x`"
          {:tags [:event-update]}
          [x] (voice (numbers/add x)))

        (defn voice-
          "Builds an event-update that decrease track number number by `x`"
          {:tags [:event-update]}
          [x] (voice (numbers/sub x))))

    (defn cc
      "Build an event-update that adds the given control change to the received event.
       `key` is the control-change code, and `val` is the value for it."
      {:tags [:event-update]}
      [key val]
      (if-let [code (constants/cc-code key)]
        (events/map->efn {:cc {code (fn [v]
                                      (let [v (maps/value-merge v val)]
                                        (cond
                                          (number? v) (numbers/->7bits-natural v)
                                          (sequential? v) (mapv numbers/->7bits-natural v)
                                          :else (u/throw* "Bad value for event's :cc entry: " v))))}})
        (u/throw* "Unrecognised control change code: " key)))

    (defn pc
      "Builds an update that sets programs changes on the received event"
      {:tags [:event-update]}
      [& xs]
      (events/map->efn {:pc (vec xs)}))

    (defn patch
      "Build an event-update that change the patch of the received event."
      {:tags [:event-update]}
      ([x]
       (cond (keyword? x) (patch (vst/pick x))
             (vector? x) (ef_ (assoc _ :patch x))
             (number? x) (patch nil x)
             :else (u/throw* "noon.score/patch :: bad argument " x)))
      ([bank program]
       (patch [bank program])))

    (do :aliases

        (events/-def-durations)
        (events/-def-velocities)
        (events/-def-tracks)
        (events/-def-channels)

        (events/import-wrap-harmony-update-constructors
          ;; positions
          position s-position d-position c-position

          ;; intervals
          t-step s-step d-step c-step
          t-shift s-shift d-shift c-shift
          layer-step layer-shift

          ;; context tweaks
          origin scale structure degree root inversion
          repitch rescale restructure reorigin reroot redegree)

        (events/import-wrap-harmony-updates
          t-round t-ceil t-floor
          s-round s-ceil s-floor
          d-round d-ceil d-floor
          s+ s-)

        (defn transpose
          "Transpose the pitch origin of all events by the given update."
          {:tags [:event-update :harmonic]}
          [f]
          (assert (events/event-update? f) "transpose only takes event-update")
          (ef_ (let [new-origin (h/hc->pitch (:pitch (f ((position 0) _))))]
                 (assoc-in _ [:pitch :origin] new-origin))))

        (defn rebase
          "Applies the given transformations while preserving pitch."
          {:tags [:event-update :harmonic]}
          [& fs]
          (ef_
            (reduce #(%2 %1) _
                    (conj (vec fs)
                          (repitch (h/hc->pitch (:pitch _)))))))

        (events/-def-wrapped :structures structure)
        (events/-def-wrapped :modes scale)
        (events/-def-wrapped :pitches repitch)

        (do :intervals

            {:chromatic 'c
             :diatonic 'd
             :structural 's
             :tonic 't
             :octave 'o}

            (def ^{:tags [:alias :event-update]} c0 (c-step 0))
            (def ^{:tags [:alias :event-update]} d0 (d-step 0))
            (def ^{:tags [:alias :event-update]} s0 (s-step 0))
            (def ^{:tags [:alias :event-update]} t0 (t-step 0))

            (events/-def-steps "chromatic" "c" 37 c-step)
            (events/-def-steps "diatonic" "d" 22 d-step)
            (events/-def-steps "structural" "s" 13 s-step)
            (events/-def-steps "tonic" "t" 13 t-step)

            (events/-def-shifts "octave" "o" 9 (fn [i] (t-shift i :forced)))
            (events/-def-degrees))))

(do :score-updates

    (def ^{:doc "Identity transformation"
           :tags [:base :score-update]}
      same
      (sf_ _))

    (def ^{:doc "Identity transformation"
           :tags [:base :score-update]}
      _ same)

    (defn* k
      {:doc "Act like 'noon.score/score, ignoring current score."
       :tags [:base :score-update-builder]}
      [updates]
      (sf_ (score/score* updates)))

    (def ^{:doc "Returns the empty score regardless of input."
           :tags [:base :score-update]}
      void
      (sf_ #{}))

    (defn* chain
      {:doc "Compose several updates together linearly."
       :tags [:base :score-update-builder]}
      [updates]
      (score/chain-score-updates updates))

    (defn* par
      {:doc "Apply several update on a score merging the results."
       :tags [:base :score-update-builder :parallel]}
      [updates]
      (sf_ (score/merge-scores (map #(score/update-score _ %) updates))))

    (defn* par>
      {:doc "Accumulative 'par."
       :tags [:score-update-builder :accumulative :parallel]}
      [updates]
      (sf_ (loop [segments [_] updates updates]
             (if-let [[update & updates] updates]
               (recur (conj segments (score/update-score (peek segments) update)) updates)
               (score/merge-scores (next segments))))))

    (defn* each
      {:doc "Apply an update to each events of a score."
       :tags [:base :score-update-builder :traversing]}
      [updates]
      (sf_ (reduce score/map-update _ updates)))

    (defn* lin
      {:doc "Apply each `update` to the received score and concatenate the results."
       :tags [:base :score-update-builder :linear]}
      [updates]
      (sf_ (score/concat-scores
            (map #(score/update-score _ %) updates))))

    (defn* lin>
      {:doc "Accumulative 'lin."
       :tags [:base :score-update-builder :linear :accumulative]}
      [updates]
      (sf_ (loop [segments [_] updates updates]
             (if-let [[update & updates] updates]
               (recur (conj segments (score/update-score (peek segments) update)) updates)
               (score/concat-scores (next segments))))))

    (defn* fit
      {:doc "Wraps the given transformation 'x, stretching its output to the input score duration.
             In other words, turn any transformation into another one that do not change the duration of its input score."
       :tags [:base :score-update-builder]}
      [updates]
      (sf_ (score/fit-score (score/update-score _ (chain* updates))
                            {:duration (score/score-duration _)})))

    (defn* tup
      {:doc "Like 'lin but preserve the length of the input score"
       :tags [:base :score-update-builder :linear]}
      [updates] (fit (lin* updates)))

    (defn* tup>
      {:doc "Accumulative 'tup."
       :tags [:score-update-builder :accumulative :linear]}
      [updates] (fit (lin>* updates)))

    (defn* append
      {:doc "Like 'lin but insert the current score before."
       :tags [:base :score-update-builder :linear]}
      [updates]
      (lin* (cons same updates)))

    (defn* append>
      {:doc "Accumulative 'append."
       :tags [:score-update-builder :linear :accumulative]}
      [updates]
      (chain* (map append updates)))

    (defn* superpose
      {:doc "Like 'par but keep the current score."
       :tags [:base :score-update-builder :parallel]}
      [updates]
      (par* (cons same updates)))

    (defn* superpose>
      {:doc "Accumulative 'superpose."
       :tags [:score-update-builder :parallel :accumulative]}
      [updates]
      (chain* (map superpose updates)))

    (defn rep
      {:doc "Iterates the given `update` `n-times` over the input score and concat the results."
       :tags [:base :score-update-builder :linear :accumulative]}
      ([n-times update]
       (rep n-times update false))
      ([n-times update skip-first]
       (let [update (score/->score-update update)]
         (sf_ (->> (if skip-first (update _) _)
                   (iterate update)
                   (take n-times)
                   (score/concat-scores))))))

    (defn rup
      {:doc "Iterates the given `update` `n-times` over the input score and tup the results."
       :tags [:base :score-update-builder :linear :accumulative]}
      ([n-times update]
       (rup n-times update false))
      ([n-times update skip-first]
       (fit (rep n-times update skip-first))))

    (defn dup
      {:doc "Duplicate the received score `n-times` and concat the duplicates."
       :tags [:base :score-update-builder :linear :multiplicative]}
      [n-times]
      (sf_ (score/concat-scores (repeat n-times _))))

    (defn dupt
      {:doc "Duplicate received score `n-times` and tup the duplicates."
       :tags [:base :score-update-builder :linear :multiplicative]}
      [n-times]
      (fit (dup n-times)))

    (defn ntup
      {:doc "Creates a tup of size `n` using `update`."
       :tags [:base :score-update-builder :linear :multiplicative]}
      [n update] (tup* (repeat n update)))

    (defn nlin
      {:doc "Creates a lin of size `n` using `update`."
       :tags [:base :score-update-builder :linear :multiplicative]}
      [n update] (lin* (repeat n update)))

    (defn* parts
      {:doc "Apply updates to subscores: (parts sel1 upd1 sel2 upd2 ...)"
       :tags [:base :score-update-builder :partial]}
      [xs]
      (sf_ (reduce (fn [s [filt upd]]
                     (score/partial-update s filt upd))
                   _ (partition 2 xs))))

    (defn repeat-while
      {:doc "Iterate the given `update` while `test` is passing."
       :tags [:base :score-update-builder :iterative]}
      ([test update] (repeat-while test update same))
      ([test update after]
       (let [update (score/->score-update! update)
             test (score/->score-checker! test)
             after (score/->score-update! after)]
         (sf_ (let [nxt (update _)]
                (if (test nxt)
                  (recur nxt)
                  (after nxt)))))))

    (defn* fst
      {:doc "Tries given `updates` in order until the first success (non empty score)."
       :tags [:base :score-update-builder :selective]}
      [updates]
      (sf_ (loop [updates updates]
             (if-let [[update & updates] (seq updates)]
               (or (not-empty (score/update-score _ update))
                   (recur updates))))))

    (defn* fst-that
      {:doc "Tries given `updates` in order until one passes `test`."
       :tags [:base :score-update-builder :selective]}
      [test updates]
      (let [test (score/->score-checker! test)]
        (fst* (map (f_ (chain _ test))
                   updates))))

    (defn shrink
      {:doc "Shrink a score using `event-check` on each events to determine if it is kept or not."
       :tags [:base :score-update-builder :temporal]}
      [event-check]
      (sf_ (score/filter-score _ event-check)))

    (defn adjust
      {:doc "Time stretching/shifting operation syntax sugar over `noon.score/fit-score`."
       :tags [:base :score-update-builder :temporal]}
      [x]
      (let [opts (cond (map? x) x
                       (number? x) {:duration x}
                       (vector? x) {:duration (x 1) :position (x 0)})]
        (sf_ (score/fit-score _ opts))))

    (defn in-place
      {:doc "Turn the given update `u` into an update that reposition received score to position zero before applying `u` to it.
             The resulting score is then adjusted to its initial duration and shifted to its original position.
             This is useful when you need to scan update a score. It is similar to what the `noon.score/each` function is doing."
       :tags [:base :score-update-builder :temporal]}
      [u]
      (sf_ (let [score-origin (score/score-origin _)
                 score-duration (- (score/score-duration _) score-origin)]
             (score/update-score (score/shift-score _ (- score-origin))
                                 [u (adjust {:position score-origin :duration score-duration})]))))

    (defn* fork-with
      {:doc "Like `noon.score/par` but let you the opportunity to do something on the score based on the index of the branch before applying corresponding update."
       :tags [:score-update-builder]}
      [branch-idx->update branch-updates]
      (par* (map-indexed (fn [i update] (chain (branch-idx->update i) update))
                         branch-updates)))

    (defn* voices
      {:doc "Apply `updates` in parallel on subsequent voices."
       :tags [:base :score-update-builder :parallel]}
      [updates] (fork-with* voice+ updates))

    (defn* chans
      {:doc "Apply `updates` in parallel on subsequent midi channels."
       :tags [:base :score-update-builder :parallel]}
      [updates] (fork-with* chan+ updates))

    (defn* tracks
      {:doc "Apply `updates` in parallel on subsequent midi tracks."
       :tags [:base :score-update-builder :parallel]}
      [updates] (fork-with* track+ updates))

    (defn mirror
      {:doc "Mirrors all pitches around `pitch`."
       :tags [:harmonic :score-update-builder]}
      [pitch] (each {:pitch (h/mirror pitch)}))

    (def ^{:doc "Reverse the given score."
           :tags [:temporal :score-update]}
      rev
      (sf_ (score/reverse-score _)))

    (defn event-scale
      {:doc "Restrains and scale one event `dimension` to the given bounds over the whole score."
       :tags [:score-update-builder :scaling :bounding]}
      [dimension x]
      (let [[min-out max-out]
            (cond (number? x) [0 x]
                  (vector? x) x)]
        (sf_ (let [[min-in max-in] (mapv dimension (score/score-bounds _ dimension))
                   f #(u/scale-range % min-in max-in min-out max-out)]
               (score/update-score _ (each (events/ef_ (update _ dimension f))))))))

    (do :selection

        (defn min-by
          {:doc "Build an update that returns a one element score,
                 applying `f` to each event and selecting the event for which `f` returns the lowest value."
           :tags [:score-update-builder :selective]}
          [f]
          (sf_ #{(first (sort-by f _))}))

        (defn max-by
          {:doc "Build an update that returns a one element score,
                 applying `f` to each event and selecting the event for which `f` returns the greatest value."
           :tags [:score-update-builder :selective]}
          [f]
          (sf_ #{(last (sort-by f _))}))

        (def ^{:doc "Return a one event score, holding the lowest pitch of the received score."
               :tags [:score-update :selective :harmonic]}
          min-pitch (min-by events/pitch-value))

        (def ^{:doc "Return a one event score, holding the highest pitch of the received score."
               :tags [:score-update :selective :harmonic]}
          max-pitch (max-by events/pitch-value))

        (do :time

            "Updates to select time sections of a score."

            (defn from
              {:doc "Build an update that removes the elements anterior to the given position from the received score."
               :tags [:score-update-builder :temporal :selective]}
              [x]
              (shrink {:position (numbers/gte x)}))

            (defn until
              {:doc "Build an update that removes the elements posterior to the given position from the received score."
               :tags [:score-update-builder :temporal :selective]}
              [x]
              (shrink {:position (numbers/lt x)}))

            (defn between
              {:doc "Build an update that keeps only events that are positioned between x and y positions."
               :tags [:score-update-builder :temporal :selective]}
              [x y]
              (chain (from x) (until y)))

            (defn start-from
              {:doc "Build an update that shifts the score to the given position, removing all anterior events."
               :tags [:score-update-builder :temporal :selective]}
              [x]
              (chain (from x) {:position (numbers/sub x)}))

            (def ^{:doc "Shifting the score to last position erasing all anterior events."
                   :tags [:score-update :temporal :selective]}
              start-from-last
              (sf_ (-> (group-by :position _)
                       sort last val set
                       (score/update-score {:position 0}))))

            (defn start-from-nth-last
              {:doc "Shifting the score to `nth` last position erasing all anterior events."
               :tags [:score-update-builder :temporal :selective]}
              [nth]
              (sf_ (let [sorted (sort (group-by :position _))]
                     (if (>= (count sorted) nth)
                       (let [taken (map (comp set val) (take-last nth sorted))]
                         (score/update-score (score/merge-scores taken)
                                             {:position (numbers/sub (:position (ffirst taken)))}))))))

            (defn trim
              {:doc "Build and update that removes everything before `beg` and after `end` from the received score. (triming overlapping durations)."
               :tags [:score-update-builder :temporal :selective]}
              [beg end]
              (sf_ (score/trim-score _ (or beg 0) (or end (score/score-duration _)))))

            (defn only-between
              {:doc "Use `f` to update the subscore delimited by `beg` and `end` positions. Leave other events unchanged."
               :tags [:score-update-builder :temporal :selective]}
              [beg end f]
              (par [(trim beg end) (in-place f)]
                   (trim nil beg)
                   (trim end nil)))))

    (do :checks

        (defn within-bounds?
          {:doc "Build a check update (one that can return nil or the score unchanged) succeed if `event-fn` applied to each event is between `min` and `max`."
           :tags [:score-update-builder :check :bounding]}
          [event-fn min max]
          (score/->score-checker
           (fn [s] (every? (fn [e] (<= min (event-fn e) max))
                           s))))

        (defn within-time-bounds?
          {:doc "Build a check update (one that can return nil or the score unchanged) Succeed if all its events are between `start` and `end`."
           :tags [:score-update-builder :check :temporal]}
          [start end]
          (score/->score-checker
           (fn [s]
             (and (>= (score/score-origin s) start)
                  (<= (score/score-duration s) end)))))

        (defn within-pitch-bounds?
          {:doc "Build a check update (one that can return nil or the score unchanged)Succeed if all pitches are between `min` and `max`.`min` and `max` should be 'pitchable' (pitch map | pitch keyword | int)."
           :tags [:score-update-builder :check :harmonic]}
          [min max]
          (within-bounds? (comp h/hc->chromatic-value :pitch)
                          (:c (constants/get-pitch min))
                          (:c (constants/get-pitch max))))

        (def ^{:doc "Returns the score unchanged if every pitch within it are in the 0-127 MIDI range."
               :tags [:score-update :check :harmonic]}
          within-midi-pitch-bounds?
          (within-pitch-bounds? 0 127)))

    (do :non-determinism

        (defmacro !
          {:doc "Takes a non deterministic `expression` resulting in a score update.Returns a score update that wraps the `expression` so that it is evaluated each time the update is called."
           :tags [:score-update-builder :non-deterministic]}
          [expression]
          `(vary-meta (sfn score# (score/update-score score# ~expression))
                      assoc :non-deterministic true))

        (defn* one-of
          {:doc "Returns an update that choose randomly one of the given `updates` before applying it."
           :tags [:score-update-builder :non-deterministic]}
          [updates]
          (! (pr/rand-nth updates)))

        (defn* maybe
          {:doc "Like `noon.score/one-of`, return an update that choose randomly one of the given `updates`, but can also do nothing."
           :tags [:score-update-builder :non-deterministic]}
          [updates]
          (one-of* (cons same updates)))

        (defn probs
          {:doc "Takes a map of type {update number} where each key is an update and each value is its probability of occurence."
           :tags [:score-update-builder :non-deterministic]}
          [m]
          (let [pm (g/weighted m)]
            (! (pm))))

        (defn* any-that
          {:doc "Tries `updates` in random order until one passes `test`."
           :tags [:score-update-builder :non-deterministic :check]}
          [test updates]
          (! (fst-that* test (pr/shuffle updates))))

        (defn* mixtup
          {:doc "A tup that mix its elements."
           :tags [:score-update-builder :linear :non-deterministic]}
          [updates]
          (tup* (pr/shuffle updates)))

        (defn* shuftup
          {:doc "A tup that shuffles its elements everytime it is used."
           :tags [:score-update-builder :linear :non-deterministic]}
          [updates]
          (! (mixtup* updates)))

        (defn* mixlin
          {:doc "A lin that mix its elements."
           :tags [:score-update-builder :linear :non-deterministic]}
          [updates]
          (lin* (pr/shuffle updates)))

        (defn* shuflin
          {:doc "A lin that shuffles its elements everytime it is used."
           :tags [:score-update-builder :linear :non-deterministic]}
          [updates]
          (! (mixlin* updates)))

        #_(defn* shuffle-dimensions
            "Shuffles the values of the given dimensions."
            {:tags [:non-deterministic]}
            [dims]
            (sf_ (let [size (count _)
                       idxs (range size)
                       mappings (zipmap idxs (pr/shuffle idxs))
                       events (vec _)]
                   (reduce (fn [s i] (conj s (merge (events i) (select-keys (events (mappings i)) dims))))
                           #{}
                           idxs)))))

    (do :incubator

        (defn* voices>
          {:doc "Like `noon.score/par>` but keep track of voice number."
           :tags [:incubator :score-update-builder]}
          [updates]
          (par>* (map (fn [i update] (chain (voice+ i) update))
                      (range)
                      updates)))

        (defn nlin>
          {:doc "Creates a `noon.score/lin>` of size `n` using `update`."
           :tags [:incubator :score-update-builder]}
          [n update] (lin>* (repeat n update)))

        (defn ntup>
          {:doc "Creates a `noon.score/tup>` of size `n` using `update`."
           :tags [:incubator :score-update-builder]}
          [n update] (tup>* (repeat n update)))

        (defn fill
          {:doc "Fill the score using a `tup` of `update` of size (score-duration / `resolution`).
                 `resolution` should be an exact multiple of received score's duration."
           :tags [:incubator :score-update-builder :temporal :multiplicative]}
          [resolution update]
          (sf_ (let [sdur (score/score-duration _)
                     n (quot sdur resolution)]
                 (if-not (zero? (rem sdur resolution))
                   (u/throw* `fill " resolution should be a multiple of score length "))
                 (score/update-score _ (ntup n update)))))

        (defn fill>
          {:doc "Fill the score using an accumulative `tup>` of `update` of size (score-duration / `resolution`)
                 `resolution` should be an exact multiple of received score's duration."
           :tags [:incubator :score-update-builder :temporal :multiplicative]}
          [resolution update]
          (sf_ (let [sdur (score/score-duration _)]
                 (if-not #?(:clj (zero? (rem sdur resolution))
                            :cljs (numbers/is-multiple? sdur resolution))
                   (u/throw* `fill> " resolution should be a multiple of score length "
                             {:score-duration sdur :rem (rem sdur resolution)
                              :resolution resolution}))
                 (score/update-score _ (ntup> #?(:clj (quot sdur resolution)
                                                 :cljs (int (Math/round (/ sdur resolution))))
                                              update)))))

        (defn $by
          {:doc "Splits the score according to the return of `event->group` applied to each event.
                 apply `update` on each subscore and merge all the results together.
                 Before being updated, each subscore is repositioned to zero, and shifted back to its original position after."
           :tags [:incubator :score-update-builder]}
          [event->group update]
          (sf_ (->> (group-by event->group _)
                    (map (fn [[_ group]]
                           (let [s (set group)
                                 o (score/score-origin s)]
                             (-> (score/shift-score s (- o))
                                 (score/update-score update)
                                 (score/shift-score o)))))
                    score/merge-scores)))

        (defn zip
          {:doc "Zips the current score with the result of updating it with `update`.
                 the zipping is done by :position with `zip-fn` that takes two scores and produce one.
                 All the scores returned by `zip-fn` are merged into a final one which is returned."
           :tags [:incubator :score-update-builder]}
          [zip-fn update]
          (sf_ (let [updated (score/update-score _ update)]
                 (->> (group-by :position _)
                      (map (fn [[position xs]]
                             (assert (apply = (map :duration xs))
                                     "each position group should have events of the same duration.")
                             (let [duration (:duration (first xs))
                                   chunk (score/update-score updated (between position (+ position duration)))]
                               (zip-fn (set xs) chunk))))
                      score/merge-scores))))

        (defn try-until
          {:doc "Given an undeterministic `update`, tries it on the score until the result of it passes `test`"
           :tags [:incubator :score-update-builder]}
          [test update & {:keys [max] :or {max 100}}]
          (sf_ (loop [n 0]
                 (or (score/update-score _ (chain update test))
                     (if (>= max n)
                       (recur (inc n)))))))

        (defn newrep
          {:doc "INCUB: simple rep"
           :tags [:incubator :score-update-builder]}
          ([n] (newrep n same))
          ([n & xs]
           (let [[update flags] (if (keyword? (first xs)) [same xs] [(first xs) (rest xs)])
                 flags (zipmap flags (repeat true))
                 updates (repeat n update)]
             (cond (:par flags) (par* updates)
                   (:fit flags) (tup* updates)
                   :else (lin* updates)))))

        (defn iter
          {:doc "INCUB: accumulative rep"
           :tags [:incubator :score-update-builder]}
          [x & xs]
          (let [[n [f & {:as options}]] (if (number? x) [x xs] [nil (cons x xs)])]
            #_(println n f options)
            (sf_ (let [u (score/->score-update f)
                       seed (if (:next options) (score/update-score _ u) _)
                       scores (->> (iterate u seed)
                                   (drop (:drop options 0))
                                   (take (:take options n)))]
                   (cond (:par options) (score/merge-scores scores)
                         (:fit options) (score/fit-score (score/concat-scores scores) {:duration (score/score-duration _)})
                         :else (score/concat-scores scores))))))

        (defn connect-by
          {:doc "Build an update that use `f` to join successive score's chunks.
                 - Chunks the score with `noon.score/chunk-score` accordingly to `by`, resulting in a list of scores.
                 - Iterates this sorted list by pair, applying `f` to each one producing a new score.
                 - all those scores are merged together."
           :tags [:incubator :score-update-builder :traversing]}
          [by f]
          (sf_ (let [chunks (score/chunk-score _ by)]
                 (reduce (fn [s [n1 n2]]
                           (into s (f n1 n2)))
                         (last chunks) (partition 2 1 chunks)))))

        (defn scan
          {:doc "Chunk the score using the `by` function.
                 Chunks are partitioned by `size` and stepped by `step`.
                `f` is applied to each chunks partition and should return a single score.
                Resulting scores are merged together."
           :tags [:incubator :score-update-builder :traversing]}
          ([size f]
           (scan :position size size f))
          ([size step f]
           (scan :position size step f))
          ([by size step f]
           (sf_ (->> (score/chunk-score _ by)
                     (partition size step)
                     (map f)
                     (score/merge-scores)))))

        (defn scan>
          {:doc "Accumulative scan. Use `f` to accumulatively update time slices of given `size` of the score, stepping by `step`."
           :tags [:temporal :accumulative :iterative]}
          ([size f]
           (scan> size size f))
          ([size step f]
           (sf_ (reduce (fn [s from]
                          (score/update-score s (only-between from (+ from size) f)))
                        _ (range 0 (score/score-duration _) step)))))))
