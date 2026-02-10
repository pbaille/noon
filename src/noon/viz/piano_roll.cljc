(ns noon.viz.piano-roll
  "Piano roll visualization for noon scores.
   Produces hiccup SVG, usable standalone or as a Clay/Kindly renderer.

   Usage:
     (require '[noon.viz.piano-roll :as pr])
     (require '[noon.eval :refer [score]])

     ;; Single score
     (pr/piano-roll (score (tup s0 s1 s2)))

     ;; With options
     (pr/piano-roll (score (lin I IV V I) (each (tup s0 s1 s2)))
                    {:title \"I-IV-V-I\" :target-width 600})

     ;; Grouped (multiple scores, stacked)
     (pr/piano-roll-group
       [{:label \"major\" :score (score (tup d0 d1 d2 d3 d4 d5 d6 d7))}
        {:label \"dorian\" :score (score (scale :dorian) (tup d0 d1 d2 d3 d4 d5 d6 d7))}]
       {:shared-pitch-range true})

   Clay/Kindly:
     Output carries ^{:kindly/kind :kind/hiccup} metadata.
     In a Clay notebook, the result renders inline as an interactive SVG."
  (:require [noon.harmonic-context :as hc]
            [noon.score :as score]))

;; ── Constants ────────────────────────────────────────────────────

(def ^:private note-names
  ["C" "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯" "A" "A♯" "B"])

(def ^:private layer-colors
  {:tonic      {:fill "#1a56db" :stroke "#1446b3" :label "Tonic"}
   :structural {:fill "#3b82f6" :stroke "#2563eb" :label "Structural"}
   :diatonic   {:fill "#93c5fd" :stroke "#60a5fa" :label "Diatonic"}
   :chromatic  {:fill "#d1d5db" :stroke "#9ca3af" :label "Chromatic"}})

(def ^:private layer-order
  {:tonic 0 :structural 1 :diatonic 2 :chromatic 3})

(def ^:private harmony-bg "rgba(99, 102, 241, 0.06)")
(def ^:private harmony-line "rgba(99, 102, 241, 0.25)")

;; ── Helpers ──────────────────────────────────────────────────────

(defn- black-key? [midi]
  (contains? #{1 3 6 8 10} (mod midi 12)))

(defn- note-name [midi]
  (str (nth note-names (mod midi 12))
       (- (quot midi 12) 1)))

(defn- ceil [x]
  #?(:clj  (long (Math/ceil (double x)))
     :cljs (js/Math.ceil x)))

;; ── Score → data ─────────────────────────────────────────────────

(defn score->notes
  "Convert a noon score to a vector of note maps:
   {:position :duration :pitch (MIDI int) :channel :kind (:tonic/:structural/:diatonic/:chromatic)}"
  [s]
  (->> (filter :pitch s)
       (sort-by :position)
       (mapv (fn [{:as e, p :pitch}]
               {:position (double (:position e))
                :duration (double (:duration e))
                :channel  (:channel e)
                :pitch    (hc/hc->chromatic-value p)
                :kind     (cond
                            (hc/tonic-equivalent? p)     :tonic
                            (hc/structural-equivalent? p) :structural
                            (hc/diatonic-equivalent? p)  :diatonic
                            :else                        :chromatic)}))))

(defn score->harmonies
  "Extract harmony boundary segments from a score.
   Returns [{:position :duration} ...], one per harmonic context span."
  [s]
  (let [event->harmony (fn [e] (dissoc (:pitch e) :position))]
    (->> (filter :pitch s)
         (sort-by :position)
         (partition-by event->harmony)
         (mapv (fn [xs]
                 (let [pos (:position (first xs))
                       end (apply max (map #(+ (:position %) (:duration %)) xs))]
                   {:position (double pos)
                    :duration (double (- end pos))}))))))

(defn score->data
  "Convert a score to {:notes [...] :harmonies [...]}."
  [s]
  {:notes     (score->notes s)
   :harmonies (score->harmonies s)})

;; ── Layout computation ───────────────────────────────────────────

(defn- pitch-bounds
  "Compute {:min :max} pitch range from notes, with padding."
  [notes padding]
  (let [pitches (mapv :pitch notes)]
    {:min (- (apply min pitches) padding)
     :max (+ (apply max pitches) padding 1)}))

(defn- time-scale
  "Compute pixels-per-beat so the roll fits target-width."
  [notes target-width]
  (let [max-time (apply max (map #(+ (:position %) (:duration %)) notes))]
    (if (zero? max-time)
      400
      (min (max (/ target-width max-time) 150) 700))))

;; ── SVG building blocks ──────────────────────────────────────────

(def ^:private row-h 18)
(def ^:private kb-w 44)
(def ^:private note-pad 1.5)

(defn- pitch-y
  "Y coordinate for a pitch row (higher pitch = lower Y)."
  [max-pitch pitch]
  (* (- max-pitch pitch 1) row-h))

(defn- svg-grid-rows
  "Background rows — white for natural keys, light grey for sharps/flats."
  [min-pitch max-pitch x0 grid-w]
  (into [:g]
        (mapcat
         (fn [p]
           (let [y (pitch-y max-pitch p)]
             [[:rect {:x x0 :y y :width grid-w :height row-h
                      :fill (if (black-key? p) "#f5f5f5" "#fff")}]
              [:line {:x1 x0 :y1 (+ y row-h) :x2 (+ x0 grid-w 1) :y2 (+ y row-h)
                      :stroke (if (= 11 (mod p 12)) "#d4d4d4" "#eeeeee")
                      :stroke-width 0.5}]])))
        (range min-pitch max-pitch)))

(defn- svg-harmonies
  "Alternating tint + dashed vertical lines at harmony boundaries."
  [harmonies x0 ts grid-h]
  (when (> (count harmonies) 1)
    (into [:g]
          (keep-indexed
           (fn [i h]
             (let [x (+ x0 (* (:position h) ts))
                   w (* (:duration h) ts)
                   children (cond-> []
                              (odd? i)
                              (conj [:rect {:x x :y 0 :width w :height grid-h
                                            :fill harmony-bg}])
                              (pos? i)
                              (conj [:line {:x1 x :y1 0 :x2 x :y2 grid-h
                                            :stroke harmony-line :stroke-width 1
                                            :stroke-dasharray "4,3"}]))]
               (when (seq children)
                 (into [:g] children)))))
          harmonies)))

(defn- svg-keyboard
  "Piano keyboard labels on the left edge."
  [min-pitch max-pitch]
  (into [:g]
        (mapcat
         (fn [p]
           (let [y   (pitch-y max-pitch p)
                 blk (black-key? p)]
             (cond-> [[:rect {:x 0 :y y :width kb-w :height row-h
                              :fill (if blk "#374151" "#f9fafb")
                              :stroke "#d1d5db" :stroke-width 0.5}]]
               (or (zero? (mod p 12)) (= p min-pitch))
               (conj [:text {:x (- kb-w 5) :y (+ y (/ row-h 2) 3.5)
                             :text-anchor "end"
                             :font-size 8.5
                             :font-family "'SF Mono', 'Fira Code', monospace"
                             :fill (if blk "#e5e7eb" "#6b7280")}
                      (note-name p)])))))
        (range min-pitch max-pitch)))

(defn- svg-notes
  "Colored, rounded note rectangles with optional pitch labels."
  [notes x0 ts max-pitch]
  (into [:g]
        (mapcat
         (fn [note]
           (let [x     (+ x0 (* (:position note) ts))
                 y     (+ (pitch-y max-pitch (:pitch note)) note-pad)
                 w     (max (- (* (:duration note) ts) 1) 3)
                 h     (- row-h (* note-pad 2))
                 color (get layer-colors (:kind note) (:chromatic layer-colors))]
             (cond-> [[:rect {:x x :y y :width w :height h
                              :rx 2.5 :ry 2.5
                              :fill (:fill color) :stroke (:stroke color)
                              :stroke-width 0.75 :opacity 0.92}]]
               (> w 24)
               (conj [:text {:x (+ x (/ w 2)) :y (+ y (/ h 2) 3)
                             :text-anchor "middle"
                             :font-size 8
                             :font-family "'SF Mono', 'Fira Code', monospace"
                             :fill (if (#{:tonic :structural} (:kind note)) "#fff" "#475569")
                             :font-weight 500}
                      (nth note-names (mod (:pitch note) 12))])))))
        notes))

(defn- svg-legend
  "Color legend as an HTML div (not SVG)."
  [kinds]
  (let [sorted (sort-by layer-order kinds)]
    (into [:div {:style {:display "flex" :gap "14px" :margin-bottom "10px"
                         :font-size "10.5px" :color "#555"}}]
          (map (fn [kind]
                 [:div {:style {:display "flex" :align-items "center" :gap "4px"}}
                  [:div {:style {:width "10px" :height "10px" :border-radius "2px"
                                 :background (get-in layer-colors [kind :fill])}}]
                  [:span {} (get-in layer-colors [kind :label])]]))
          sorted)))

;; ── Roll assembly ────────────────────────────────────────────────

(defn- build-roll
  "Assemble one piano roll SVG from pre-computed data."
  [notes harmonies {:keys [min-pitch max-pitch time-scale show-keyboard show-harmonies]}]
  (let [x0       (if show-keyboard kb-w 0)
        max-time (apply max (map #(+ (:position %) (:duration %)) notes))
        grid-w   (max (ceil (* max-time time-scale)) 60)
        n-rows   (- max-pitch min-pitch)
        grid-h   (* n-rows row-h)
        w        (+ x0 grid-w 1)
        h        (+ grid-h 1)]
    [:svg {:xmlns   "http://www.w3.org/2000/svg"
           :width   w :height h
           :viewBox (str "0 0 " w " " h)
           :style   {:display "block"}}
     ;; Grid background
     (svg-grid-rows min-pitch max-pitch x0 grid-w)
     ;; Harmony boundaries
     (when show-harmonies
       (svg-harmonies harmonies x0 time-scale grid-h))
     ;; Grid border
     [:rect {:x x0 :y 0 :width grid-w :height grid-h
             :fill "none" :stroke "#ddd" :stroke-width 1}]
     ;; Keyboard
     (when show-keyboard
       (svg-keyboard min-pitch max-pitch))
     ;; Notes
     (svg-notes notes x0 time-scale max-pitch)]))

;; ── Public API ───────────────────────────────────────────────────

(defn piano-roll
  "Render a noon score as a piano roll.

   Returns hiccup [:div ...] with ^{:kindly/kind :kind/hiccup} metadata.

   Options:
     :target-width   — pixel width for time axis (default 500)
     :show-keyboard  — piano keys on left edge (default true)
     :show-harmonies — harmony boundary markers (default true)
     :show-legend    — color legend above roll (default true)
     :title          — optional title string
     :padding        — pitch range padding in semitones (default 1)"
  [score & [{:keys [target-width show-keyboard show-harmonies show-legend title padding]
             :or   {target-width   500
                    show-keyboard  true
                    show-harmonies true
                    show-legend    true
                    padding        1}
             :as   opts}]]
  (let [notes     (score->notes score)
        harmonies (score->harmonies score)
        _         (assert (seq notes) "Score has no pitched events")
        bounds    (pitch-bounds notes padding)
        ts        (time-scale notes target-width)
        kinds     (distinct (map :kind notes))]
    (with-meta
      [:div {:style {:display     "inline-block"
                     :padding     "20px"
                     :font-family "-apple-system, 'Helvetica Neue', sans-serif"}}
       (when title
         [:div {:style {:font-size     "13px"
                        :font-weight   600
                        :color         "#333"
                        :margin-bottom "6px"
                        :font-family   "'SF Mono', 'Fira Code', 'Menlo', monospace"}}
          title])
       (when show-legend
         (svg-legend kinds))
       (build-roll notes harmonies
                   {:min-pitch      (:min bounds)
                    :max-pitch      (:max bounds)
                    :time-scale     ts
                    :show-keyboard  show-keyboard
                    :show-harmonies show-harmonies})]
      {:kindly/kind :kind/hiccup})))

(defn piano-roll-group
  "Render multiple labeled scores as a stacked group.

   `items` — seq of {:label \"...\" :score <noon-score>}

   Returns hiccup [:div ...] with ^{:kindly/kind :kind/hiccup} metadata.

   Options:
     :shared-pitch-range — use same vertical range for all (default false)
     :target-width       — pixel width (default 500)
     :show-keyboard      — show piano keyboards (default true)
     :show-harmonies     — show harmony boundaries (default true)
     :show-legend        — show shared legend (default true)
     :padding            — pitch range padding (default 1)"
  [items & [{:keys [shared-pitch-range target-width show-keyboard show-harmonies show-legend padding]
             :or   {shared-pitch-range false
                    target-width       500
                    show-keyboard      true
                    show-harmonies     true
                    show-legend        true
                    padding            1}}]]
  (let [all-data      (mapv (fn [{:keys [label score]}]
                              (let [notes     (score->notes score)
                                    harmonies (score->harmonies score)]
                                {:label label :notes notes :harmonies harmonies}))
                            items)
        all-notes     (into [] (mapcat :notes) all-data)
        _             (assert (seq all-notes) "No pitched events in any score")
        kinds         (distinct (map :kind all-notes))
        ts            (time-scale all-notes target-width)
        shared-bounds (when shared-pitch-range
                        (pitch-bounds all-notes padding))]
    (with-meta
      [:div {:style {:display     "inline-block"
                     :padding     "20px"
                     :font-family "-apple-system, 'Helvetica Neue', sans-serif"}}
       (when show-legend
         (svg-legend kinds))
       (into [:div {:style {:display        "flex"
                            :flex-direction "column"
                            :gap            "6px"}}]
             (map-indexed
              (fn [i {:keys [label notes harmonies]}]
                [:div {}
                 (when (pos? i)
                   [:div {:style {:height     "1px"
                                  :background "#e5e7eb"
                                  :margin     "4px 0"}}])
                 [:div {:style {:font-size     "11px"
                                :font-weight   500
                                :color         "#444"
                                :font-family   "'SF Mono', 'Fira Code', 'Menlo', monospace"
                                :margin-bottom "2px"}}
                  label]
                 (let [bounds (or shared-bounds (pitch-bounds notes padding))]
                   (build-roll notes harmonies
                               {:min-pitch      (:min bounds)
                                :max-pitch      (:max bounds)
                                :time-scale     ts
                                :show-keyboard  show-keyboard
                                :show-harmonies show-harmonies}))])
              all-data))]
      {:kindly/kind :kind/hiccup})))
