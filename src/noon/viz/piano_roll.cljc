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

     ;; With a named palette
     (pr/piano-roll (score (tup s0 s1 s2))
                    {:palette :ember-ocean})

     ;; Grouped (multiple scores, stacked)
     (pr/piano-roll-group
       [{:label \"major\" :score (score (tup d0 d1 d2 d3 d4 d5 d6 d7))}
        {:label \"dorian\" :score (score (scale :dorian) (tup d0 d1 d2 d3 d4 d5 d6 d7))}]
       {:shared-pitch-range true :palette :purple-gold})

   Clay/Kindly:
     Output carries ^{:kindly/kind :kind/hiccup} metadata.
     In a Clay notebook, the result renders inline as an interactive SVG."
  (:require [noon.harmonic-context :as hc]
            [noon.score :as score]))

;; ── Constants ────────────────────────────────────────────────────

(def ^:private note-names
  ["C" "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯" "A" "A♯" "B"])

(def ^:private layer-labels
  {:tonic "Tonic" :structural "Structural" :diatonic "Diatonic" :chromatic "Chromatic"})

(def ^:private layer-order
  {:tonic 0 :structural 1 :diatonic 2 :chromatic 3})

(def palettes
  "Named color palettes for note layers.
   Each palette maps layer kind to {:fill :stroke}.
   Use via the :palette option in piano-roll / piano-roll-group."
  {:ocean        {:tonic      {:fill "#1a56db" :stroke "#1446b3"}
                  :structural {:fill "#3b82f6" :stroke "#2563eb"}
                  :diatonic   {:fill "#93c5fd" :stroke "#60a5fa"}
                  :chromatic  {:fill "#d1d5db" :stroke "#9ca3af"}}

   :indigo-teal  {:tonic      {:fill "#4f46e5" :stroke "#4338ca"}
                  :structural {:fill "#6366f1" :stroke "#4f46e5"}
                  :diatonic   {:fill "#2dd4bf" :stroke "#14b8a6"}
                  :chromatic  {:fill "#cbd5e1" :stroke "#94a3b8"}}

   :purple-gold  {:tonic      {:fill "#7c3aed" :stroke "#6d28d9"}
                  :structural {:fill "#a78bfa" :stroke "#8b5cf6"}
                  :diatonic   {:fill "#fbbf24" :stroke "#f59e0b"}
                  :chromatic  {:fill "#d1d5db" :stroke "#9ca3af"}}

   :rose-cyan    {:tonic      {:fill "#e11d48" :stroke "#be123c"}
                  :structural {:fill "#f472b6" :stroke "#ec4899"}
                  :diatonic   {:fill "#22d3ee" :stroke "#06b6d4"}
                  :chromatic  {:fill "#cbd5e1" :stroke "#94a3b8"}}

   :ember-ocean  {:tonic      {:fill "#ea580c" :stroke "#c2410c"}
                  :structural {:fill "#fb923c" :stroke "#f97316"}
                  :diatonic   {:fill "#38bdf8" :stroke "#0ea5e9"}
                  :chromatic  {:fill "#d1d5db" :stroke "#9ca3af"}}

   :forest-berry {:tonic      {:fill "#059669" :stroke "#047857"}
                  :structural {:fill "#34d399" :stroke "#10b981"}
                  :diatonic   {:fill "#c084fc" :stroke "#a855f7"}
                  :chromatic  {:fill "#cbd5e1" :stroke "#94a3b8"}}

   :sapphire-amber {:tonic      {:fill "#1d4ed8" :stroke "#1e40af"}
                    :structural {:fill "#60a5fa" :stroke "#3b82f6"}
                    :diatonic   {:fill "#fbbf24" :stroke "#f59e0b"}
                    :chromatic  {:fill "#d1d5db" :stroke "#9ca3af"}}

   :slate-coral  {:tonic      {:fill "#475569" :stroke "#334155"}
                  :structural {:fill "#94a3b8" :stroke "#64748b"}
                  :diatonic   {:fill "#fb7185" :stroke "#f43f5e"}
                  :chromatic  {:fill "#e2e8f0" :stroke "#cbd5e1"}}})

(def ^:private harmony-bg "rgba(99, 102, 241, 0.06)")
(def ^:private harmony-line "rgba(99, 102, 241, 0.25)")

(def ^:private max-row-h 18)
(def ^:private min-row-h 8)
(def ^:private target-grid-h 350.0)
(def ^:private kb-w 44)
(def ^:private note-pad 1.5)

(def ^:private mono-font "'SF Mono', 'Fira Code', 'Menlo', monospace")
(def ^:private sans-font "-apple-system, 'Helvetica Neue', sans-serif")

(def ^:private default-opts
  {:target-width   500
   :show-keyboard  true
   :show-harmonies true
   :show-legend    true
   :padding        1
   :palette        :ocean})

;; ── Helpers ──────────────────────────────────────────────────────

(defn- black-key? [midi]
  (contains? #{1 3 6 8 10} (mod midi 12)))

(defn- note-name [midi]
  (str (nth note-names (mod midi 12))
       (- (quot midi 12) 1)))

(defn- ceil [x]
  #?(:clj  (long (Math/ceil (double x)))
     :cljs (js/Math.ceil x)))

(defn- resolve-colors
  "Resolve :palette option to a colors map.
   Accepts a keyword (palette name) or a map (custom colors)."
  [palette]
  (cond
    (keyword? palette) (or (get palettes palette)
                           (throw (ex-info (str "Unknown palette: " palette
                                                ". Available: " (keys palettes))
                                           {:palette palette})))
    (map? palette)     palette
    :else              (get palettes :ocean)))

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
                            (hc/tonic-equivalent? p)      :tonic
                            (hc/structural-equivalent? p)  :structural
                            (hc/diatonic-equivalent? p)    :diatonic
                            :else                          :chromatic)}))))

(defn score->harmonies
  "Extract harmony boundary segments from a score.
   Returns [{:position :duration} ...], one per harmonic context span.
   Groups by scale, structure, and diatonic origin — chromatic
   transpositions within the same harmony don't create new segments."
  [s]
  (let [event->harmony (fn [e]
                         (let [{:keys [scale structure origin]} (:pitch e)]
                           [scale structure (:d origin)]))]
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

(defn- compute-layout
  "Derive all spatial layout values from notes and options.
   Returns a map used by all rendering functions.
   Includes resolved :colors for renderers that need them."
  [notes {:keys [target-width show-keyboard padding palette]}]
  (let [pitches   (mapv :pitch notes)
        min-pitch (- (apply min pitches) padding)
        max-pitch (+ (apply max pitches) padding 1)
        n-rows    (- max-pitch min-pitch)
        row-h     (min max-row-h (max min-row-h (/ target-grid-h n-rows)))
        max-time  (apply max (map #(+ (:position %) (:duration %)) notes))
        ts        (if (zero? max-time)
                    400
                    (min (max (/ target-width max-time) 150) 700))
        x0        (if show-keyboard kb-w 0)
        grid-w    (max (ceil (* max-time ts)) 60)
        grid-h    (* n-rows row-h)]
    {:min-pitch  min-pitch
     :max-pitch  max-pitch
     :max-time   max-time
     :time-scale ts
     :x0         x0
     :row-h      row-h
     :grid-w     grid-w
     :n-rows     n-rows
     :grid-h     grid-h
     :svg-w      (+ x0 grid-w 1)
     :svg-h      (+ grid-h 1)
     :colors     (resolve-colors palette)}))

;; ── SVG renderers ────────────────────────────────────────────────
;; Each takes a layout map as first arg, plus its own data when needed.

(defn- svg-grid-rows
  "Background rows — white for natural keys, light grey for sharps/flats."
  [{:keys [min-pitch max-pitch x0 grid-w row-h]}]
  (into [:g]
        (mapcat
         (fn [p]
           (let [y (* (- max-pitch p 1) row-h)]
             [[:rect {:x x0 :y y :width grid-w :height row-h
                      :fill (if (black-key? p) "#f5f5f5" "#fff")}]
              [:line {:x1 x0 :y1 (+ y row-h) :x2 (+ x0 grid-w 1) :y2 (+ y row-h)
                      :stroke (if (= 11 (mod p 12)) "#d4d4d4" "#eeeeee")
                      :stroke-width 0.5}]])))
        (range min-pitch max-pitch)))

(defn- svg-harmonies
  "Alternating tint + dashed vertical lines at harmony boundaries."
  [{:keys [x0 time-scale grid-h]} harmonies]
  (when (> (count harmonies) 1)
    (into [:g]
          (keep-indexed
           (fn [i h]
             (let [x (+ x0 (* (:position h) time-scale))
                   w (* (:duration h) time-scale)
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
  [{:keys [min-pitch max-pitch row-h]}]
  (into [:g]
        (mapcat
         (fn [p]
           (let [y   (* (- max-pitch p 1) row-h)
                 blk (black-key? p)]
             (cond-> [[:rect {:x 0 :y y :width kb-w :height row-h
                              :fill (if blk "#374151" "#f9fafb")
                              :stroke "#d1d5db" :stroke-width 0.5}]]
               (or (zero? (mod p 12)) (= p min-pitch))
               (conj [:text {:x (- kb-w 5) :y (+ y (/ row-h 2) 3.5)
                             :text-anchor "end"
                             :font-size (min 8.5 (- row-h 2))
                             :font-family mono-font
                             :fill (if blk "#e5e7eb" "#6b7280")}
                      (note-name p)])))))
        (range min-pitch max-pitch)))

(defn- svg-notes
  "Colored, rounded note rectangles with optional pitch labels."
  [{:keys [x0 time-scale max-pitch row-h colors]} notes]
  (let [note-pad (min note-pad (* row-h 0.1))]
    (into [:g]
          (mapcat
           (fn [note]
             (let [x     (+ x0 (* (:position note) time-scale))
                   y     (+ (* (- max-pitch (:pitch note) 1) row-h) note-pad)
                   w     (max (- (* (:duration note) time-scale) 1) 3)
                   h     (- row-h (* note-pad 2))
                   color (get colors (:kind note) (:chromatic colors))]
               (cond-> [[:rect {:x x :y y :width w :height h
                                :rx 2.5 :ry 2.5
                                :fill (:fill color) :stroke (:stroke color)
                                :stroke-width 0.75 :opacity 0.92}]]
                 (> w 24)
                 (conj [:text {:x (+ x (/ w 2)) :y (+ y (/ h 2) 3)
                               :text-anchor "middle"
                               :font-size (min 8 (- row-h 2))
                               :font-family mono-font
                               :fill (if (#{:tonic :structural} (:kind note)) "#fff" "#475569")
                               :font-weight 500}
                        (nth note-names (mod (:pitch note) 12))])))))
          notes)))

(defn- svg-border
  "Thin border around the grid area."
  [{:keys [x0 grid-w grid-h]}]
  [:rect {:x x0 :y 0 :width grid-w :height grid-h
          :fill "none" :stroke "#ddd" :stroke-width 1}])

;; ── HTML renderers ───────────────────────────────────────────────

(defn- legend
  "Color legend as an HTML div."
  [colors kinds]
  (let [sorted (sort-by layer-order kinds)]
    (into [:div {:style {:display "flex" :gap "14px" :margin-bottom "10px"
                         :font-size "10.5px" :color "#555"}}]
          (map (fn [kind]
                 [:div {:style {:display "flex" :align-items "center" :gap "4px"}}
                  [:div {:style {:width "10px" :height "10px" :border-radius "2px"
                                 :background (get-in colors [kind :fill])}}]
                  [:span {} (get layer-labels kind)]]))
          sorted)))

(defn- label-bar
  "Small monospace label above a roll."
  [text]
  [:div {:style {:font-size     "11px"
                 :font-weight   500
                 :color         "#444"
                 :font-family   mono-font
                 :margin-bottom "2px"}}
   text])

(defn- title-bar
  "Bold monospace title above a roll."
  [text]
  [:div {:style {:font-size     "13px"
                 :font-weight   600
                 :color         "#333"
                 :margin-bottom "6px"
                 :font-family   mono-font}}
   text])

(defn- separator []
  [:div {:style {:height "1px" :background "#e5e7eb" :margin "4px 0"}}])

(defn- wrap-hiccup
  "Outer wrapper div with kindly metadata."
  [& children]
  (with-meta
    (into [:div {:style {:display     "inline-block"
                         :padding     "20px"
                         :font-family sans-font}}]
          (remove nil?)
          children)
    {:kindly/kind :kind/hiccup}))

;; ── Roll assembly ────────────────────────────────────────────────

(defn- build-roll
  "Assemble one piano roll SVG from layout and data."
  [layout notes harmonies {:keys [show-keyboard show-harmonies]}]
  (let [{:keys [svg-w svg-h]} layout]
    [:svg {:xmlns   "http://www.w3.org/2000/svg"
           :width   svg-w :height svg-h
           :viewBox (str "0 0 " svg-w " " svg-h)
           :style   {:display "block"}}
     (svg-grid-rows layout)
     (when show-harmonies
       (svg-harmonies layout harmonies))
     (svg-border layout)
     (when show-keyboard
       (svg-keyboard layout))
     (svg-notes layout notes)]))

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
     :padding        — pitch range padding in semitones (default 1)
     :palette        — keyword (e.g. :ember-ocean) or custom colors map (default :ocean)
                       See `palettes` for available names."
  ([score] (piano-roll score {}))
  ([score opts]
   (let [{:keys [show-legend title]
          :as   opts} (merge default-opts opts)
         notes     (score->notes score)
         harmonies (score->harmonies score)
         _         (assert (seq notes) "Score has no pitched events")
         layout    (compute-layout notes opts)]
     (wrap-hiccup
      (when title (title-bar title))
      (when show-legend (legend (:colors layout) (distinct (map :kind notes))))
      (build-roll layout notes harmonies opts)))))

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
     :padding            — pitch range padding (default 1)
     :palette            — keyword (e.g. :ember-ocean) or custom colors map (default :ocean)
                           See `palettes` for available names."
  ([items] (piano-roll-group items {}))
  ([items opts]
   (let [{:keys [shared-pitch-range show-legend]
          :as   opts} (merge default-opts opts)

         all-data  (mapv (fn [{:keys [label score]}]
                           {:label     label
                            :notes     (score->notes score)
                            :harmonies (score->harmonies score)})
                         items)

         all-notes (into [] (mapcat :notes) all-data)
         _         (assert (seq all-notes) "No pitched events in any score")

         shared-layout (when shared-pitch-range
                         (compute-layout all-notes opts))

         colors    (:colors (or shared-layout (compute-layout all-notes opts)))

         rolls     (into [:div {:style {:display        "flex"
                                        :flex-direction "column"
                                        :gap            "6px"}}]
                         (map-indexed
                          (fn [i {:keys [label notes harmonies]}]
                            (let [layout (or shared-layout (compute-layout notes opts))]
                              [:div {}
                               (when (pos? i) (separator))
                               (when label (label-bar label))
                               (build-roll layout notes harmonies opts)])))
                         all-data)]

     (wrap-hiccup
      (when show-legend (legend colors (distinct (map :kind all-notes))))
      rolls))))
