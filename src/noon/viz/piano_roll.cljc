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

     ;; Filter to specific channels
     (pr/piano-roll (score (chans (tup s0 s1 s2) (tup d0 d1 d2)))
                    {:channels #{0}})

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

(def ^:private layer-labels
  {:tonic "Tonic" :structural "Structural" :diatonic "Diatonic" :chromatic "Chromatic"})

(def ^:private layer-order
  {:tonic 0 :structural 1 :diatonic 2 :chromatic 3})

;; ── Channel coloring ─────────────────────────────────────────────
;; Each MIDI channel gets a base hue; note kind (tonic/structural/
;; diatonic/chromatic) is expressed through saturation & lightness.

(def ^:private default-channel-hues
  "16 maximally-spaced hues for MIDI channels 0-15.
   First 8 optimized for common multi-channel use."
  [220 10 145 45 280 175 335 80
   250 110 350 90 310 200 55 160])

(defn- hsl [h s l]
  (str "hsl(" h ", " s "%, " l "%)"))

(defn- kind-shades
  "Generate fill/stroke colors for all 4 note kinds from a single hue.
   Tonic is darkest/most saturated, chromatic is lightest/most desaturated."
  [hue]
  {:tonic      {:fill (hsl hue 72 38) :stroke (hsl hue 80 28)}
   :structural {:fill (hsl hue 60 50) :stroke (hsl hue 68 42)}
   :diatonic   {:fill (hsl hue 45 66) :stroke (hsl hue 52 58)}
   :chromatic  {:fill (hsl hue 15 78) :stroke (hsl hue 20 70)}})

(defn- resolve-colors
  "Build a channel-indexed color map: {channel -> {kind -> {:fill :stroke}}}.
   `hues` is a vector of HSL hue degrees (one per channel index).
   `channels` is the collection of channel numbers present."
  [hues channels]
  (into {}
        (map (fn [ch]
               [ch (kind-shades (nth hues (mod ch (count hues))))]))
        channels))

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
   :channels       nil    ;; nil = all channels, or set of ints e.g. #{0 2}
   :hues           nil})  ;; nil = default-channel-hues, or custom vector of HSL degrees

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
   Includes channel-indexed :colors map."
  [notes {:keys [target-width show-keyboard padding hues]}]
  (let [pitches   (mapv :pitch notes)
        channels  (distinct (map :channel notes))
        hue-vec   (or hues default-channel-hues)
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
     :channels   (vec (sort channels))
     :colors     (resolve-colors hue-vec channels)}))

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
  "Colored, rounded note rectangles with optional pitch labels.
   Color is determined by channel (hue) × kind (shade)."
  [{:keys [x0 time-scale max-pitch row-h colors]} notes]
  (let [note-pad (min note-pad (* row-h 0.1))]
    (into [:g]
          (mapcat
           (fn [note]
             (let [x     (+ x0 (* (:position note) time-scale))
                   y     (+ (* (- max-pitch (:pitch note) 1) row-h) note-pad)
                   w     (max (- (* (:duration note) time-scale) 1) 3)
                   h     (- row-h (* note-pad 2))
                   color (get-in colors [(:channel note) (:kind note)]
                                (get-in colors [(:channel note) :chromatic]
                                        {:fill "#d1d5db" :stroke "#9ca3af"}))]
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
  "Color legend as an HTML div.
   Single channel: shows kind swatches (tonic/structural/diatonic/chromatic).
   Multi-channel: shows channel hue labels + kind shade explanation."
  [colors channels kinds]
  (let [multi? (> (count channels) 1)]
    [:div {:style {:display "flex" :flex-direction "column" :gap "6px"
                   :margin-bottom "10px" :font-size "10.5px" :color "#555"}}
     ;; Channel row (multi-channel) or kind row (single channel)
     (if multi?
       ;; ── Multi-channel: one swatch per channel ──
       (into [:div {:style {:display "flex" :gap "12px" :align-items "center"}}]
             (map (fn [ch]
                    [:div {:style {:display "flex" :align-items "center" :gap "4px"}}
                     [:div {:style {:width "10px" :height "10px" :border-radius "2px"
                                    :background (get-in colors [ch :structural :fill])}}]
                     [:span {} (str "ch " ch)]]))
             (sort channels))
       ;; ── Single channel: kind swatches ──
       (let [ch (first channels)]
         (into [:div {:style {:display "flex" :gap "14px"}}]
               (map (fn [kind]
                      [:div {:style {:display "flex" :align-items "center" :gap "4px"}}
                       [:div {:style {:width "10px" :height "10px" :border-radius "2px"
                                      :background (get-in colors [ch kind :fill])}}]
                       [:span {} (get layer-labels kind)]]))
               (sort-by layer-order kinds))))
     ;; Kind shade hint for multi-channel
     (when multi?
       [:div {:style {:display "flex" :gap "10px" :font-size "9.5px" :color "#999"}}
        [:span {} "dark → tonic"]
        [:span {} "medium → structural"]
        [:span {} "light → diatonic"]
        [:span {} "grey → chromatic"]])]))

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

(defn score->channels
  "Return the sorted set of distinct MIDI channel numbers in a score."
  [s]
  (->> (filter :pitch s)
       (map :channel)
       distinct
       sort
       vec))

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
     :channels       — nil for all channels, or set/vec of channel numbers to show
     :hues           — nil for defaults, or vector of 16 HSL hue degrees"
  ([score] (piano-roll score {}))
  ([score opts]
   (let [{:keys [show-legend title channels]
          :as   opts} (merge default-opts opts)
         all-notes (score->notes score)
         notes     (if channels
                     (filterv #(contains? (set channels) (:channel %)) all-notes)
                     all-notes)
         harmonies (score->harmonies score)
         _         (assert (seq notes) "Score has no pitched events (after channel filter)")
         layout    (compute-layout notes opts)]
     (wrap-hiccup
      (when title (title-bar title))
      (when show-legend
        (legend (:colors layout) (:channels layout) (distinct (map :kind notes))))
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
     :channels           — nil for all channels, or set/vec of channel numbers to show
     :hues               — nil for defaults, or vector of 16 HSL hue degrees"
  ([items] (piano-roll-group items {}))
  ([items opts]
   (let [{:keys [shared-pitch-range show-legend channels]
          :as   opts} (merge default-opts opts)

         chan-set  (when channels (set channels))

         all-data  (mapv (fn [{:keys [label score]}]
                           (let [notes (cond->> (score->notes score)
                                         chan-set (filterv #(contains? chan-set (:channel %))))]
                             {:label     label
                              :notes     notes
                              :harmonies (score->harmonies score)}))
                         items)

         all-notes (into [] (mapcat :notes) all-data)
         _         (assert (seq all-notes) "No pitched events in any score (after channel filter)")

         shared-layout (when shared-pitch-range
                         (compute-layout all-notes opts))

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
                         all-data)

         ref (or shared-layout (compute-layout all-notes opts))]

     (wrap-hiccup
      (when show-legend
        (legend (:colors ref) (:channels ref) (distinct (map :kind all-notes))))
      rolls))))
