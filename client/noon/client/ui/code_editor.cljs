(ns noon.client.ui.code-editor
  (:require [noon.eval :as eval]
            [noon.output.midi :as midi]
            [noon.score :as score]
            [noon.viz.piano-roll :as pr]
            [uix.core :as uix :refer [defui $]]
            [uic.component :refer [c sc]]
            [noon.utils.misc :as u]
            [clojure.string :as str]
            ["@uiw/react-codemirror" :default CodeMirror]
            ["@nextjournal/lang-clojure" :refer [clojure]]
            ["react-icons/vsc" :as icons-vsc]
            ["react-icons/tb" :as icons-tb :refer [TbPiano]]
            ["react-spinners/BeatLoader" :default spinner]
            ["@uiw/codemirror-themes-all" :as cm-themes]
            [noon.client.ui.misc :as ui.misc]
            ["react-highlight" :default Highlight]))

(def EDITOR_EXTENSIONS
  #js [(clojure)])

;; ── Theme system ─────────────────────────────────────────────────

(def themes
  {:light {:cm-theme        cm-themes/quietlight
           :accent          "lightskyblue"
           :accent-bg       "rgba(135,206,250,0.1)"
           :accent-bg-half  "rgba(135,206,250,0.05)"
           :accent-border   "rgba(135,206,250,0.2)"
           :bg              "#ffffff"
           :bg-secondary    "#f1f5f9"
           :bg-overlay      "rgba(255,255,255,0.5)"
           :text-primary    "#1e293b"
           :text-secondary  "#94a3b8"
           :text-tertiary   "#64748b"
           :text-muted      "#cbd5e1"
           :border          "#e2e8f0"
           :shadow          "0 1px 3px rgba(0,0,0,0.08)"
           :shadow-sm       "0 1px 2px rgba(0,0,0,0.04)"
           :highlight-bg    "#ffffff"
           :error-text      "rgba(239,68,68,0.45)"
           :error-bg        "rgba(239,68,68,0.05)"
           :error-border    "red"
           :spinner-color   "lightskyblue"
           :toggle-active-bg "#ffffff"
           :badge-color     "tomato"}
   :dark  {:cm-theme        cm-themes/materialDarker
           :accent          "#5eafd6"
           :accent-bg       "rgba(94,175,214,0.15)"
           :accent-bg-half  "rgba(94,175,214,0.08)"
           :accent-border   "rgba(94,175,214,0.25)"
           :bg              "#1e1e2e"
           :bg-secondary    "#2a2a3c"
           :bg-overlay      "rgba(30,30,46,0.6)"
           :text-primary    "#cdd6f4"
           :text-secondary  "#6c7086"
           :text-tertiary   "#a6adc8"
           :text-muted      "#45475a"
           :border          "#313244"
           :shadow          "0 1px 3px rgba(0,0,0,0.3)"
           :shadow-sm       "0 1px 2px rgba(0,0,0,0.2)"
           :highlight-bg    "#1e1e2e"
           :error-text      "rgba(243,139,168,0.7)"
           :error-bg        "rgba(243,139,168,0.08)"
           :error-border    "#f38ba8"
           :spinner-color   "#5eafd6"
           :toggle-active-bg "#313244"
           :badge-color     "#f38ba8"}})

(defn- resolve-theme [options]
  (get themes (keyword (or (:theme options) :light)) (:light themes)))

(defn- result->score
  "Extract a noon score from an eval result, if present.
   `play` returns a map with {:score <score>} in metadata.
   `score` returns the score directly."
  [result]
  (cond
    (score/score? result) result
    (some-> (meta result) :score) (:score (meta result))
    :else nil))

;; ── Mode toggle (Kind / Channel) ─────────────────────────────────

(defui mode-toggle [{:keys [value options on-change theme-colors]}]
  (let [t (or theme-colors (:light themes))]
    (sc {:flex [:row {:items :center}]
         :rounded 0.5
         :overflow :hidden
         :bg {:color (:bg-secondary t)}
         :p 0.15}
        (into []
              (map-indexed
               (fn [_i [k label]]
                 (c :button
                    {:key (name k)
                     :style {:p [0.25 0.6]
                             :min-width "52px"
                             :border {:width 0}
                             :rounded 0.4
                             :bg {:color (if (= k value) (:toggle-active-bg t) :transparent)}
                             :color (if (= k value) (:text-primary t) (:text-secondary t))
                             :cursor :pointer
                             :font-size "10px"
                             :font-weight 500
                             :font-family "'SF Mono', 'Fira Code', monospace"
                             :text-align :center
                             :transition "all 0.15s ease"
                             :box-shadow (when (= k value) (:shadow t))
                             :hover (when-not (= k value) {:color (:text-tertiary t)})}
                     :on-click #(on-change k)}
                    label)))
              options))))

;; ── Channel toggle pill ──────────────────────────────────────────

(defn- cycle-ch-state
  "Cycle a channel through :visible → :dimmed → :hidden → :visible.
   Prevents all channels from being hidden simultaneously."
  [ch-states ch all-channels]
  (let [current    (get ch-states ch :visible)
        next-state (case current :visible :dimmed, :dimmed :hidden, :hidden :visible)
        proposed   (assoc ch-states ch next-state)]
    (if (and (= next-state :hidden)
             (every? #(= :hidden (get proposed % :visible)) all-channels))
      (assoc ch-states ch :visible)
      proposed)))

(defui channel-toggle [{:keys [ch state color on-click theme-colors]}]
  (let [t (or theme-colors (:light themes))]
    (c :button
       {:style {:flex [:row {:items :center :gap 0.25}]
                :p [0.25 0.5]
                :border {:width 0}
                :rounded 0.4
                :bg {:color (case state
                              :visible (:toggle-active-bg t)
                              :dimmed  (:toggle-active-bg t)
                              (:bg-secondary t))}
                :color (case state
                         :visible (:text-primary t)
                         :dimmed  (:text-secondary t)
                         (:text-muted t))
                :cursor :pointer
                :font-size "10px"
                :font-weight 500
                :font-family "'SF Mono', 'Fira Code', monospace"
                :transition "all 0.15s ease"
                :box-shadow (case state
                              :visible (:shadow t)
                              :dimmed  (:shadow-sm t)
                              nil)
                :hover (when (not= state :visible) {:color (:text-tertiary t)})}
        :on-click on-click}
       (c :span {:style {:display :inline-block
                         :width "7px" :height "7px"
                         :border-radius "50%"
                         :background color
                         :opacity (case state :visible 1, :dimmed 0.4, 0.15)}})
       (str "ch " ch))))

;; ── Piano roll view ──────────────────────────────────────────────

(defui piano-roll-view [{:keys [score theme-colors]}]
  (let [t (or theme-colors (:light themes))
        dark? (= (:bg t) (:bg (:dark themes)))
        all-channels (pr/score->channels score)
        multi? (> (count all-channels) 1)
        [color-mode set-color-mode] (uix/use-state :kind)
        [ch-states set-ch-states] (uix/use-state {})
        [focus-ch set-focus-ch] (uix/use-state nil)]
    (sc {:overflow-x :auto
         :overflow-y :hidden
         :p [0.5 0]}
        ;; Toolbar (shown for multi-channel scores)
        (when multi?
          (sc {:flex [:row {:gap 0.5 :items :center}]
               :p [0.4 0.5]}
              ($ mode-toggle
                 {:value   color-mode
                  :options [[:kind "Kind"] [:channel "Channel"]]
                  :on-change set-color-mode
                  :theme-colors t})
              ;; Channel toggles (channel mode only)
              (when (= color-mode :channel)
                (sc {:flex [:row {:items :center}]
                     :rounded 0.5
                     :overflow :hidden
                     :bg {:color (:bg-secondary t)}
                     :p 0.15}
                    (mapv (fn [ch]
                            (let [state (get ch-states ch :visible)
                                  fill  (:fill (nth pr/channel-colors (mod ch 16)))]
                              ($ channel-toggle
                                 {:key ch :ch ch :state state
                                  :color fill
                                  :theme-colors t
                                  :on-click (fn []
                                              (set-ch-states #(cycle-ch-state % ch all-channels))
                                              (set-focus-ch ch))})))
                          all-channels)))))
        (let [visible-chs  (filterv #(= :visible (get ch-states % :visible)) all-channels)
              dimmed-chs   (filterv #(= :dimmed  (get ch-states % :visible)) all-channels)
              shown-chs    (into visible-chs dimmed-chs)
              ;; Channel order: focus-ch rendered last (on top)
              channel-order (when (and focus-ch (> (count shown-chs) 1))
                              (let [others (filterv #(not= % focus-ch) shown-chs)]
                                (conj others focus-ch)))
              channels     (when (and (= color-mode :channel) (seq shown-chs))
                             shown-chs)]
          (c :div {:style {:width "max-content"
                           :min-width "100%"}
                   :dangerouslySetInnerHTML
                   #js {:__html (ui.misc/hiccup->html
                                 (pr/piano-roll score
                                                (cond-> {:target-width 500
                                                         :color-mode color-mode
                                                         :dark? dark?}
                                                  channels      (assoc :channels channels)
                                                  (seq dimmed-chs) (assoc :dimmed-channels (set dimmed-chs))
                                                  channel-order (assoc :channel-order channel-order))))}})))))

(defui code-editor [{:keys [source options]}]
  (let [t (resolve-theme options)
        input-editor-ref (uix/use-ref)
        [source set-source] (uix/use-state source)
        [return set-return] (uix/use-state nil)
        [score* set-score] (uix/use-state nil)
        [editing set-editing] (uix/use-state false)
        [evaluating set-evaluating] (uix/use-state false)
        [playing set-playing] (uix/use-state false)
        [local-piano-roll set-local-piano-roll] (uix/use-state nil) ;; nil = follow option, true/false = override
        show-piano-roll? (if (some? local-piano-roll) local-piano-roll (:show-piano-roll? options))
        accent (:accent t)
        error? (:error return)]

    (sc :.code-editor

        {:m [0 :1em]
         :position :relative}

        (when (:clj-only options)
          (sc {:position [:absolute {:top 6 :right 6}]
               :z-index 100}
              (c ui.misc/badge
                 {:color (:badge-color t)
                  :size :sm
                  :text "clj-only"})))

        (sc :.code-editor_input

            {:border {:width 2
                      :color (:accent-border t)
                      :bottom (when return {:width 0})}
             :p 0
             :bg {:color (:bg t)}
             :flex [:row {:items :center}]}

            (c :.code-editor-input_left-button

               {:style {:flex :center
                        :bg {:color (:accent-bg t)}
                        :color (if evaluating (:text-secondary t) accent)
                        :p 1 :align-self :stretch}
                :on-click (if return
                            (fn [_]
                              (set-return nil)
                              (set-score nil)
                              (set-playing false)
                              (eval/stop))
                            (fn [_]
                              (if playing
                                (eval/stop)
                                (when-not evaluating
                                  (set-evaluating true)
                                  ;; this delay is needed for the set-evaluating call
                                  ;; to have effect before evaluation begins.
                                  (js/setTimeout (fn []
                                                   (eval/eval-string-async
                                                    source
                                                    (fn [x]
                                                      (println (some-> x :result))
                                                      (when-let [id (some-> x :result :id)]
                                                        (set-playing true)
                                                        (midi/on-done-playing id (fn [] (set-playing false))))
                                                      (set-score (some-> x :result result->score))
                                                      (set-evaluating false)
                                                      (set-return x))))
                                                 15)))))}
               (cond
                 playing (c icons-tb/TbPlayerStopFilled)
                 return (c icons-vsc/VscChevronUp)
                 :else (c icons-vsc/VscDebugStart)))

            (c :.code-editor-input_content

               {:style {:overflow :scroll
                        :position :relative
                        :width :full}
                :on-click (fn [_] (set-editing true))
                :on-blur (fn [_] (set-editing false))}

               (sc :.code-editor-input_overlay

                   {:z-index (if evaluating 10000 -1)
                    :position [:absolute [0 0 0 0]]
                    :size :full
                    :bg {:color (:bg-overlay t)}
                    :flex :center}

                   (c spinner {:color (:spinner-color t) :loading evaluating :size 15}))

               (if editing

                 (c CodeMirror
                    {:ref input-editor-ref
                     :value source
                     :on-change (fn [x] (set-source x))
                     :autoFocus true
                     :extensions EDITOR_EXTENSIONS
                     :theme (:cm-theme t)
                     :basic-setup #js {:lineNumbers false
                                       :foldGutter false
                                       :highlightActiveLine false}})
                 (sc {"pre" {:m 0 :p 0}
                      "code.hljs" {:bg {:color (:highlight-bg t)}
                                   :color (:text-primary t)}}
                     (c Highlight
                        {:class "clojure"}
                        (str source))))))
        (when return

          (if (and score* show-piano-roll?)

            ;; ── Piano roll output ──────────────────────────
            ;; Full-width block layout for horizontal scrolling.
            (sc :.code-editor-output

                {:border {:color (:accent-border t)
                          :width 2}
                 :bg {:color (:bg t)}
                 :p 0
                 :position :relative}

                (c :.code-editor-output_close

                   {:style {:position [:absolute {:top 4 :right 4}]
                            :z-index 10
                            :flex :center
                            :bg {:color :transparent}
                            :color accent
                            :border {:width 0}
                            :p 0.3
                            :cursor :pointer
                            :hover {:color (:badge-color t)}}
                    :on-click (fn [_]
                                (set-return nil)
                                (set-score nil)
                                (set-local-piano-roll nil))}
                   (c icons-vsc/VscClose))

                ($ piano-roll-view {:score score* :theme-colors t}))

            ;; ── Text output (errors / non-score results) ───
            (sc :code-editor-output

                {:border {:color (if error? (:error-border t) (:accent-border t))
                          :width 2}
                 :p 0
                 :flex [:row {:items :center}]}

                (c :.code-editor-output_left-button

                   {:style {:flex :center
                            :bg {:color (:bg t)}
                            :border {:right [2 (if error? (:error-border t) (:accent-border t))]}
                            :color (if error? (:error-border t) accent)
                            :p [1 0.5]
                            :align-self :stretch}
                    :on-click (fn [_]
                                (set-return nil)
                                (set-score nil)
                                (set-local-piano-roll nil))}
                   (c icons-vsc/VscClose))

                (sc :.code-editor-output_content

                    {:bg {:color (if error? (:error-bg t) (:accent-bg t))}
                     :color (when error? (:error-text t))
                     :p (if error? [0.5 0.3] [0.3 0.7])
                     :flexi [1 1 :auto]
                     :position :relative}

                    ;; Piano roll toggle button for score results
                    (when score*
                      (c :button
                         {:style {:position [:absolute {:top "50%" :right 8}]
                                  :transform "translateY(-50%)"
                                  :bg {:color (:bg t)}
                                  :border {:width 0}
                                  :outline :none
                                  :color (:text-secondary t)
                                  :p 0
                                  :cursor :pointer
                                  :flex :center
                                  :transition "all 0.15s ease"
                                  :hover {:color (:badge-color t)}}
                          :title "Show piano roll"
                          :on-click (fn [_] (set-local-piano-roll true))}
                         (c TbPiano {:size 18})))

                    (if editing

                      (c CodeMirror
                         {:value (str/trim (or (some-> return :error .-message)
                                               (u/pretty-str (:result return))))
                          :editable false
                          :extensions EDITOR_EXTENSIONS
                          :theme (:cm-theme t)
                          :basic-setup #js {:lineNumbers false
                                            :foldGutter false
                                            :highlightActiveLine false}})

                      (sc {"pre" {:m 0 :p 0}
                           "code.hljs" {:bg {:color (:highlight-bg t)}
                                        :color (:text-primary t)}}
                          (c Highlight
                             {:class "clojure"}
                             (str (str/trim (or (some-> return :error .-message)
                                                (u/pretty-str (:result return)))))))))))))))
