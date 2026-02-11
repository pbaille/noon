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
            ["react-icons/tb" :as icons-tb]
            ["react-spinners/BeatLoader" :default spinner]
            ["@uiw/codemirror-themes-all" :as cm-themes]
            [noon.client.ui.misc :as ui.misc]
            ["react-highlight" :default Highlight]))

(def EDITOR_EXTENSIONS
  #js [(clojure)])

(def EDITOR_THEME cm-themes/quietlight)

(defn- result->score
  "Extract a noon score from an eval result, if present.
   `play` returns a map with {:score <score>} in metadata.
   `score` returns the score directly."
  [result]
  (cond
    (score/score? result) result
    (some-> (meta result) :score) (:score (meta result))
    :else nil))

(defui channel-pill [{:keys [ch selected on-click hue]}]
  (c :button
     {:style {:p [0.15 0.5]
              :text [:xs :medium]
              :font-family "'SF Mono', 'Fira Code', monospace"
              :border [1 (if selected :grey6 :grey3)]
              :rounded 0.5
              :bg {:color (if selected (str "hsl(" hue ", 60%, 50%)") :white)}
              :color (if selected :white :grey5)
              :cursor :pointer
              :font-size "10px"
              :hover {:border [1 :grey5]}}
      :on-click on-click}
     (str "ch " ch)))

(def ^:private default-channel-hues
  [220 10 145 45 280 175 335 80 250 110 350 90 310 200 55 160])

(defui piano-roll-view [{:keys [score]}]
  (let [all-channels (pr/score->channels score)
        multi? (> (count all-channels) 1)
        [hidden set-hidden] (uix/use-state #{})]
    (sc {:overflow-x :auto
         :overflow-y :hidden
         :p [0.5 0]}
        ;; Channel toggles for multi-channel scores
        (when multi?
          (sc {:flex [:row {:gap 0.3 :items :center}]
               :p [0.3 0.5]}
              (sc {:text [:xs] :color :grey4 :p {:right 0.3} :font-size "9px"}
                  "channels")
              (mapv (fn [ch]
                      (let [vis? (not (contains? hidden ch))]
                        ($ channel-pill
                           {:key ch :ch ch :selected vis?
                            :hue (nth default-channel-hues (mod ch 16))
                            :on-click #(set-hidden
                                        (if vis?
                                          (if (< (count hidden) (dec (count all-channels)))
                                            (conj hidden ch)
                                            hidden)
                                          (disj hidden ch)))})))
                    all-channels)))
        (let [visible (when multi?
                        (let [v (vec (remove hidden all-channels))]
                          (when (seq v) v)))]
          (c :div {:style {:width "max-content"
                           :min-width "100%"}
                   :dangerouslySetInnerHTML
                   #js {:__html (ui.misc/hiccup->html
                                 (pr/piano-roll score
                                                (cond-> {:target-width 500}
                                                  visible (assoc :channels visible))))}})))))


(defui code-editor [{:keys [source options]}]
  (let [input-editor-ref (uix/use-ref)
        [source set-source] (uix/use-state source)
        [return set-return] (uix/use-state nil)
        [score* set-score] (uix/use-state nil)
        [editing set-editing] (uix/use-state false)
        [evaluating set-evaluating] (uix/use-state false)
        [playing set-playing] (uix/use-state false)
        color :light-skyblue
        error? (:error return)]

    (sc :.code-editor

        {:m [0 :1em]
         :position :relative}

        (when (:clj-only options)
          (sc {:position [:absolute {:top 6 :right 6}]
               :z-index 100}
              (c ui.misc/badge
                 {:color :tomato
                  :size :sm
                  :text "clj-only"})))

        (sc :.code-editor_input

            {:border {:width 2
                      :color [color {:a 0.2}]
                      :bottom (when return {:width 0})}
             :p 0
             :flex [:row {:items :center}]}

            (c :.code-editor-input_left-button

               {:style {:flex :center
                        :bg {:color [color {:a 0.1}]}
                        :color (if evaluating [color {:a 0.5}] color)
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
                    :bg {:color [:white {:a 0.5}]}
                    :flex :center}

                   (c spinner {:color "lightskyblue"  :loading evaluating :size 15}))

               (if editing

                 (c CodeMirror
                    {:ref input-editor-ref
                     :value source
                     :on-change (fn [x] (set-source x))
                     :autoFocus true
                     :extensions EDITOR_EXTENSIONS
                     :theme EDITOR_THEME ;noctisLilac ; vscodeLight ;quietlight ;githubLight
                     :basic-setup #js {:lineNumbers false
                                       :foldGutter false
                                       :highlightActiveLine false}})
                 (sc {"pre" {:m 0 :p 0}
                      "code.hljs" {:bg {:color :white}}}
                     (c Highlight
                        {:class "clojure"}
                        (str source))))))
        (when return

          (if score*

            ;; ── Piano roll output ──────────────────────────
            ;; Full-width block layout for horizontal scrolling.
            (sc :.code-editor-output

                {:border {:color [color {:a 0.2}]
                          :width 2}
                 :p 0
                 :position :relative}

                (c :.code-editor-output_close

                   {:style {:position [:absolute {:top 4 :right 4}]
                            :z-index 10
                            :flex :center
                            :bg {:color [:white {:a 0.8}]}
                            :color color
                            :p 0.3
                            :rounded 1
                            :cursor :pointer
                            :hover {:color :grey8}}
                    :on-click (fn [_]
                                (set-return nil)
                                (set-score nil))}
                   (c icons-vsc/VscClose))

                ($ piano-roll-view {:score score*}))

            ;; ── Text output (errors / non-score results) ───
            (sc :code-editor-output

                {:border {:color [(if error? :red color) {:a 0.2}]
                          :width 2}
                 :p 0
                 :flex [:row {:items :center}]}

                (c :.code-editor-output_left-button

                   {:style {:flex :center
                            :bg {:color :white}
                            :border {:right [2 [(if error? :red color) {:a 0.2}]]}
                            :color (if error? :red color)
                            :p [1 0.5]
                            :align-self :stretch}
                    :on-click (fn [_]
                                (set-return nil)
                                (set-score nil))}
                   (c icons-vsc/VscClose))

                (sc :.code-editor-output_content

                    {:bg {:color (if error? [:red {:a 0.05}] [color {:a 0.1}])}
                     :color (if (:error return) [:red {:a 0.45}])
                     :p (if error? [0.5 0.3] [0.3 0.7])
                     :flexi [1 1 :auto]}

                    (if editing

                      (c CodeMirror
                         {:value (str/trim (or (some-> return :error .-message)
                                               (u/pretty-str (:result return))))
                          :editable false
                          :extensions EDITOR_EXTENSIONS
                          :theme EDITOR_THEME
                          :basic-setup #js {:lineNumbers false
                                            :foldGutter false
                                            :highlightActiveLine false}})

                      (sc {"pre" {:m 0 :p 0}
                           "code.hljs" {:bg {:color [:white {:a 0}]}}}
                          (c Highlight
                             {:class "clojure"}
                             (str (str/trim (or (some-> return :error .-message)
                                                (u/pretty-str (:result return)))))))))))))))
