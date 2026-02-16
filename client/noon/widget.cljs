(ns noon.widget
  "Self-contained interactive noon editor widget for embedding in external pages.
   Mounts the UIx code-editor component on DOM elements.

   Auto-mounts on elements with [data-noon-widget] attribute.
   Source code is read from a child <pre class='noon-source'> element.

   Group widgets use [data-noon-widget-group] with data-entries JSON attribute."
  (:require [noon.eval :as eval]
            [noon.output :as output]
            [noon.output.midi :as midi]
            [noon.score :as score]
            [noon.viz.piano-roll :as pr]
            [noon.client.ui.misc :as ui.misc]
            [uix.core :as uix :refer [defui $]]
            [uix.dom]
            [uic.component :refer [c sc]]
            ["@uiw/react-codemirror" :default CodeMirror]
            ["@nextjournal/lang-clojure" :refer [clojure]]
            ["@uiw/codemirror-themes-all" :as cm-themes]
            ["react-icons/vsc" :as icons-vsc]
            ["react-icons/tb" :as icons-tb :refer [TbPiano]]
            ["react-spinners/BeatLoader" :default spinner]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]))

;; ── Constants ───────────────────────────────────────────────────

(def ^:private editor-extensions #js [(clojure)])
(def ^:private editor-theme cm-themes/quietlight)
(def ^:private accent :light-skyblue)

;; ── Helpers ─────────────────────────────────────────────────────

(defn- result->score
  "Extract a noon score from an eval result."
  [result]
  (cond
    (score/score? result) result
    (some-> (meta result) :score) (:score (meta result))
    :else nil))

(defn- play-score!
  "Play a score via noon.output, returns the play id."
  [score]
  (:id (output/noon {:play true} score)))

;; ── Piano roll view ─────────────────────────────────────────────

(defui piano-roll-view [{:keys [score]}]
  (sc {:overflow-x :auto
       :overflow-y :hidden
       :p [0.5 0]}
      (c :div {:style {:width "max-content"
                       :min-width "100%"}
               :dangerouslySetInnerHTML
               #js {:__html (ui.misc/hiccup->html
                             (pr/piano-roll score {:target-width 500}))}})))

;; ── Code editor widget ──────────────────────────────────────────

(defui code-editor [{:keys [source label]}]
  (let [[source* set-source] (uix/use-state source)
        [return set-return] (uix/use-state nil)
        [score* set-score] (uix/use-state nil)
        [editing set-editing] (uix/use-state false)
        [evaluating set-evaluating] (uix/use-state false)
        [playing set-playing] (uix/use-state false)
        error? (:error return)

        do-eval
        (uix/use-callback
         (fn []
           (when-not evaluating
             ;; Resume AudioContext within user gesture
             (midi/ensure-audio-context!)
             (set-evaluating true)
             (set-return nil)
             (set-score nil)
             (js/setTimeout
              (fn []
                (eval/eval-string-async
                 source*
                 (fn [{:keys [result error] :as x}]
                   (set-evaluating false)
                   (if error
                     (set-return x)
                     (do
                       ;; Play audio: either from (play ...) result or auto-play score
                       (let [id (or (:id result)
                                    (when-let [s (result->score result)]
                                      (play-score! s)))]
                         (when id
                           (set-playing true)
                           (midi/on-done-playing id #(set-playing false))))
                       ;; Extract score for piano roll
                       (set-score (result->score result))
                       (set-return x))))
                 (fn [x]
                   (set-evaluating false)
                   (set-return x))))
              20)))
         [source* evaluating])

        do-eval-silent
        (uix/use-callback
         (fn []
           (when-not evaluating
             (set-evaluating true)
             (js/setTimeout
              (fn []
                (eval/eval-string-async
                 source*
                 (fn [{:keys [result] :as x}]
                   (set-evaluating false)
                   (set-score (result->score result))
                   (set-return x))
                 (fn [{:as x}]
                   (set-evaluating false)
                   (set-return x))))
              20)))
         [source* evaluating])]

    ;; Auto-eval on mount (silent — no audio)
    (let [mounted (uix/use-ref false)]
      (uix/use-effect
       (fn []
         (when-not @mounted
           (reset! mounted true)
           (do-eval-silent))
         js/undefined)
       [do-eval-silent]))

    (sc :.noon-widget-root
        {:m [0 :1em]
         :position :relative}

        ;; Label
        (when label
          (sc {:p [0.4 0.75]
               :text [:xs :semibold :mono]
               :color :grey5
               :text-transform :uppercase
               :letter-spacing "0.5px"
               :bg {:color "#f8fafc"}
               :border {:bottom [1 :grey2]}}
              label))

        ;; Editor row
        (sc {:border {:width 2
                      :color [accent {:a 0.2}]
                      :bottom (when return {:width 0})}
             :p 0
             :flex [:row {:items :center}]}

            ;; Play/stop/collapse button
            (c :button
               {:style {:flex :center
                        :bg {:color [accent {:a 0.1}]}
                        :color (if evaluating [accent {:a 0.5}] accent)
                        :p 1 :align-self :stretch
                        :border {:width 0}
                        :cursor (if evaluating :wait :pointer)}
                :on-click (if return
                            (fn [_]
                              (set-return nil)
                              (set-score nil)
                              (set-playing false)
                              (eval/stop))
                            (fn [_]
                              (if playing
                                (do (eval/stop)
                                    (set-playing false))
                                (do-eval))))}
               (cond
                 playing (c icons-tb/TbPlayerStopFilled)
                 return (c icons-vsc/VscChevronUp)
                 :else (c icons-vsc/VscDebugStart)))

            ;; Editor content
            (c :div
               {:style {:overflow :scroll
                        :position :relative
                        :width "100%"
                        :flex "1 1 auto"}
                :on-click (fn [_] (set-editing true))
                :on-blur (fn [_] (set-editing false))}

               ;; Loading overlay
               (sc {:z-index (if evaluating 10000 -1)
                    :position [:absolute [0 0 0 0]]
                    :size :full
                    :bg {:color [:white {:a 0.5}]}
                    :flex :center}
                   (c spinner {:color "lightskyblue" :loading evaluating :size 15}))

               (if editing
                 (c CodeMirror
                    {:value source*
                     :on-change (fn [x] (set-source x))
                     :autoFocus true
                     :extensions editor-extensions
                     :theme editor-theme
                     :basic-setup #js {:lineNumbers false
                                       :foldGutter false
                                       :highlightActiveLine false}})
                 (sc {"pre" {:m 0 :p 0}
                      "code" {:bg {:color :white}
                              :p [0.3 0.7]
                              :display :block
                              :font-size "13px"
                              :font-family "'SF Mono', 'Fira Code', ui-monospace, monospace"
                              :white-space :pre-wrap
                              :word-break :break-word}}
                     (c :pre (c :code source*))))))

        ;; Output area
        (when return
          (if score*
            ;; Piano roll
            (sc {:border {:color [accent {:a 0.2}]
                          :width 2}
                 :p 0
                 :position :relative}

                ;; Close button
                (c :button
                   {:style {:position [:absolute {:top 4 :right 4}]
                            :z-index 10
                            :flex :center
                            :bg {:color :transparent}
                            :color accent
                            :border {:width 0}
                            :p 0.3
                            :cursor :pointer
                            :hover {:color :tomato}}
                    :on-click (fn [_]
                                (set-return nil)
                                (set-score nil))}
                   (c icons-vsc/VscClose))

                ($ piano-roll-view {:score score*}))

            ;; Error / text output
            (sc {:border {:color [(if error? :red accent) {:a 0.2}]
                          :width 2}
                 :p 0
                 :flex [:row {:items :center}]}

                (c :button
                   {:style {:flex :center
                            :bg {:color :white}
                            :border {:width 0
                                     :right [2 [(if error? :red accent) {:a 0.2}]]}
                            :color (if error? :red accent)
                            :p [1 0.5]
                            :align-self :stretch
                            :cursor :pointer}
                    :on-click (fn [_]
                                (set-return nil)
                                (set-score nil))}
                   (c icons-vsc/VscClose))

                (sc {:bg {:color (if error? [:red {:a 0.05}] [accent {:a 0.1}])}
                     :color (when error? [:red {:a 0.45}])
                     :p (if error? [0.5 0.3] [0.3 0.7])
                     :flexi [1 1 :auto]}
                    (when score*
                      (c :button
                         {:style {:position [:absolute {:top "50%" :right 8}]
                                  :transform "translateY(-50%)"
                                  :bg {:color :white}
                                  :border {:width 0}
                                  :outline :none
                                  :color "#b0b8c4"
                                  :p 0
                                  :cursor :pointer
                                  :flex :center
                                  :hover {:color :tomato}}
                          :on-click (fn [_] (set-return nil) (set-score nil))}
                         (c TbPiano {:size 18})))
                    (sc {"pre" {:m 0 :p 0}
                         "code" {:font-size "12px"
                                 :font-family "'SF Mono', 'Fira Code', ui-monospace, monospace"}}
                        (c :pre (c :code (or (some-> return :error .-message)
                                             (str (:result return)))))))))))))

;; ── Widget group ────────────────────────────────────────────────

(defui widget-group [{:keys [entries]}]
  (sc {:m [0 :1em]}
      (mapv (fn [{:keys [label source]}]
              ($ code-editor {:key (str label source)
                              :source source
                              :label label}))
            entries)))

;; ── Auto-mount ──────────────────────────────────────────────────

(defn- parse-group-entries [el]
  (when-let [json-str (.getAttribute el "data-entries")]
    (try
      (let [entries (js/JSON.parse json-str)]
        (mapv (fn [entry]
                {:label (aget entry "label")
                 :source (aget entry "source")})
              entries))
      (catch :default e
        (js/console.error "noon-widget: failed to parse group entries" e)
        nil))))

(defn mount-all! []
  ;; Single widgets
  (doseq [el (array-seq (.querySelectorAll js/document "[data-noon-widget]"))]
    (when-not (.getAttribute el "data-noon-mounted")
      (.setAttribute el "data-noon-mounted" "true")
      (let [source-el (.querySelector el ".noon-source")
            source (when source-el (.-textContent source-el))
            label (.getAttribute el "data-label")]
        (when source
          (when source-el (.removeChild el source-el))
          (let [root (uix.dom/create-root el)]
            (uix.dom/render-root ($ code-editor {:source source :label label}) root))))))

  ;; Group widgets
  (doseq [el (array-seq (.querySelectorAll js/document "[data-noon-widget-group]"))]
    (when-not (.getAttribute el "data-noon-mounted")
      (.setAttribute el "data-noon-mounted" "true")
      (when-let [entries (parse-group-entries el)]
        (let [root (uix.dom/create-root el)]
          (uix.dom/render-root ($ widget-group {:entries entries}) root))))))

;; ── Entry point ─────────────────────────────────────────────────

(defonce ^:private initialized? (atom false))

(defn- ensure-stylefy-nodes!
  "Ensure stylefy's required <style> elements exist in the DOM.
   The noon client app has them in index.html, but external pages won't."
  []
  (when-not (.getElementById js/document "_stylefy-constant-styles_")
    (let [style (.createElement js/document "style")]
      (.setAttribute style "id" "_stylefy-constant-styles_")
      (.appendChild js/document.head style)))
  (when-not (.getElementById js/document "_stylefy-styles_")
    (let [style (.createElement js/document "style")]
      (.setAttribute style "id" "_stylefy-styles_")
      (.appendChild js/document.head style))))

(defn ^:export init []
  ;; Initialize stylefy once
  (when-not @initialized?
    (reset! initialized? true)
    (ensure-stylefy-nodes!)
    (stylefy/init {:dom (gdom/init)}))

  (if (= "loading" (.-readyState js/document))
    (.addEventListener js/document "DOMContentLoaded" (fn [_] (mount-all!)))
    (mount-all!)))

;; ── Public API ──────────────────────────────────────────────────

(defn ^:export mount
  [element source options]
  (when-not @initialized?
    (reset! initialized? true)
    (ensure-stylefy-nodes!)
    (stylefy/init {:dom (gdom/init)}))
  (let [label (when options (aget options "label"))
        root (uix.dom/create-root element)]
    (uix.dom/render-root ($ code-editor {:source source :label label}) root)))

(defn ^:export mountGroup
  [element entries]
  (when-not @initialized?
    (reset! initialized? true)
    (ensure-stylefy-nodes!)
    (stylefy/init {:dom (gdom/init)}))
  (let [parsed (mapv (fn [e] {:label (aget e "label") :source (aget e "source")}) entries)
        root (uix.dom/create-root element)]
    (uix.dom/render-root ($ widget-group {:entries parsed}) root)))
