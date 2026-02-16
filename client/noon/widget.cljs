(ns noon.widget
  "Self-contained interactive noon editor widget.
   Mounts CodeMirror + SCI eval + piano roll + audio on DOM elements.
   No React/UIx dependency — uses direct DOM manipulation.

   Auto-mounts on elements with [data-noon-widget] attribute.
   Source code is read from a child <pre class='noon-source'> element."
  (:require [noon.eval :as eval]
            [noon.output :as output]
            [noon.output.midi :as midi]
            [noon.score :as score]
            [noon.viz.piano-roll :as pr]
            [clojure.string :as str]
            ["codemirror" :as cm]
            ["@codemirror/view" :as cm-view :refer [keymap]]
            ["@codemirror/state" :as cm-state]
            ["@nextjournal/lang-clojure" :refer [clojure]]))

;; ── Hiccup → HTML ───────────────────────────────────────────────

(defn- style-map->str [m]
  (when (map? m)
    (->> m
         (map (fn [[k v]] (str (name k) ":" v)))
         (str/join ";"))))

(defn- hiccup->html
  "Recursively render Clojure hiccup to an HTML string."
  [form]
  (cond
    (nil? form)    ""
    (string? form) form
    (number? form) (str form)
    (vector? form)
    (let [[tag & rest] form
          [attrs children] (if (map? (first rest))
                             [(first rest) (next rest)]
                             [nil rest])
          tag-name (name tag)
          attr-str (when attrs
                     (str (when-let [s (:style attrs)]
                            (str " style=\"" (style-map->str s) "\""))
                          (->> (dissoc attrs :style)
                               (map (fn [[k v]] (str " " (name k) "=\"" v "\"")))
                               (apply str))))]
      (str "<" tag-name attr-str ">"
           (apply str (map hiccup->html children))
           "</" tag-name ">"))
    (seq? form) (apply str (map hiccup->html form))
    :else (str form)))

;; ── DOM helpers ─────────────────────────────────────────────────

(defn- create-el
  "Create a DOM element with optional attributes and style."
  [tag & {:keys [class style text html children parent]}]
  (let [el (.createElement js/document tag)]
    (when class (.setAttribute el "class" class))
    (when style
      (doseq [[k v] style]
        (aset (.-style el) (name k) v)))
    (when text (set! (.-textContent el) text))
    (when html (set! (.-innerHTML el) html))
    (when children
      (doseq [child children]
        (when child (.appendChild el child))))
    (when parent (.appendChild parent el))
    el))

(defn- add-style!
  "Add inline styles to an element."
  [el style-map]
  (doseq [[k v] style-map]
    (aset (.-style el) (name k) v)))

;; ── CSS injection (once) ────────────────────────────────────────

(defonce ^:private css-injected? (atom false))

(def ^:private widget-css
  ".noon-widget-root {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    border: 1px solid #e2e8f0;
    border-radius: 8px;
    overflow: hidden;
    background: #fff;
    margin: 1em 0;
  }
  .noon-widget-root .cm-editor {
    max-height: 200px;
    font-size: 13px;
  }
  .noon-widget-root .cm-editor.cm-focused {
    outline: none;
  }
  .noon-widget-root .cm-scroller {
    overflow: auto;
  }
  .noon-widget-toolbar {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 6px 10px;
    background: #f8fafc;
    border-top: 1px solid #e2e8f0;
  }
  .noon-widget-btn {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    padding: 4px 12px;
    border: none;
    border-radius: 4px;
    font-size: 12px;
    font-weight: 500;
    cursor: pointer;
    transition: all 0.15s ease;
    font-family: inherit;
  }
  .noon-widget-btn-eval {
    background: lightskyblue;
    color: white;
  }
  .noon-widget-btn-eval:hover {
    background: #5bb8f5;
  }
  .noon-widget-btn-eval:disabled {
    opacity: 0.6;
    cursor: wait;
  }
  .noon-widget-btn-stop {
    background: #f1f5f9;
    color: #64748b;
    border: 1px solid #e2e8f0;
  }
  .noon-widget-btn-stop:hover {
    border-color: #94a3b8;
    color: #334155;
  }
  .noon-widget-error {
    padding: 6px 10px;
    background: #fef2f2;
    border-top: 1px solid #fecaca;
    color: #dc2626;
    font-size: 12px;
    font-family: 'SF Mono', 'Fira Code', ui-monospace, monospace;
    white-space: pre-wrap;
    word-break: break-word;
  }
  .noon-widget-viz {
    padding: 8px;
    border-top: 1px solid #e2e8f0;
    overflow-x: auto;
    display: flex;
    justify-content: center;
  }
  .noon-widget-viz svg {
    max-width: 100%;
    height: auto;
  }
  .noon-widget-spinner {
    padding: 20px;
    text-align: center;
    color: #94a3b8;
    font-size: 13px;
  }
  .noon-widget-label {
    padding: 6px 12px;
    font-size: 11px;
    font-weight: 600;
    color: #64748b;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    background: #f8fafc;
    border-bottom: 1px solid #e2e8f0;
    font-family: 'SF Mono', 'Fira Code', ui-monospace, monospace;
  }
  .noon-widget-group {
    margin: 1em 0;
  }
  .noon-widget-group > .noon-widget-root {
    margin: 0;
    border-radius: 0;
    border-bottom-width: 0;
  }
  .noon-widget-group > .noon-widget-root:first-child {
    border-radius: 8px 8px 0 0;
  }
  .noon-widget-group > .noon-widget-root:last-child {
    border-radius: 0 0 8px 8px;
    border-bottom-width: 1px;
  }
  .noon-widget-group > .noon-widget-root:only-child {
    border-radius: 8px;
  }
  ")

(defn- inject-css! []
  (when-not @css-injected?
    (let [style (.createElement js/document "style")]
      (set! (.-textContent style) widget-css)
      (.appendChild js/document.head style)
      (reset! css-injected? true))))

;; ── SVG icons (inline) ──────────────────────────────────────────

(def ^:private icon-play
  "<svg width=\"14\" height=\"14\" viewBox=\"0 0 16 16\" fill=\"currentColor\"><path d=\"M4 2l10 6-10 6V2z\"/></svg>")

(def ^:private icon-stop
  "<svg width=\"14\" height=\"14\" viewBox=\"0 0 16 16\" fill=\"currentColor\"><rect x=\"3\" y=\"3\" width=\"10\" height=\"10\" rx=\"1\"/></svg>")

(def ^:private icon-loading
  "<svg width=\"14\" height=\"14\" viewBox=\"0 0 16 16\" fill=\"currentColor\" class=\"noon-spin\"><circle cx=\"8\" cy=\"2\" r=\"1.5\" opacity=\"0.3\"/><circle cx=\"12.24\" cy=\"4\" r=\"1.5\" opacity=\"0.4\"/><circle cx=\"14\" cy=\"8\" r=\"1.5\" opacity=\"0.5\"/><circle cx=\"12.24\" cy=\"12\" r=\"1.5\" opacity=\"0.6\"/><circle cx=\"8\" cy=\"14\" r=\"1.5\" opacity=\"0.7\"/><circle cx=\"3.76\" cy=\"12\" r=\"1.5\" opacity=\"0.8\"/><circle cx=\"2\" cy=\"8\" r=\"1.5\" opacity=\"0.9\"/><circle cx=\"3.76\" cy=\"4\" r=\"1.5\" opacity=\"1\"/></svg>")

;; ── CodeMirror setup ────────────────────────────────────────────

;; (unused, editor extensions are set up inline in create-editor)

(defn- create-editor
  "Create a CodeMirror 6 editor in the given parent element."
  [parent source {:keys [on-eval]}]
  (let [eval-keymap (.of keymap
                         #js [#js {:key "Mod-Enter"
                                   :run (fn [_view]
                                          (when on-eval (on-eval))
                                          true)}])
        state (.create cm-state/EditorState
                       #js {:doc source
                            :extensions #js [cm/minimalSetup
                                             (clojure)
                                             (.-lineWrapping cm-view/EditorView)
                                             eval-keymap]})
        view (cm-view/EditorView. #js {:state state
                                       :parent parent})]
    view))

;; ── Widget instance ─────────────────────────────────────────────

(defn- result->score
  "Extract a noon score from an eval result."
  [result]
  (cond
    (score/score? result) result
    (some-> (meta result) :score) (:score (meta result))
    :else nil))

(defn- play-score!
  "Play a score via noon.output and return the play id.
   Used when eval result is a score (not a play result)."
  [score]
  (let [result (output/noon {:play true} score)]
    (:id result)))

(defn- mount-widget
  "Mount an interactive noon editor widget into the given DOM element.

   Options:
     :source     - noon source code string
     :label      - optional label shown above the editor
     :auto-eval  - auto-evaluate on mount (default: true)
     :editable   - show editor (default: true)
     :target-width - piano roll width (default: 600)"
  [container {:keys [source label auto-eval editable target-width]
              :or   {auto-eval true editable true target-width 600}}]
  (let [;; State
        state (atom {:evaluating false
                     :playing false
                     :score nil
                     :error nil})

        ;; DOM structure
        root (create-el "div" :class "noon-widget-root")

        ;; Label (optional)
        _ (when label
            (create-el "div" :class "noon-widget-label"
                       :text label :parent root))

        ;; Editor container
        editor-wrap (when editable
                      (create-el "div" :parent root))

        ;; Toolbar
        toolbar (create-el "div" :class "noon-widget-toolbar" :parent root)

        eval-btn (create-el "button" :class "noon-widget-btn noon-widget-btn-eval"
                            :html (str icon-play " <span>Eval & Play</span>")
                            :parent toolbar)

        stop-btn (create-el "button" :class "noon-widget-btn noon-widget-btn-stop"
                            :html (str icon-stop " <span>Stop</span>")
                            :parent toolbar)

        status-el (create-el "span"
                             :style {:font-size "11px" :color "#94a3b8"
                                     :margin-left "auto" :font-style "italic"}
                             :parent toolbar)

        ;; Error display
        error-el (create-el "div" :class "noon-widget-error" :parent root)

        ;; Visualization
        viz-el (create-el "div" :class "noon-widget-viz" :parent root)

        ;; CodeMirror editor
        editor-view (when editable
                      (create-editor editor-wrap source
                                     {:on-eval #(do)})) ;; will be set below

        ;; ── Actions ─────────────────────────────────────
        get-source (fn []
                     (if editor-view
                       (.toString (.. editor-view -state -doc))
                       source))

        render-piano-roll
        (fn [score]
          (set! (.-innerHTML viz-el)
                (hiccup->html
                 (pr/piano-roll score {:target-width target-width})))
          (add-style! viz-el {:display "flex"}))

        show-error
        (fn [msg]
          (set! (.-textContent error-el) msg)
          (add-style! error-el {:display "block"}))

        hide-error
        (fn []
          (add-style! error-el {:display "none"}))

        do-eval
        (fn [& {:keys [play?] :or {play? true}}]
          (when-not (:evaluating @state)
            ;; Resume AudioContext within user gesture (before async work)
            (when play? (midi/ensure-audio-context!))

            (swap! state assoc :evaluating true :error nil)
            (set! (.-disabled eval-btn) true)
            (set! (.-innerHTML eval-btn) (str icon-loading " <span>Evaluating...</span>"))
            (hide-error)
            (set! (.-textContent status-el) "")

            (js/setTimeout
             (fn []
               (eval/eval-string-async
                (get-source)
                (fn [{:keys [result error]}]
                  (swap! state assoc :evaluating false)
                  (set! (.-disabled eval-btn) false)
                  (set! (.-innerHTML eval-btn) (str icon-play " <span>Eval & Play</span>"))
                  (if error
                    (do
                      (show-error (.-message error))
                      (swap! state assoc :error (.-message error)))
                    (let [;; Get score from result (works for both score and play results)
                          s (result->score result)
                          ;; Get play id — present for (play ...) results, nil for (score ...)
                          play-id (when play?
                                    (or (:id result)
                                        ;; Auto-play if result is a score without play id
                                        (when s (play-score! s))))]
                      ;; Handle playback tracking
                      (when play-id
                        (swap! state assoc :playing true)
                        (add-style! stop-btn {:display "inline-flex"})
                        (set! (.-textContent status-el) "Playing...")
                        (midi/on-done-playing
                         play-id
                         (fn []
                           (swap! state assoc :playing false)
                           (add-style! stop-btn {:display "none"})
                           (set! (.-textContent status-el) ""))))
                      ;; Render piano roll
                      (when s
                        (swap! state assoc :score s)
                        (render-piano-roll s)))))
                (fn [{:keys [error]}]
                  (swap! state assoc :evaluating false)
                  (set! (.-disabled eval-btn) false)
                  (set! (.-innerHTML eval-btn) (str icon-play " <span>Eval & Play</span>"))
                  (show-error (.-message error)))))
             20)))

        do-stop
        (fn []
          (eval/stop)
          (swap! state assoc :playing false)
          (add-style! stop-btn {:display "none"})
          (set! (.-textContent status-el) ""))]

    ;; ── Initialize ──────────────────────────────────────
    ;; Hide elements initially
    (add-style! stop-btn {:display "none"})
    (add-style! error-el {:display "none"})
    (add-style! viz-el {:display "none"})

    ;; Wire up eval keymap (Cmd/Ctrl+Enter)
    (when editor-view
      ;; Add keyboard listener on the editor wrapper
      (.addEventListener editor-wrap "keydown"
                         (fn [e]
                           (when (and (or (.-metaKey e) (.-ctrlKey e))
                                      (= (.-key e) "Enter"))
                             (.preventDefault e)
                             (do-eval :play? true)))))

    ;; Button handlers
    (.addEventListener eval-btn "click" (fn [_] (do-eval :play? true)))
    (.addEventListener stop-btn "click" (fn [_] (do-stop)))

    ;; Mount
    (.appendChild container root)

    ;; Auto-eval (render piano roll only, don't play audio)
    (when auto-eval
      (js/setTimeout #(do-eval :play? false) 100))

    ;; Return handle
    {:root root
     :editor editor-view
     :eval! do-eval
     :stop! do-stop
     :state state}))

;; ── Group widget ────────────────────────────────────────────────

(defn- mount-group
  "Mount a group of noon widgets (stacked, with labels).
   entries is a seq of {:label \"...\" :source \"...\"}."
  [container entries]
  (let [group-el (create-el "div" :class "noon-widget-group")]
    (doseq [{:keys [label source]} entries]
      (let [item-el (create-el "div")]
        (.appendChild group-el item-el)
        (mount-widget item-el {:source source
                               :label label
                               :auto-eval true
                               :editable true})))
    (.appendChild container group-el)))

;; ── Auto-mount ──────────────────────────────────────────────────

(defn- parse-group-entries
  "Parse group entries from a JSON string or child elements."
  [el]
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

(defn mount-all!
  "Find and mount all noon widgets on the page."
  []
  (inject-css!)

  ;; Single widgets: <div data-noon-widget>
  ;;   source is read from child <pre class="noon-source">
  (doseq [el (array-seq (.querySelectorAll js/document "[data-noon-widget]"))]
    (when-not (.getAttribute el "data-noon-mounted")
      (.setAttribute el "data-noon-mounted" "true")
      (let [source-el (.querySelector el ".noon-source")
            source (when source-el (.-textContent source-el))
            label (.getAttribute el "data-label")]
        (when source
          ;; Remove the source pre (it was just for transport)
          (when source-el
            (.removeChild el source-el))
          (mount-widget el {:source source
                            :label label
                            :auto-eval true
                            :editable true})))))

  ;; Group widgets: <div data-noon-widget-group data-entries='[...]'>
  (doseq [el (array-seq (.querySelectorAll js/document "[data-noon-widget-group]"))]
    (when-not (.getAttribute el "data-noon-mounted")
      (.setAttribute el "data-noon-mounted" "true")
      (when-let [entries (parse-group-entries el)]
        (mount-group el entries)))))

;; ── Entry point ─────────────────────────────────────────────────

(defn ^:export init []
  (if (= "loading" (.-readyState js/document))
    (.addEventListener js/document "DOMContentLoaded" (fn [_] (mount-all!)))
    (mount-all!)))

;; ── Public API (for manual mounting) ────────────────────────────

(defn ^:export mount
  "Mount a single noon widget.
   element - DOM element to mount into
   source  - noon source code string
   options - JS object with optional keys: label, autoEval, editable"
  [element source options]
  (let [opts (when options
               (cond-> {}
                 (aget options "label")     (assoc :label (aget options "label"))
                 (some? (aget options "autoEval")) (assoc :auto-eval (aget options "autoEval"))
                 (some? (aget options "editable")) (assoc :editable (aget options "editable"))))]
    (inject-css!)
    (mount-widget element (merge {:source source :auto-eval true :editable true} opts))))

(defn ^:export mountGroup
  "Mount a group of noon widgets.
   element - DOM element to mount into
   entries - JS array of {label, source} objects"
  [element entries]
  (inject-css!)
  (mount-group element
               (mapv (fn [e] {:label (aget e "label") :source (aget e "source")})
                     entries)))
