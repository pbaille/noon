(ns noon.widget
  "Mounting layer for embedding noon code-editor widgets in external pages.
   Scans the DOM for [data-noon-widget] elements and mounts the
   noon.client.ui.code-editor component on them.

   Called explicitly via noon.widget.init() from a <script> tag."
  (:require [noon.client.ui.code-editor :as ui.code-editor]
            [clojure.edn :as edn]
            [uix.core :as uix :refer [$ defui]]
            [uix.dom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]))

(defonce ^:private initialized? (atom false))

(defn- ensure-stylefy!
  "Initialize stylefy for external pages that don't have the
   required <style> elements in their HTML."
  []
  (when-not @initialized?
    (reset! initialized? true)
    (doseq [id ["_stylefy-constant-styles_" "_stylefy-styles_"]]
      (when-not (.getElementById js/document id)
        (let [style (.createElement js/document "style")]
          (.setAttribute style "id" id)
          (.appendChild js/document.head style))))
    (stylefy/init {:dom (gdom/init)})))

(defn- parse-options
  "Parse widget options from the data-noon-options attribute."
  [el]
  (when-let [opts-str (.getAttribute el "data-noon-options")]
    (try (edn/read-string opts-str)
         (catch :default _ nil))))

(def ^:private light-hljs-css
  "Light highlight.js theme for Clojure syntax in light widgets."
  ".noon-light .hljs { background: #ffffff; color: #1e293b; }
   .noon-light .hljs-keyword { color: #8959a8; font-weight: bold; }
   .noon-light .hljs-built_in { color: #0086b3; }
   .noon-light .hljs-string { color: #718c00; }
   .noon-light .hljs-number { color: #f5871f; }
   .noon-light .hljs-literal { color: #d14; }
   .noon-light .hljs-symbol { color: #990073; }
   .noon-light .hljs-comment { color: #8e908c; font-style: italic; }
   .noon-light .hljs-title { color: #4271ae; }
   .noon-light .hljs-params { color: #1e293b; }
   .noon-light .hljs-attr { color: #008080; }
   .noon-light .hljs-name { color: #4271ae; }")

(def ^:private dark-hljs-css
  "Dark highlight.js theme aligned with CodeMirror materialDark."
  ".noon-dark .hljs { background: #2e3235; color: #bdbdbd; }
   .noon-dark .hljs-keyword { color: #cf6edf; }
   .noon-dark .hljs-built_in { color: #56c8d8; }
   .noon-dark .hljs-string { color: #99d066; }
   .noon-dark .hljs-number { color: #ffad42; }
   .noon-dark .hljs-literal { color: #56c8d8; }
   .noon-dark .hljs-symbol { color: #facf4e; }
   .noon-dark .hljs-comment { color: #707d8b; font-style: italic; }
   .noon-dark .hljs-title { color: #56c8d8; }
   .noon-dark .hljs-params { color: #bdbdbd; }
   .noon-dark .hljs-attr { color: #bdbdbd; }
   .noon-dark .hljs-name { color: #56c8d8; }")

(defn- inject-hljs-theme!
  "Inject the highlight.js theme stylesheet for the given theme."
  [theme-id css]
  (let [style-id (str "_noon-hljs-" theme-id "_")]
    (when-not (.getElementById js/document style-id)
      (let [style (.createElement js/document "style")]
        (.setAttribute style "id" style-id)
        (set! (.-textContent style) css)
        (.appendChild js/document.head style)))))

;; ── System color scheme detection ────────────────────────────────

(defn- detect-system-theme
  "Detect the current system color scheme via prefers-color-scheme media query.
   Returns :dark or :light."
  []
  (if (and js/window
           (.matchMedia js/window "(prefers-color-scheme: dark)")
           (.-matches (.matchMedia js/window "(prefers-color-scheme: dark)")))
    :dark
    :light))

(defn- use-system-theme
  "React hook that tracks the system color scheme.
   Returns :dark or :light, updating reactively when the user toggles."
  []
  (let [[theme set-theme] (uix/use-state (detect-system-theme))]
    (uix/use-effect
     (fn []
       (let [mq (.matchMedia js/window "(prefers-color-scheme: dark)")
             handler (fn [e] (set-theme (if (.-matches e) :dark :light)))]
         (.addEventListener mq "change" handler)
         (fn [] (.removeEventListener mq "change" handler))))
     [])
    theme))

;; ── Auto-themed widget wrapper ───────────────────────────────────

(defui auto-widget
  "Widget wrapper that resolves :auto theme to :light or :dark
   based on prefers-color-scheme, then renders the code-editor."
  [{:keys [source options container-el]}]
  (let [requested-theme (keyword (or (:theme options) :auto))
        system-theme (use-system-theme)
        resolved-theme (if (= :auto requested-theme) system-theme requested-theme)
        dark? (= :dark resolved-theme)]

    ;; Sync the noon-light/noon-dark class on the container element
    (uix/use-effect
     (fn []
       (let [cl (.-classList container-el)]
         (if dark?
           (do (.remove cl "noon-light") (.add cl "noon-dark"))
           (do (.remove cl "noon-dark") (.add cl "noon-light")))))
     [dark? container-el])

    ($ ui.code-editor/code-editor
       {:source source
        :options (merge {:show-piano-roll? true} options)
        :theme-key resolved-theme})))

;; ── Widget mounting ──────────────────────────────────────────────

(defn- mount-widgets! []
  ;; Inject both hljs themes upfront — auto widgets may switch at any time
  (inject-hljs-theme! "light" light-hljs-css)
  (inject-hljs-theme! "dark" dark-hljs-css)
  (doseq [el (array-seq (.querySelectorAll js/document "[data-noon-widget]"))]
    (when-not (.getAttribute el "data-noon-mounted")
      (.setAttribute el "data-noon-mounted" "true")
      (let [source-el (.querySelector el ".noon-source")
            source (when source-el (.-textContent source-el))
            options (parse-options el)]
        (when source
          (when source-el (.removeChild el source-el))
          (let [root (uix.dom/create-root el)]
            (uix.dom/render-root
             ($ auto-widget
                {:source source
                 :options options
                 :container-el el})
             root)))))))

(defn ^:export init []
  (ensure-stylefy!)
  (if (= "loading" (.-readyState js/document))
    (.addEventListener js/document "DOMContentLoaded" (fn [_] (mount-widgets!)))
    (mount-widgets!)))
