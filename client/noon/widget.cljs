(ns noon.widget
  "Mounting layer for embedding noon code-editor widgets in external pages.
   Scans the DOM for [data-noon-widget] elements and mounts the
   noon.client.ui.code-editor component on them.

   Called explicitly via noon.widget.init() from a <script> tag."
  (:require [noon.client.ui.code-editor :as ui.code-editor]
            [clojure.edn :as edn]
            [uix.core :refer [$]]
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
  "Minimal dark highlight.js theme for Clojure syntax in dark widgets."
  ".noon-dark .hljs { background: #1e1e2e; color: #cdd6f4; }
   .noon-dark .hljs-keyword { color: #cba6f7; }
   .noon-dark .hljs-built_in { color: #89b4fa; }
   .noon-dark .hljs-string { color: #a6e3a1; }
   .noon-dark .hljs-number { color: #fab387; }
   .noon-dark .hljs-literal { color: #f38ba8; }
   .noon-dark .hljs-symbol { color: #f9e2af; }
   .noon-dark .hljs-comment { color: #6c7086; font-style: italic; }
   .noon-dark .hljs-title { color: #89b4fa; }
   .noon-dark .hljs-params { color: #cdd6f4; }
   .noon-dark .hljs-attr { color: #89dceb; }
   .noon-dark .hljs-name { color: #89b4fa; }")

(defn- inject-hljs-theme!
  "Inject the highlight.js theme stylesheet for the given theme."
  [theme-id css]
  (let [style-id (str "_noon-hljs-" theme-id "_")]
    (when-not (.getElementById js/document style-id)
      (let [style (.createElement js/document "style")]
        (.setAttribute style "id" style-id)
        (set! (.-textContent style) css)
        (.appendChild js/document.head style)))))

(defn- mount-widgets! []
  (doseq [el (array-seq (.querySelectorAll js/document "[data-noon-widget]"))]
    (when-not (.getAttribute el "data-noon-mounted")
      (.setAttribute el "data-noon-mounted" "true")
      (let [source-el (.querySelector el ".noon-source")
            source (when source-el (.-textContent source-el))
            options (parse-options el)
            theme (keyword (or (:theme options) :light))
            dark? (= :dark theme)]
        (when source
          (when source-el (.removeChild el source-el))
          (if dark?
            (do (inject-hljs-theme! "dark" dark-hljs-css)
                (.add (.-classList el) "noon-dark"))
            (do (inject-hljs-theme! "light" light-hljs-css)
                (.add (.-classList el) "noon-light")))
          (let [root (uix.dom/create-root el)]
            (uix.dom/render-root
             ($ ui.code-editor/code-editor
                {:source source
                 :options (merge {:show-piano-roll? true} options)})
             root)))))))

(defn ^:export init []
  (ensure-stylefy!)
  (if (= "loading" (.-readyState js/document))
    (.addEventListener js/document "DOMContentLoaded" (fn [_] (mount-widgets!)))
    (mount-widgets!)))
