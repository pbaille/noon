(ns noon.widget
  "Mounting layer for embedding noon code-editor widgets in external pages.
   Scans the DOM for [data-noon-widget] elements and mounts the
   noon.client.ui.code-editor component on them.

   Called explicitly via noon.widget.init() from a <script> tag."
  (:require [noon.client.ui.code-editor :as ui.code-editor]
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

(defn- mount-widgets! []
  (doseq [el (array-seq (.querySelectorAll js/document "[data-noon-widget]"))]
    (when-not (.getAttribute el "data-noon-mounted")
      (.setAttribute el "data-noon-mounted" "true")
      (let [source-el (.querySelector el ".noon-source")
            source (when source-el (.-textContent source-el))]
        (when source
          (when source-el (.removeChild el source-el))
          (let [root (uix.dom/create-root el)]
            (uix.dom/render-root
             ($ ui.code-editor/code-editor {:source source})
             root)))))))

(defn ^:export init []
  (ensure-stylefy!)
  (if (= "loading" (.-readyState js/document))
    (.addEventListener js/document "DOMContentLoaded" (fn [_] (mount-widgets!)))
    (mount-widgets!)))
