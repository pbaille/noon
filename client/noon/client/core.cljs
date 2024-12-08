(ns noon.client.core
  (:require [noon.client.ui :as ui]
            [uix.core :refer [$]]
            [uix.dom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]
            [noon.client.examples :as ex]))

(def ^:export audio-ctx (new js/AudioContext))

(defonce root (uix.dom/create-root (js/document.getElementById "app")))

(defn render []
  (uix.dom/render-root
   ($ :div
      (map (fn [[k code]]
             ($ :div
                {:key k}
                ($ :h1 (name k))
                ($ ui/code-editor
                   {:source code :resume-audio-ctx (fn [_] (.resume audio-ctx))})))
           ex/examples))
   root))

(defn ^:dev/after-load reload []
  (.resume audio-ctx)
  (render))

(defn ^:export init []
  (stylefy/init {:dom (gdom/init)})
  (.resume audio-ctx)
  (render))
