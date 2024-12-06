(ns noon.client.core
  (:require [noon.client.ui :as ui]
            [uix.core :refer [$]]
            [uix.dom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]))

(def ^:export audio-ctx (new js/AudioContext))

(defonce root (uix.dom/create-root (js/document.getElementById "app")))

(defn render []
  (uix.dom/render-root ($ ui/code-editor {:source "(tup s0 s1 s2)"}) root))

(defn ^:dev/after-load reload []
  (.resume audio-ctx)
  (render))

(defn ^:export init []
  (stylefy/init {:dom (gdom/init)})
  (.resume audio-ctx)
  (render))
