(ns noon.client.core
  (:require [noon.client.ui :as ui]
            [uix.core :refer [$]]
            [uix.dom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]))

(def ^:export audio-ctx (new js/AudioContext))

(defonce root (uix.dom/create-root (js/document.getElementById "app")))

(def default-source
  '(let [x s0] (tup x s1)))

(defn render []
  (uix.dom/render-root
   ($ ui/code-editor {:source (str default-source)
                      :resume-audio-ctx (fn [_] (.resume audio-ctx))})
   root))

(defn ^:dev/after-load reload []
  (.resume audio-ctx)
  (render))

(defn ^:export init []
  (stylefy/init {:dom (gdom/init)})
  (.resume audio-ctx)
  (render))
