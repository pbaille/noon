(ns noon.client.core
  (:require [noon.client.ui :as ui]
            [uix.core :refer [$]]
            [uic.component :refer [sc]]
            [uix.dom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]
            [noon.client.guide :as guide]))

(def ^:export audio-ctx (new js/AudioContext))

(defonce root (uix.dom/create-root (js/document.getElementById "app")))

(defn render []
  (uix.dom/render-root
   (sc {:p 2
        :text [:sans {:leading :normal}]
        ".cm-editor" {:bg {:color "transparent"}}}
       ($ guide/guide))
   #_($ ui/examples)
   root))

(defn ^:dev/after-load reload []
  (.resume audio-ctx)
  (render))

(defn ^:export init []
  (stylefy/init {:dom (gdom/init)})
  (.resume audio-ctx)
  #_(println guide/guide)
  (render))
