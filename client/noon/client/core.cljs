(ns noon.client.core
  (:require [noon.client.ui.doc :as ui]
            [uic.component :refer [c sc]]
            [uix.dom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]))

(defonce root (uix.dom/create-root (js/document.getElementById "app")))

(defn render []
  (uix.dom/render-root
   (c ui/doc)
   root))

(defn ^:dev/after-load reload []
  (render))

(defn ^:export init []
  (stylefy/init {:dom (gdom/init)})
  #_ (println guide/guide)
  (render))
