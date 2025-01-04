(ns noon.client.core
  (:require [noon.client.ui.doc :as ui]
            [uic.component :refer [c sc]]
            [uix.dom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]))

(defonce root (uix.dom/create-root (js/document.getElementById "app")))

(def styles {:p 0
             :text [:sans {:leading :normal}]
             ".cm-editor" {:bg {:color "transparent"}}
             "code" {:bg {:color [:gray {:a 0.1}]}
                     :color [:black {:a 0.8}]
                     :p [1 0.5]
                     :text :bold
                     :rounded 1}
             ".cm-editor.cm-focused" {:border {:width 0}
                                      :outline :none
                                      :box-shadow :none}
             :height "100vh"
             :width {:max 800}})

(defn render []
  (uix.dom/render-root
   (sc styles (c ui/doc))
   root))

(defn ^:dev/after-load reload []
  (render))

(defn ^:export init []
  (stylefy/init {:dom (gdom/init)})
  #_ (println guide/guide)
  (render))
