(ns noon.client.core
  (:require #_[noon.client.ui :as ui]
            [uix.core :refer [$]]
            [uic.component :refer [sc]]
            [uix.dom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]
            [noon.client.guide :as guide]))

(defonce root (uix.dom/create-root (js/document.getElementById "app")))

(defn render []
  (uix.dom/render-root
   (sc {:p [2 0]
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
        :margin {:bottom "100vh"}
        :width {:max 800}
        }
       ($ guide/guide)
       #_($ ui/section
          {:level 1 :path [] :title "Examples"
           :visibility :folded}
          ($ ui/examples)))
   root))

(defn ^:dev/after-load reload []
  (render))

(defn ^:export init []
  (stylefy/init {:dom (gdom/init)})
  #_ (println guide/guide)
  (render))
