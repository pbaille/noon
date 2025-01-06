(ns noon.client.ui.breadcrumbs
  (:require [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]
            [noon.client.ui.utils :as ui.utils]
            [noon.client.state :refer [<<]]
            ["react-icons/tb" :as icons-tb]))

(defui breadcrumbs
  []
  (let [elements (<< [:doc.ui.breadcrumbs.get])
        mode (<< [:doc.ui.navigation-mode.get])]

    (c :div.breadcrumbs
       {:style (merge {:p [3 0]
                       :height 50
                       :z-index 1000
                       :width :full
                       :bg {:color :white}
                       :position [:fixed {:top 0 :left 0}]
                       :overflow-x :none
                       :overflow-y :hidden}
                      (if (or (empty? elements)
                              (not= :breadcrumbs mode))
                        {:display :none :border :none}
                        {:flex [:start :wrap {:items :baseline :gap 1}]
                         :border {:bottom [2 :grey1]}}))}

       (mapv (fn bc-item [{:keys [level href inline-code text]}]
               (c {:key href
                   :style {:flex-shrink 0 :flex [:row {:gap 1 :items :baseline}]}}
                  (when (> level 1)
                    (sc {:flex-shrink 0
                         :color :grey6}
                        (c icons-tb/TbCaretRightFilled)))
                  (c :a {:style {:flex-shrink 0 :color "inherit" :text-decoration "none"}
                         :href href}
                     (uix/$ (ui.utils/level->header-keyword level)
                            {:style {:margin 1}}
                            (if inline-code
                              (c :code text)
                              text)))))
             elements))))
