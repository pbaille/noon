(ns noon.client.ui.breadcrumbs
  (:require [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]
            [noon.client.ui.utils :as ui.utils]
            [noon.client.state :refer [<<]]
            [noon.client.constants :as constants]
            ["react-icons/tb" :as icons-tb]))

(defn scroll-to-element [element-id & [behavior]]
  (let [element (.getElementById js/document element-id)
        element-position (.-offsetTop element)
        scroll-options #js {:top (+ (- element-position constants/BREADCRUMBS_HEIGHT)
                                    (.-offsetHeight element))
                            :left 0
                            :behavior (or behavior "smooth")}
        container (.getElementById js/document constants/DOC_CONTAINER_ID)]
    (when element
      (.scrollTo container scroll-options)
      (ui.utils/set-url-hash-no-scroll element-id))))

(defui breadcrumbs
  []
  (let [elements (<< [:doc.ui.breadcrumbs.get])
        mode (<< [:doc.ui.navigation-mode.get])]

    (c :div.breadcrumbs
       {:style (merge {:p 0
                       :height constants/BREADCRUMBS_HEIGHT
                       :z-index 1000
                       :width :full
                       :bg {:color :white}
                       :flex-shrink 0
                       :overflow :hidden
                       :flex [:start :wrap {:items :baseline :gap 1}]
                       :border {:bottom [2 :grey1]}}
                      (if (not= :breadcrumbs mode)
                        {:height 0 :p [0 0] :border {:border [0 :white]}}))}

       (mapv (fn bc-item [{:keys [level href inline-code text]}]
               (let [header (ui.utils/level->header-keyword level)]
                 (c {:key href
                     :style {:flex-shrink 0 :flex [:row {:gap 1 :items :baseline}]}}
                    (when (> level 1)
                      (sc {:flex-shrink 0
                           :color :grey6}
                          (c icons-tb/TbCaretRightFilled)))
                    (c header {:on-click (fn [_]
                                           (scroll-to-element (subs href 1)))
                               :style {:p 0
                                       :cursor :pointer
                                       :hover {:color :light-skyblue}}}
                       (if inline-code
                         (c :code text)
                         text)))))
             elements))))
