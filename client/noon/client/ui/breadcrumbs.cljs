(ns noon.client.ui.breadcrumbs
  (:require [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]
            [noon.client.ui.utils :as ui.utils]
            [noon.client.state :refer [<< >>]]
            [noon.client.constants :as constants]
            ["react-icons/tb" :as icons-tb]
            ["react-icons/fa" :refer [FaGithub]]))

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
       {:style (merge {:m {:right 3}
                       :height constants/BREADCRUMBS_HEIGHT
                       :z-index 1000
                       #_:width #_:full
                       :bg {:color :white}
                       :flex-shrink 0
                       :overflow :hidden
                       :flex [:start :wrap {:items :baseline :gap 1}]
                       :border {:bottom [2 :grey1]}
                       :position :relative}
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
             elements)

       (sc {:position [:absolute {:right 0}]
            :flex [:row {:items :center :gap 0.5}]}

           (let [active? (<< [:piano-rolls.get])]
             (c :button
                {:style {:p [0.3 0.5]
                         :border {:width 1
                                  :color (if active? "#87ceeb" "#e2e8f0")}
                         :rounded 0.4
                         :bg {:color (if active? [:light-skyblue {:a 0.1}] :white)}
                         :cursor :pointer
                         :font-size "13px"
                         :transition "all 0.15s ease"
                         :hover {:border {:color (if active? "#5bb8db" "#94a3b8")}}}
                 :title (if active? "Hide all piano rolls" "Show all piano rolls")
                 :on-click (fn [_] (>> [:piano-rolls.toggle]))}
                "🎹"))

           (c :a
              {:style {:p 2
                       :text :xl
                       :color :grey6
                       :hover {:color :light-skyblue}}
               :href constants/GITHUB_REPO_URL}
              (c FaGithub))))))
