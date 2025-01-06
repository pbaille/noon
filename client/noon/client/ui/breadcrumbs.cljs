(ns noon.client.ui.breadcrumbs
  (:require [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]
            [noon.client.ui.utils :as ui.utils]
            [noon.client.ui.hooks :as hooks]
            [noon.client.state :refer [<< >>]]
            ["react-icons/tb" :as icons-tb]))

(defui breadcrumbs
  []
  (let [container-ref (uix/use-ref)
        breadcrumbs-ref (uix/use-ref)
        elements (<< [:doc.ui.breadcrumbs.get])
        mode (<< [:doc.ui.navigation-mode.get])]

    (uix/use-layout-effect (fn []
                             (when (seq (:path (last elements)))
                               (>> [:doc.ui.nodes.upd (:path (last elements))
                                    (fn [node] (merge node {:breadcrumbs-ref breadcrumbs-ref}))])
                               (when (and container-ref breadcrumbs-ref)
                                 (println @container-ref)
                                 (let [content-box (.getBoundingClientRect @breadcrumbs-ref)
                                       container-box (.getBoundingClientRect @container-ref)
                                       overflow (- (.-right content-box) (.-width container-box))]
                                   (when (> overflow -30)
                                     (>> [:doc.ui.breadcrumbs.shrink (:path (last elements))]))))))
                           [elements])

    (c :div.breadcrumbs
       {:ref container-ref
        :style {:m [3 0]
                :z-index 1000
                :width :full
                :bg {:color :white}
                :position [:fixed {:top 0 :left 0}]
                :overflow-x :scroll
                :& (if (or (empty? elements)
                           (not= :breadcrumbs mode))
                     {:display :none :border :none}
                     {:flex [:start {:items :baseline :gap 1}]
                      :border {:bottom [2 :grey1]}})}}

       (c {:ref breadcrumbs-ref
           :style {:height 50
                   :flex [:row {:gap 1 :items :baseline}]}}
          (next (mapcat (fn [{:keys [level href text inline-code]}]
                          [(c {:style {:flex-shrink 0
                                       :color :grey6}
                               :key (str level "-button")}
                              (c icons-tb/TbCaretRightFilled))
                           (c :a {:style {:flex-shrink 0 :color "inherit" :text-decoration "none"}
                                  :href href
                                  :key (str level "-link")}
                              (uix/$ (ui.utils/level->header-keyword level)
                                     {:style {:margin 1}}
                                     (if inline-code
                                       (c :code text)
                                       text)))])
                        elements)))

       #_(when right-button
           (sc button-style right-button)))))
