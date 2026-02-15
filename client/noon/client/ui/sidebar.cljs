(ns noon.client.ui.sidebar
  (:require [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]
            [noon.client.ui.utils :as ui.utils]
            [noon.client.state :refer [<< >>]]
            [noon.client.constants :as constants]
            [noon.client.doc :as doc]
            ["react-icons/tb" :as icons-tb :refer [TbPiano]]
            ["react-icons/fa" :refer [FaGithub]]))

(def link-styles
  {:flex-shrink 0
   :color "inherit"
   :text :semibold
   :text-decoration "none"
   :hover {:color :light-skyblue}})

(defn goto-element [element-id]
  (let [element (.getElementById js/document element-id)
        element-position (.-offsetTop element)
        scroll-options #js {:top element-position
                            :left 0
                            :behavior "instant"}
        container (.getElementById js/document constants/DOC_CONTAINER_ID)]
    (when element
      (.scrollTo container scroll-options)
      (ui.utils/set-url-hash-no-scroll element-id))))

(defui sidebar-section
  [{:keys [id path level title children inline-code]}]

  (let [visibility (<< [:doc.ui.sidebar.folding.get path])

        current-path (<< [:doc.ui.current-path])

        header (ui.utils/level->header-keyword (inc level))

        button-style {:text [:md :bold]
                      :color :grey3
                      :hover {:color :tomato}
                      :flex-shrink 0}

        visibility-toggler (fn [value]
                             (fn [e] (.stopPropagation e) (>> [:doc.ui.sidebar.folding.set path value])))

        fold-button (c icons-tb/TbCaretDownFilled
                       {:on-click (visibility-toggler :folded)})

        children? (seq children)

        summary-button (if children?
                         (c icons-tb/TbCaretRightFilled
                            {:on-click (visibility-toggler :expanded)})
                         (c icons-tb/TbPoint))

        button (case visibility
                 :expanded fold-button
                 :folded summary-button)
        focus? (= current-path path)]

    #_(println current-path)
    (c :div.section
       {:key (str path)}
       (c header
          {:style {:m [0 1]
                   :flex [:start {:items :baseline :gap 1}]}}
          (sc button-style button)
          (c :a
             {:style (merge link-styles
                            (when focus?
                              {:color :tomato}))
              :on-click (fn [e]
                          (goto-element id)
                          (if children? ((visibility-toggler :expanded) e)))}
             (if inline-code (c :code {:class (when focus? "focus")} title) title)))

       (sc {:p {:left 0.85}}
           (sc {:display (if (= :folded visibility) :none :block)
                :p {:left 1.15}
                :border {:left [2 [:gray {:a 0.1}]]}}

               children)))))

(defn render-sidebar-node [node]
  (when (= :section (:type node))
    (uix/$ sidebar-section
           (assoc node :key (str (:idx node)))
           (keep render-sidebar-node (sort-by :idx (vals (:children node)))))))

(def sidebar-elements
  (keep render-sidebar-node (sort-by :idx (vals (:children doc/doc-data)))))

(defui sidebar []
  (let [mode (<< [:doc.ui.navigation-mode.get])
        icon (case mode
               :sidebar icons-tb/TbLayoutSidebarLeftCollapseFilled
               :breadcrumbs icons-tb/TbLayoutSidebarLeftExpandFilled)]
    (sc {:flex [:row {:items :center}]}
        (sc :div
            {:flex :column
             :bg {:color [:gray {:a 0.05}]}
             ; :border {:right [2 :grey2]}
             :height "100vh"
             :overflow :scroll
             :flexi [1 0 :auto]
             :align-self :stretch
             :position :relative
             :& (case mode
                  :sidebar {:width 250
                            :p [3 3 0 2]}
                  :breadcrumbs {:width 0
                                :p 0})
             :transition "all 0.3s ease"}
            sidebar-elements
            (sc {:position [:absolute {:bottom 0 :right 0}]
                 :flex [:row {:items :center :gap 0.5}]
                 :p 1}

                (let [active? (<< [:piano-rolls.get])]
                  (c :button
                     {:style {:p 0.3
                              :border {:width 0}
                              :bg {:color :transparent}
                              :color (if active? :light-skyblue :grey6)
                              :cursor :pointer
                              :text :xl
                              :transition "all 0.15s ease"
                              :hover {:color (if active? "#5bb8db" :light-skyblue)}}
                     :title (if active? "Hide all piano rolls" "Show all piano rolls")
                     :on-click (fn [_] (>> [:piano-rolls.toggle]))}
                     (c TbPiano)))

                (c :a
                   {:style {:text :xl
                            :color :grey6
                            :hover {:color :light-skyblue}}
                    :href constants/GITHUB_REPO_URL}
                   (c FaGithub))))
        (case mode
          :sidebar (sc {:p [0.5 1]
                        :width 25
                        :color :grey6
                        :rounded [0 1 1 0]
                        :bg {:color [:gray {:a 0.05}]}
                        :hover {:color :tomato}}
                       (c icon {:style {:width 25}
                                :on-click (fn [] (>> [:doc.ui.navigation-mode.set :breadcrumbs]))}))
          :breadcrumbs (c {:on-click (fn [] (>> [:doc.ui.navigation-mode.set :sidebar]))
                           :style {:p [0.5 1]
                                   :width 10
                                   :height 30
                                   :color :grey6
                                   :rounded [0 1 1 0]
                                   :bg {:color [:tomato {:a 0.4}]}
                                   :hover {:bg {:color [:tomato {:a 0.7}]}}}})))))
