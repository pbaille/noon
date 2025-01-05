(ns noon.client.ui.navigation
  (:require [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]
            [noon.client.ui.utils :as ui.utils]
            [noon.client.state :refer [<< >>]]
            [noon.client.doc :as doc]
            ["react-icons/tb" :as icons-tb]))

(def link-styles
  {:flex-shrink 0
   :color "inherit"
   :text :semibold
   :text-decoration "none"
   :hover {:color :light-skyblue}})

(defui sidebar-section
  [{:keys [id path level title children inline-code]}]

  (let [visibility (<< [:doc.ui.sidebar.folding.get path])

        current-path (<< [:doc.ui.current-path])

        header (ui.utils/level->header-keyword (inc level))

        button-style {:text [:md :bold]
                      :color :grey3
                      :hover {:color :tomato}}

        visibility-toggler (fn [value]
                             (fn [e] (.stopPropagation e) (>> [:doc.ui.sidebar.folding.set path value])))

        fold-button (c icons-tb/TbCaretDownFilled
                       {:on-click (visibility-toggler :folded)})

        summary-button (if (seq children)
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
              :href (str "#" id)
              :on-click (visibility-toggler :expanded)}
             (if inline-code (c :code {:class (when focus? "focus")} title) title)))

       (sc {:p {:left 0.85}}
           (sc {:display (if (= :folded visibility) :none :block)
                :p {:left 1.15}
                :border {:left [2 [:gray {:a 0.1}] ]}}

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
             :& (case mode
                  :sidebar {:width 250
                            :p [3 3 0 2]}
                  :breadcrumbs {:width 0
                                :p 0})
              :transition "all 0.3s ease"
             }
            sidebar-elements)
        (sc {:p [0.5 1]
             :width 25
             :color :grey6
             :rounded [0 1 1 0]
             :bg {:color [:gray {:a 0.05}]}
             :hover {:color :tomato}}
            (c icon {:style {:width 25}
                     :on-click (fn [] (>> [:doc.ui.navigation-mode.set
                                           (case mode
                                             :sidebar :breadcrumbs
                                             :breadcrumbs :sidebar)]))})))))

(defui breadcrumbs
  []
  (let [elements (<< [:doc.ui.breadcrumbs])
        mode (<< [:doc.ui.navigation-mode.get])]
    (sc :div.breadcrumbs
        {:m [3 0]
         :z-index 1000
         :width :full
         :bg {:color :white}
         :position [:fixed {:top 0 :left 0}]
         :overflow-x :scroll
         :& (if (or (empty? elements)
                    (not= :breadcrumbs mode))
              {:display :none :border :none}
              {:flex [:start {:items :baseline :gap 1}]
               :border {:bottom [2 :grey1]}})}

        (sc {:flex [:row {:gap 1 :items :baseline}]}
            (mapcat (fn [{:keys [level href text]}]
                      [(c {:style {}
                           :key (str level "-button")}
                          (c icons-tb/TbCaretRightFilled))
                       (c :a {:style {:flex-shrink 0 :color "inherit" :text-decoration "none"}
                              :href href
                              :key (str level "-link")}
                          (uix/$ (ui.utils/level->header-keyword level)
                                 {:style {:margin 1}}
                                 text))])
                    elements))

        #_(when right-button
            (sc button-style right-button)))))
