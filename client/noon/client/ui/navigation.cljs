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
              :href (str "#" id)}
             (if inline-code (c :code {:class (when focus? "focus")} title) title)))

       (c :div
          {:style {:display (if (= :folded visibility) :none :block)
                   :p [0 0 0 2]}}

          children))))

(defn render-sidebar-node [node]
  (when (= :section (:type node))
    (uix/$ sidebar-section
           (assoc node :key (str (:idx node)))
           (keep render-sidebar-node (sort-by :idx (vals (:children node)))))))

(defui sidebar []
  (sc :div
      {:flex :column
       :bg {:color [:gray {:a 0.05}]}
       :p [3 3 0 2]
       ; :border {:right [2 :grey2]}
       :height "100vh"
       :width 250
       :overflow :scroll
       :flexi [1 0 :auto]
       :align-self :stretch
       ;; :transition "width 0.3s ease"
       }
      (keep render-sidebar-node (sort-by :idx (vals (:children doc/doc-data))))))

(defui breadcrumbs
  []
  (when-let [elements (<< [:doc.ui.breadcrumbs.get])]

    (sc {:m [3 0]
         :z-index 1000
         :width :full
         :bg {:color :white}
         :position [:fixed {:top 0 :left 0}]
         :flex [:start {:items :baseline :gap 1}]
         :border {:bottom [2 :grey1]}
         :overflow-x :scroll}

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
