(ns noon.client.ui.section
  (:require [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]
            ["react-icons/lu" :refer [LuSquarePlus LuSquareMinus LuSquareMenu]]
            [noon.client.state :refer [<< >>]]
            [noon.client.ui.utils :as ui.utils]))

(defui section
  [{:keys [id path idx level title children has-subsections inline-code]}]

  (let [header-ref (uix/use-ref)
        visibility (<< [:doc.ui.folding.get path])
        mode (<< [:doc.ui.navigation-mode.get])

        header (ui.utils/level->header-keyword level)

        button-style {:text [:md :bold]
                      :color :grey3
                      :hover {:color :tomato}}

        visibility-toggler (fn [value]
                             (fn [e] (.stopPropagation e) (>> [:doc.ui.folding.set path value])))

        fold-button (when (not (= 1 level))
                      (c LuSquareMinus
                         {:on-click (visibility-toggler :folded)}))

        expand-button (c LuSquarePlus
                         {:on-click (visibility-toggler :expanded)})

        summary-button (when has-subsections
                         (c LuSquareMenu
                            {:on-click (visibility-toggler :summary)}))

        [left-button right-button] (case visibility
                                     :summary [expand-button fold-button]
                                     :folded [(or summary-button expand-button)]
                                     :expanded [(or fold-button summary-button)])]

    ;; registering node
    (uix/use-effect (fn pouet []
                      #_(println "registering node" path)
                      (>> [:doc.ui.nodes.upd path (fn [node] (merge node {:header-ref header-ref
                                                                          :idx idx
                                                                          :title title
                                                                          :inline-code inline-code}))]))
                    [path title idx inline-code])

    (c :div.section
       {:on-click (fn [_] (>> [:doc.ui.nodes.pp path]))}
       (c header
          {:ref header-ref
           :id id
           :style (merge {:flex [:start {:items :baseline :gap 1}]
                          :width :full
                          :border {:bottom [2 :grey1]}
                          :p [0 1]}
                         (if-not (or (= mode :sidebar)
                                     (> level 1))
                           {:display :none}))}
          (if inline-code (c :code title) title)
          (sc button-style left-button)
          (when right-button (sc button-style right-button)))

       (c :div
          {:style {:display (if (= :folded visibility) :none :block)
                   :p [0 0 0 (case mode :breadcrumbs 1 :sidebar 2)]}}
          children))))
