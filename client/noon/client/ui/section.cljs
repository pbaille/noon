(ns noon.client.ui.section
  (:require [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]
            ["react-icons/lu" :refer [LuSquarePlus LuSquareMinus LuSquareMenu]]
            [noon.client.state :refer [<< >>]]
            [noon.client.ui.hooks :as hooks]
            [noon.client.ui.utils :as ui.utils]))

(defui section
  [{:keys [id path idx level title children has-subsections inline-code]}]

  (let [header-ref (uix/use-ref)
        content-ref (uix/use-ref)
        visibility (<< [:doc.ui.folding.get path])

        header (ui.utils/level->header-keyword level)

        button-style {:text [:md :bold]
                      :color :grey3
                      :hover {:color :tomato}}

        visibility-toggler (fn [value]
                             (fn [e] (.stopPropagation e) (>> [:doc.ui.folding.set path value])))

        fold-button (c LuSquareMinus
                       {:on-click (visibility-toggler :folded)})

        expand-button (c LuSquarePlus
                         {:on-click (visibility-toggler :expanded)})

        summary-button (when has-subsections
                         (c LuSquareMenu
                            {:on-click (visibility-toggler :summary)}))

        [left-button right-button] (case visibility
                                     :summary [expand-button fold-button]
                                     :folded [summary-button]
                                     :expanded [fold-button])

        header-visible (hooks/use-visible-intersection
                        header-ref
                        (fn [entry]
                          (.-isIntersecting entry))
                        {:root nil
                         :rootMargin "-55px"
                         :threshold 0})

        content-visible (hooks/use-visible-intersection
                         content-ref
                         (fn [entry]
                           (.-isIntersecting entry))
                         {:root nil
                          :rootMargin "0px"
                          :threshold 0})]

    (uix/use-effect (fn pouet []
                      (>> [:doc.ui.nodes.set path :idx idx])
                      (>> [:doc.ui.nodes.set path :header-visible header-visible])
                      (>> [:doc.ui.nodes.set path :content-visible content-visible]))
                    [content-visible header-visible path idx])

    (c :div.section
       {:id id}
       (c header
          {:ref header-ref
           :style {:flex [:start {:items :baseline :gap 1}]
                   :border {:bottom [2 :grey1]}
                   :p {:bottom 1}}}
          (if inline-code (c :code title) title)
          (sc button-style left-button)
          (when right-button (sc button-style right-button)))

       (c :div
          {:ref content-ref
           :style {:display (if (= :folded visibility) :none :block)
                   :p [0 0 0 2]}}
          children))))
