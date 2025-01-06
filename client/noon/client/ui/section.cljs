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
                                     :folded [(or summary-button expand-button)]
                                     :expanded [fold-button])
        header-visibility (hooks/use-visible-intersection
                           header-ref
                           (fn [entry]
                             (let [r (.-intersectionRatio entry)]
                               (cond (= 0 r) :invisible
                                     (= 1 r) :visible
                                     :else :partial)))
                           {:root nil
                            :rootMargin "0px"
                            :threshold [0 1]})]

    ;; registering node
    (uix/use-effect (fn pouet []
                      #_(println "registering node" path)
                      (>> [:doc.ui.nodes.upd path (fn [node] (merge node {:header-ref header-ref
                                                                          :idx idx
                                                                          :title title
                                                                          :inline-code inline-code}))]))
                    [path title idx inline-code])

    ;; ping of visibility changes
    ;; it triggers the :current-path signal
    (uix/use-effect (fn pouet []
                      #_(println "trigg " path header-visibility)
                      (>> [:doc.ui.nodes.upd path (fn [node] (merge node {:header-visible header-visibility}))]))
                    [path header-visibility])

    (c :div.section
       {:id id
        :on-click (fn [_] (>> [:doc.ui.nodes.pp path]))}
       (c header
          {:ref header-ref
           :style {:flex [:start {:items :baseline :gap 1}]
                   :width :full
                   :border {:bottom [2 :grey1]}
                   :p {:bottom 1}}}
          (if inline-code (c :code title) title)
          (sc button-style left-button)
          (when right-button (sc button-style right-button)))

       (c :div
          {:style {:display (if (= :folded visibility) :none :block)
                   :p [0 0 0 2]}}
          children))))
