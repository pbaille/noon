(ns noon.client.ui.misc
  (:require [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]))

(defui badge [{:keys [color size text]}]
  (sc {:text size
       :color color
       :rounded 3
       :bg {:color [color {:a 0.1}]}
       :p [1 0]}
      text))

(defui raw [{:keys [html]}]
  (c :div {:dangerouslySetInnerHTML #js {:__html html}}))
