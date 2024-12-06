(ns noon.client.ui
  (:require [noon.client.eval :as eval]
            [uic.component :refer [c]]
            [uix.core :as uix :refer [$ defui]]
            ["@uiw/react-codemirror" :default CodeMirror]
            ["@nextjournal/lang-clojure" :refer [clojure]]))


(defui code-editor [{:keys [source]}]
  (let [[source set-source] (uix/use-state source)]
    (c :div
       ($ CodeMirror
          {:value source
           :on-change (fn [x] (set-source x))
           :extensions #js [(clojure)]})
       (c :button
          {:on-click (fn [_] (eval/evaluate-string (str "(play* " source ")")
                                                   (fn [{:keys [value]}]
                                                     (println value))
                                                   {:ns 'noon.user}))}
          "eval"))))
