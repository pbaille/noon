(ns noon.client.ui
  (:require [noon.client.eval :as eval]
            [uix.core :as uix :refer [$ defui]]
            ["@uiw/react-codemirror" :default CodeMirror]
            ["@nextjournal/lang-clojure" :refer [clojure]]))


(defui code-editor [{:keys [source
                            resume-audio-ctx]}]
  (let [[source set-source] (uix/use-state source)]
    ($ :div
       ($ CodeMirror
          {:value source
           :on-change (fn [x] (set-source x))
           :extensions #js [(clojure)]})
       ($ :button
          {:on-click (fn [_]
                       (println (eval/sci-eval source)))}
          "eval")
       ($ :button
          {:on-click (fn [_]
                       (resume-audio-ctx)
                       (println (eval/sci-play source)))}
          "play"))))
