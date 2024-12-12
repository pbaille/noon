(ns noon.client.ui
  (:require [noon.client.eval :as eval]
            [uix.core :as uix :refer [$ defui]]
            [uic.component :refer [c sc]]
            ["@uiw/react-codemirror" :default CodeMirror]
            ["@nextjournal/lang-clojure" :refer [clojure]]
            ["react-icons/vsc" :as icons]))


(defui code-editor [{:keys [source
                            resume-audio-ct]}]
  (let [[source set-source] (uix/use-state source)
        [return set-return] (uix/use-state nil)
        color :light-skyblue]
    (c (sc {:border [2 [color {:a 0.2}]]
            :p [0.3 0.7]}
           (c CodeMirror
              {:value source
               :on-change (fn [x] (set-source x))
               :extensions #js [(clojure)]
               :basic-setup #js {:lineNumbers false
                                 :foldGutter false
                                 :highlightActiveLine false}}))
       (c {:style {:text :sm
                   :color color
                   :bg {:color [color {:a 0.2}]}
                   #_:rounded #_[0 0 2 2]
                   :overflow :hidden
                   :flex [:start {:justify :space-around :items :center}]}}
          (c {:style {:flex :center :flexi [1 1 :auto] :p 1}
              :on-click (fn [_] (println (eval/sci-eval source)))}
             (c icons/VscDebugStart))
          (c {:style {:flex :center
                      :flexi [1 1 :auto]
                      :bg {:color [color {:a 0.2}]}
                      :color [color {:l 0.5}]
                      :p 1}
              :on-click (fn [_] (set-return (str (eval/sci-eval source))))}
             (c icons/VscDebugLineByLine)))
       (when return
         (sc {:border [2 [color {:a 0.2}]]
              :p [0.3 0.7]}
             (c CodeMirror
                {:value return
                 :editable false
                 :extensions #js [(clojure)]
                 :basic-setup #js {:lineNumbers false
                                   :foldGutter false
                                   :highlightActiveLine false}}))))))

(defui section [{:keys [level title children]}]
  ($ :div
     ($ (case level
          1 :h1
          2 :h2
          :h3) title)
     (c :div
        {:style {:p [2 0]}}
        children)))
