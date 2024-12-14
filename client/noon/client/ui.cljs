(ns noon.client.ui
  (:require [noon.client.eval :as eval]
            [noon.midi :as midi]
            [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]
            [noon.client.examples :as ex]
            [noon.utils.misc :as u]
            [clojure.string :as str]
            ["react" :as react]
            ["@uiw/react-codemirror" :default CodeMirror]
            ["@nextjournal/lang-clojure" :refer [clojure]]
            ["react-icons/vsc" :as icons-vsc]
            ["react-icons/tb" :as icons-tb]
            ["react-icons/lu" :refer [LuSquarePlus LuSquareMinus LuAlignJustify LuChevronsDown LuSquareMenu]]
            ["react-spinners/BeatLoader" :default spinner]
            ["tone" :as Tone]))

(defn use-atom [atom]
  (let [[state set-state] (uix/use-state @atom)]

    (uix/use-effect
     (fn []
       (let [watch-key (str (gensym "atom-watch-"))]
         (add-watch atom watch-key
                    (fn [_ _ _ new-state]
                      (set-state new-state)))
         #(remove-watch atom watch-key)))
     [atom])

    state))

(defui code-editor [{:keys [source]}]
  (let [[source set-source] (uix/use-state source)
        [return set-return] (uix/use-state nil)
        [evaluating set-evaluating] (uix/use-state false)
        color :light-skyblue
        error? (:error return)
        playing (use-atom midi/playing*)]
    (sc {:m [0 :1em]}
        (sc {:border {:width 2
                      :color [color {:a 0.2}]
                      :bottom (when return {:width 0})}
             :p 0
             :flex [:row {:items :center}]}
            (c {:style {:flex :center
                        :bg {:color [color {:a 0.1}]}
                        :color (if evaluating [color {:a 0.5}] color)
                        :p 1 :align-self :stretch}
                :on-click (if return
                            (fn [_]
                              (set-return nil)
                              (eval/stop-audio))
                            (fn [_]
                              (if playing
                                (eval/stop-audio)
                                (when-not evaluating
                                  (set-evaluating true)
                                  (js/setTimeout (fn []
                                                   (eval/sci-eval-async
                                                    source
                                                    (fn [x] (set-evaluating false) (set-return x))))
                                                 15)))))}
               (cond
                 playing (c icons-tb/TbPlayerStopFilled)
                 return (c icons-vsc/VscChevronUp)
                 :else (c icons-vsc/VscDebugStart)))
            (sc {:overflow :hidden
                 :position :relative
                 :width :full}
                (sc {:z-index (if evaluating 10000 0)
                     :position [:absolute [0 0 0 0]]
                     :size :full
                     :bg {:color [:white {:a 0.5}]}
                     :flex :center}
                    (c spinner {:color "lightskyblue"  :loading evaluating :size 15}))

                (c CodeMirror
                   {:value source
                    :on-change (fn [x] (set-source x))
                    :extensions #js [(clojure)]
                    :basic-setup #js {:lineNumbers false
                                      :foldGutter false
                                      :highlightActiveLine false}})))
        (when return
          (sc {:border {:color [(if error? :red color) {:a 0.2}]
                        :width 2}
               :p 0
               :flex [:row {:items :center}]}
              (c {:style {:flex :center
                          :bg {:color :white}
                          :border {:right [2 [(if error? :red color) {:a 0.2}]]}
                          :color (if error? :red color)
                          :p [1 0.5]
                          :align-self :stretch}
                  :on-click (fn [_] (set-return nil))}
                 (c icons-vsc/VscClose))
              (sc {:bg {:color (if error? [:red {:a 0.05}] [color {:a 0.1}])}
                   :color (if (:error return) [:red {:a 0.45}])
                   :p (if error? [0.5 0.3] [0.3 0.7])
                   :flexi [1 1 :auto]}
                  (c CodeMirror
                     {:value (str/trim (or (some-> return :error .-message)
                                           (u/pretty-str (:result return))))
                      :editable false
                      :extensions #js [(clojure)]
                      :basic-setup #js {:lineNumbers false
                                        :foldGutter false
                                        :highlightActiveLine false}})))))))

(defn with-extra-props [component extra-props]
  (uix/$ (.-type component)
         (merge (js->clj (.-argv (.-props component)))
                extra-props)
         (.-children (.-props component))))

(defui section
  [{:keys [level title children]
    visibility-prop :visibility}]

  (let [[visibility set-visibility] (uix/use-state :expanded)
        header (case level 1 :h1 2 :h2 :h3)

        button-style {:text [:md :bold]
                      :color :grey3
                      :hover {:color :tomato}}

        [left-button right-button]
        (case visibility
          :summary [(c LuSquareMinus
                       {:on-click (fn [_] (set-visibility :folded))})
                    (c LuSquarePlus
                       {:on-click (fn [_] (set-visibility :expanded))})]
          :folded [(c LuSquareMenu
                      {:on-click (fn [_] (set-visibility :summary))})
                   (c LuSquarePlus
                      {:on-click (fn [_] (set-visibility :expanded))})]
          :expanded [(c LuSquareMinus
                        {:on-click (fn [_] (set-visibility :folded))})
                     (c LuSquareMenu
                        {:on-click (fn [_] (set-visibility :summary))})])]

    (uix/use-effect #(set-visibility (or visibility-prop :expanded))
                    [visibility-prop])

    (c :div.section

       (c header
          {:style {:flex [:start {:items :baseline :gap 1}]
                   :border {:bottom [2 :grey1]}
                   :p {:bottom 1}}}
          (sc button-style left-button)
          title
          (sc button-style right-button))

       (c :div
          {:style {:display (if (= :folded visibility) :none :block)
                   :p [0 0 0 2]}}

          (-> children
              (react/Children.map
               (fn [c]
                 (if (and (= :summary visibility) (= section (.-type c)))
                   (with-extra-props c {:visibility :folded})
                   c))))))))

(defui examples [{}]
  (c (map (fn [[k code]]
            (c {:key k}
               (c :h2 (name k))
               (c code-editor
                  {:source code})))
          ex/examples)))
