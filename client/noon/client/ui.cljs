(ns noon.client.ui
  (:require [noon.eval :as eval]
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
            ["react-icons/lu" :refer [LuSquarePlus LuSquareMinus LuSquareMenu]]
            ["react-spinners/BeatLoader" :default spinner]))

(def DEFAULT_VISIBILITY
  :summary)

(def EDITOR_EXTENSIONS
  #js [(clojure)])

(do :help

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

    (defn use-visible-intersection [ref f options]
      (let [[return set-return] (uix/use-state false)]

        (uix/use-effect
         (fn []
           (let [observer (js/IntersectionObserver.
                           (fn [entries _observer]
                             (let [entry (aget entries 0)]
                               (set-return (f entry))))
                           (clj->js options))]

             (when @ref
               #_(println "ref is not nil")
               (.observe observer (.-current ref)))

             (fn []
               #_(println "release observer")
               (when @ref
                 (.unobserve observer @ref))
               (.disconnect observer))))
         [ref f options])

        return))

    (defn with-extra-props [component extra-props]
      (let [children (.-children (.-props component))]
        #_(js/console.log children (array? children))
        (uix/$ (.-type component)
               (merge (js->clj (.-argv (.-props component)))
                      extra-props)
               children))))

(defui code-editor [{:keys [source]}]
  (let [[source set-source] (uix/use-state source)
        [return set-return] (uix/use-state nil)
        [evaluating set-evaluating] (uix/use-state false)
        [playing set-playing] (uix/use-state false)
        color :light-skyblue
        error? (:error return)]

    (sc :.code-editor

        {:m [0 :1em]}

        (sc :.code-editor_input

            {:border {:width 2
                      :color [color {:a 0.2}]
                      :bottom (when return {:width 0})}
             :p 0
             :flex [:row {:items :center}]}

            (c :.code-editor-input_left-button

               {:style {:flex :center
                        :bg {:color [color {:a 0.1}]}
                        :color (if evaluating [color {:a 0.5}] color)
                        :p 1 :align-self :stretch}
                :on-click (if return
                            (fn [_]
                              (set-return nil)
                              (set-playing false)
                              (eval/stop))
                            (fn [_]
                              (if playing
                                (eval/stop)
                                (when-not evaluating
                                  (set-evaluating true)
                                  ;; this delay is needed for the set-evaluating call
                                  ;; to have effect before evaluation begins.
                                  (js/setTimeout (fn []
                                                   (eval/eval-string-async
                                                    source
                                                    (fn [x]
                                                      (println (some-> x :result))
                                                      (when-let [id (some-> x :result :id)]
                                                        (set-playing true)
                                                        (midi/on-done-playing id (fn [] (set-playing false))))
                                                      (set-evaluating false)
                                                      (set-return x))))
                                                 15)))))}
               (cond
                 playing (c icons-tb/TbPlayerStopFilled)
                 return (c icons-vsc/VscChevronUp)
                 :else (c icons-vsc/VscDebugStart)))

            (sc :.code-editor-input_content

                {:overflow :hidden
                 :position :relative
                 :width :full}

                (sc :.code-editor-input_overlay

                    {:z-index (if evaluating 10000 0)
                     :position [:absolute [0 0 0 0]]
                     :size :full
                     :bg {:color [:white {:a 0.5}]}
                     :flex :center}

                    (c spinner {:color "lightskyblue"  :loading evaluating :size 15}))

                (do #_(println "render code-mirror")
                    (c CodeMirror
                       {:value source
                        :on-change (fn [x] (set-source x))
                        :extensions EDITOR_EXTENSIONS
                        :basic-setup #js {:lineNumbers false
                                          :foldGutter false
                                          :highlightActiveLine false}}))))
        (when return

          (sc :code-editor-output

              {:border {:color [(if error? :red color) {:a 0.2}]
                        :width 2}
               :p 0
               :flex [:row {:items :center}]}

              (c :.code-editor-output_left-button

                 {:style {:flex :center
                          :bg {:color :white}
                          :border {:right [2 [(if error? :red color) {:a 0.2}]]}
                          :color (if error? :red color)
                          :p [1 0.5]
                          :align-self :stretch}
                  :on-click (fn [_] (set-return nil))}
                 (c icons-vsc/VscClose))

              (sc :.code-editor-output_content

                  {:bg {:color (if error? [:red {:a 0.05}] [color {:a 0.1}])}
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

(defn level->header-keyword [level]
  (case level 1 :h1 2 :h2 3 :h3 4 :h4 :h5))

(defui breadcrumbs
  [{:keys [elements button-style right-button]}]

  (sc {:m 0
       :z-index 1000
       :width :full
       :bg {:color :white}
       :position [:fixed {:top 0 :left 10}]
       :flex [:start {:items :baseline :gap 1}]
       :border {:bottom [2 :grey1]}}

      (sc {:flex [:row {:gap 1 :items :baseline}]}
          (mapcat (fn [{:keys [level href text]}]
                    [(c {:style button-style
                         :key (str level "-button")}
                        (c icons-tb/TbCaretRightFilled))
                     (c :a {:style {:color "inherit" :text-decoration "none"}
                            :href href
                            :key (str level "-link")}
                        (uix/$ (level->header-keyword level)
                               {:style {:margin 1}}
                               text))])
                  elements))

      (when right-button
        (sc button-style right-button))))

(defui section
  [{:keys [id level title children has-subsections inline-code]
    visibility-prop :visibility
    breadcrumbs-prop :breadcrumbs}]

  (let [header-ref (uix/use-ref)
        content-ref (uix/use-ref)
        content-loaded-ref (uix/use-ref false)
        [visibility-override set-visibility-override] (uix/use-state nil)

        visibility (or visibility-override visibility-prop DEFAULT_VISIBILITY)

        header (level->header-keyword level)

        button-style {:text [:md :bold]
                      :color :grey3
                      :hover {:color :tomato}}

        visibility-toggler (fn [value]
                             (fn [e] (.stopPropagation e) (set-visibility-override value)))

        fold-button (c LuSquareMinus
                       {:on-click (visibility-toggler :folded)})

        expand-button (c LuSquarePlus
                         {:on-click (visibility-toggler :expanded)})

        summary-button (when has-subsections
                         (c LuSquareMenu
                            {:on-click (visibility-toggler :summary)}))

        [left-button right-button] (case visibility
                                     :summary [fold-button expand-button]
                                     :folded [expand-button summary-button]
                                     :expanded [fold-button summary-button])

        header-visible (use-visible-intersection
                        header-ref
                        (fn [entry]
                          (.-isIntersecting entry))
                        {:root nil
                         :rootMargin "-55px"
                         :threshold 0})

        content-visible (use-visible-intersection
                         content-ref
                         (fn [entry]
                           (.-isIntersecting entry))
                         {:root nil
                          :rootMargin "0px"
                          :threshold 0})]

    (uix/use-effect #(set-visibility-override nil)
                    [visibility-prop])

    (c :div.section
       {:id id}
       (c header
          {:ref header-ref
           :style {:flex [:start {:items :baseline :gap 1}]
                   :border {:bottom [2 :grey1]}
                   :p {:bottom 1}}}
          (sc button-style left-button)
          (if inline-code (c :code title) title)
          (sc button-style right-button))

       (when (and content-visible (not header-visible))
         (c breadcrumbs
            {:elements breadcrumbs-prop
             :title title
             :button-style button-style
             :right-button right-button}))

       (when (or @content-loaded-ref
                 (not= :folded visibility))
         (reset! content-loaded-ref true)
         (c :div
            {:ref content-ref
             :style {:display (if (= :folded visibility) :none :block)
                     :p [0 0 0 2]}}

            (-> children
                (react/Children.map
                 (fn [c]
                   (if (= section (.-type c))
                     (with-extra-props c
                       {:visibility
                        (case visibility
                          :summary :folded
                          :expanded :expanded
                          nil)})
                     c)))))))))

(defui examples [{}]
  (c (map (fn [[k code]]
            (c {:key k}
               (c :h2 (name k))
               (c code-editor
                  {:source code})))
          ex/examples)))
