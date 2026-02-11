(ns noon.client.playground
  (:require [noon.eval :as eval]
            [noon.output.midi :as midi]
            [noon.score :as score]
            [noon.viz.piano-roll :as pr]
            [uix.core :as uix :refer [defui $]]
            [uix.dom]
            [uic.component :refer [c sc]]
            [clojure.string :as str]
            ["@uiw/react-codemirror" :default CodeMirror]
            ["@nextjournal/lang-clojure" :refer [clojure]]
            ["@uiw/codemirror-themes-all" :as cm-themes]
            ["react-icons/vsc" :as icons-vsc]
            ["react-icons/tb" :as icons-tb]
            ["react-spinners/BeatLoader" :default spinner]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]))

;; ── Hiccup → HTML string ────────────────────────────────────────
;; Piano roll produces Clojure hiccup; we render it to an HTML string
;; and inject it via dangerouslySetInnerHTML (like ui.misc/raw).

(defn- style-map->str [m]
  (when (map? m)
    (->> m
         (map (fn [[k v]]
                (str (name k) ":" v)))
         (str/join ";"))))

(defn- attrs->str [attrs]
  (when attrs
    (->> (dissoc attrs :style)
         (map (fn [[k v]] (str " " (name k) "=\"" v "\"")))
         (apply str)
         (str (when-let [s (:style attrs)]
                (str " style=\"" (style-map->str s) "\""))))))

(defn hiccup->html
  "Recursively render Clojure hiccup to an HTML string."
  [form]
  (cond
    (nil? form)    ""
    (string? form) form
    (number? form) (str form)
    (vector? form)
    (let [[tag & rest] form
          [attrs children] (if (map? (first rest))
                             [(first rest) (next rest)]
                             [nil rest])
          tag-name (name tag)]
      (str "<" tag-name (attrs->str attrs) ">"
           (apply str (map hiccup->html children))
           "</" tag-name ">"))
    (seq? form) (apply str (map hiccup->html form))
    :else (str form)))

;; ── Constants ────────────────────────────────────────────────────

(def ^:private editor-extensions #js [(clojure)])
(def ^:private editor-theme cm-themes/quietlight)

(def ^:private accent :light-skyblue)



(def ^:private examples
  [["Triad arpeggio"
    "(play (tup s0 s1 s2))"]
   ["I-IV-V-I"
    "(play (lin I IV V I)\n      (each (tup s0 s1 s2)))"]
   ["Chromatic passing"
    "(play (lin I IV V I)\n      (each (tup s0 s1 s2 d1 c1 c1- d3 s0))\n      (adjust 2))"]
   ["Dorian melody"
    "(play (scale :dorian)\n      dur:2\n      (rup 12 (any-that (within-pitch-bounds? :C-1 :C1)\n                        d1 d2 d1- d2-)))"]
   ["Polyphonic"
    "(play\n (chans\n  [(patch :ocarina) dur:2 (lin G0 Eb0 C0 G-1 F0 D0 A-1 F-1)]\n  [(patch :vibraphone) dur2 vel3 (lin (par C0 Eb0 G0) (par A-1 F0 D0))]\n  [(patch :acoustic-bass) (lin [dur3 C-2] G-2)])\n (dup 4))"]
   ["Jazz pattern"
    "(play (tup c0 c2 c4 c7)\n      (tup c0 c3)\n      (rep 3 c4-))"]
   ["Scale comparison"
    "(score (tup d0 d1 d2 d3 d4 d5 d6 d7))"]])

(def ^:private default-code (second (nth examples 1)))

;; ── Pill button ──────────────────────────────────────────────────

(defui pill [{:keys [text selected on-click]}]
  (c :button
     {:style {:p [0.3 0.7]
              :text [:xs :medium]
              :font-family "'SF Mono', 'Fira Code', monospace"
              :border [1 (if selected :grey8 :grey3)]
              :rounded 1
              :bg {:color (if selected :grey8 :white)}
              :color (if selected :white :grey6)
              :cursor :pointer
              :hover {:border [1 :grey6]
                      :color (if selected :white :grey8)}}
      :on-click on-click}
     text))

;; ── Playground ───────────────────────────────────────────────────

(defui playground []
  (let [[source set-source] (uix/use-state default-code)
        [result set-result] (uix/use-state nil)
        [error set-error] (uix/use-state nil)
        [evaluating set-evaluating] (uix/use-state false)
        [playing set-playing] (uix/use-state false)
        [all-channels set-all-channels] (uix/use-state [0])
        [hidden-channels set-hidden-channels] (uix/use-state #{})

        do-eval
        (uix/use-callback
         (fn []
           (when-not evaluating
             (set-evaluating true)
             (set-error nil)
             (set-result nil)
             (js/setTimeout
              (fn []
                (eval/eval-string-async
                 source
                 (fn [{:keys [result error]}]
                   (set-evaluating false)
                   (if error
                     (set-error (.-message error))
                     (do
                       (when-let [id (:id result)]
                         (set-playing true)
                         (midi/on-done-playing id #(set-playing false)))
                       (let [s (cond
                                 (score/score? result) result
                                 (some-> (meta result) :score) (-> (meta result) :score)
                                 :else nil)]
                         (when s
                           (set-result s)
                           (set-all-channels (pr/score->channels s))
                           (set-hidden-channels #{}))))))
                 (fn [{:keys [error]}]
                   (set-evaluating false)
                   (set-error (.-message error)))))
              20)))
         [source evaluating])

        do-stop
        (uix/use-callback
         (fn []
           (eval/stop)
           (set-playing false))
         [])

        load-example
        (fn [code]
          (set-source code)
          (set-result nil)
          (set-error nil))]

    (sc {:height "100vh"
         :flex [:column {:gap 1}]
         :p 2
         :width {:max 1200}
         :m [0 :auto]}

        ;; ── Header ──────────────────────────────────────
        (sc {:flex [:row {:gap 1 :items :baseline}]}
            (sc {:text [:lg :bold] :color :grey9} "noon playground")
            (sc {:text [:xs] :color :grey4} "compose · visualize · listen"))

        ;; ── Examples bar ────────────────────────────────
        (sc {:flex [:row {:gap 0.5 :items :center :wrap :wrap}]}
            (sc {:text [:xs :bold] :color :grey4 :text-transform :uppercase :letter-spacing "0.5px" :p {:right 0.5}}
                "examples")
            (mapv (fn [[label code]]
                    ($ pill {:key label
                             :text label
                             :on-click #(load-example code)}))
                  examples))

        ;; ── Main content ────────────────────────────────
        (sc {:flex [:row {:gap 1}]
             :flexi [1 1 0]
             :overflow :hidden}

            ;; ── Left: editor + controls ─────────────────
            (sc {:flex [:column {:gap 0.75}]
                 :flexi [1 1 0]
                 :width {:min 300}
                 :overflow :hidden}

                ;; Editor
                (sc {:flexi [1 1 0]
                     :border [1 [accent {:a 0.25}]]
                     :rounded 1
                     :overflow :auto
                     :bg {:color :white}
                     ".cm-editor" {:bg {:color :transparent}}
                     ".cm-editor.cm-focused" {:outline :none}}
                    (c CodeMirror
                       {:value source
                        :on-change set-source
                        :extensions editor-extensions
                        :theme editor-theme
                        :basic-setup #js {:lineNumbers true
                                          :foldGutter false
                                          :highlightActiveLine true}}))

                ;; Toolbar
                (sc {:flex [:row {:gap 0.5 :items :center}]}

                    (c :button
                       {:style {:p [0.5 1.25]
                                :text [:sm :semibold]
                                :border {:width 0}
                                :rounded 1
                                :bg {:color (cond error :red
                                                  evaluating :grey3
                                                  :else accent)}
                                :color :white
                                :cursor (if evaluating :wait :pointer)
                                :flex [:row {:gap 0.5 :items :center}]
                                :hover {:bg {:color (if error [:red {:a 0.8}] [accent {:a 0.8}])}}}
                        :disabled evaluating
                        :on-click do-eval}
                       (if evaluating
                         (c spinner {:color "white" :loading true :size 8})
                         (c icons-vsc/VscDebugStart))
                       (if evaluating "Evaluating..." "Eval & Play"))

                    (when playing
                      (c :button
                         {:style {:p [0.5 1]
                                  :text [:sm :semibold]
                                  :border [1 :grey3]
                                  :rounded 1
                                  :bg {:color :white}
                                  :color :grey8
                                  :cursor :pointer
                                  :flex [:row {:gap 0.5 :items :center}]
                                  :hover {:border [1 :grey6]}}
                          :on-click do-stop}
                         (c icons-tb/TbPlayerStopFilled)
                         "Stop")))

                ;; Error
                (when error
                  (sc {:p [0.5 1]
                       :bg {:color [:red {:a 0.05}]}
                       :border [1 [:red {:a 0.2}]]
                       :rounded 1
                       :text [:xs :mono]
                       :color [:red {:a 0.7}]
                       :white-space :pre-wrap}
                      error)))

            ;; ── Right: visualization ────────────────────
            (sc {:flex [:column {:gap 0.75}]
                 :flexi [1 1 0]
                 :width {:min 300}
                 :overflow :hidden}

                ;; Channel toggles (shown when multi-channel)
                (when (> (count all-channels) 1)
                  (sc {:flex [:row {:gap 0.4 :items :center :wrap :wrap}]}
                      (sc {:text [:xs :bold] :color :grey4 :text-transform :uppercase :letter-spacing "0.5px" :p {:right 0.5}}
                          "channels")
                      (mapv (fn [ch]
                              (let [hidden? (contains? hidden-channels ch)]
                                ($ pill {:key (str "ch-" ch)
                                         :text (str "ch " ch)
                                         :selected (not hidden?)
                                         :on-click #(set-hidden-channels
                                                     (if hidden?
                                                       (disj hidden-channels ch)
                                                       ;; Don't allow hiding all channels
                                                       (if (< (count hidden-channels) (dec (count all-channels)))
                                                         (conj hidden-channels ch)
                                                         hidden-channels)))})))
                            all-channels)))

                ;; Piano roll
                (sc {:flexi [1 1 0]
                     :border [1 :grey2]
                     :rounded 1
                     :bg {:color :white}
                     :overflow :auto
                     :flex [:center]
                     :p 1}

                    (cond
                      evaluating
                      (sc {:flex :center :height 200}
                          (c spinner {:color "lightskyblue" :loading true :size 12}))

                      result
                      (let [visible-chs (let [v (remove hidden-channels all-channels)]
                                         (when (seq v) (vec v)))]
                        (c :div {:dangerouslySetInnerHTML
                                 #js {:__html (hiccup->html
                                               (pr/piano-roll result
                                                              (cond-> {:target-width 600}
                                                                visible-chs (assoc :channels visible-chs))))}}))

                      :else
                      (sc {:flex :center :height 200 :color :grey3 :text :sm}
                          "Press Eval & Play to see the piano roll"))))))))

;; ── Entry point ──────────────────────────────────────────────────

(defonce root (uix.dom/create-root (js/document.getElementById "app")))

(defn render []
  (uix.dom/render-root ($ playground) root))

(defn ^:dev/after-load reload []
  (render))

(defn ^:export init []
  (stylefy/init {:dom (gdom/init)})
  (render))
