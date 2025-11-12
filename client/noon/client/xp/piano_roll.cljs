(ns noon.client.xp.piano-roll
  (:require [uix.core :as uix :refer [defui]]
            [uic.component :refer [sc c]]
            [uic.state :as state]
            [noon.eval :refer [score]]
            [noon.score :as score]
            [noon.harmonic-context :as hc]))

(defui piano-roll

  [{:keys [bounds get-styles notes resolution]}]

  (let [{pitch-range :pitch
         time-range :time} bounds

        time-ticks (range (time-range 0)
                          (inc (time-range 1))
                          (:time resolution))

        n-columns (- (time-range 1) (time-range 0))

        n-rows (- (pitch-range 1) (pitch-range 0))

        row-height (/ 1 n-rows)

        pitches (group-by :pitch notes)]

    (c {:style {:height :full
                :position :relative
                :border {:top (get-styles :border)
                         :left (get-styles :border)}}}

       ;; grid
       (mapv (fn [pitch]
               (c {:key (str "row-" pitch)
                   :style (merge {:flex :row
                                  :size [:full row-height]}
                                 (get-styles :row pitch))}

                  ;; notes
                  (mapv (fn [{:as note
                              :keys [position duration]}]
                          (c {:key (str note)
                              :style (merge {:size [(/ duration n-columns) row-height]
                                             :position [:absolute
                                                        {:left (/ position n-columns)}]}
                                            (get-styles :note note))}))
                        (pitches pitch))

                  ;; cells
                  (mapv (fn [tick]
                          (c {:key (str "cell-" pitch "-" tick)
                              :style (get-styles :cell)}))
                        (next time-ticks))))
             (reverse (range (pitch-range 0)
                             (pitch-range 1)))))))

(do :theme

    (defn get-styles [theme key & args]
      (if-let [x (get theme key)]
        (if (fn? x)
          (apply x theme args)
          x)
        (throw (js/Error. (str "unknown theme key: " key)))))

    (def piano-roll-theme
      {:border [2 :grey5]

       :row (fn [theme pitch]
              (merge {:border {:bottom (get-styles theme :border)}}
                     (let [color (get [:white :black
                                       :white :black
                                       :white
                                       :white :black
                                       :white :black
                                       :white :black
                                       :white]
                                      (mod pitch 12))]
                       (case color
                         :white {:bg {:color :grey2}}
                         :black {:bg {:color :grey4}}))))

       :cell (fn [theme]
               {:flexi [1 1 :auto]
                :border {:right (get-styles theme :border)}})

       :note (fn [theme {:keys [channel]}]
               {:border {:bottom (get-styles theme :border)}
                :bg {:color [(case channel
                               0 :light-skyblue
                               1 :tomato
                               2 :medium-orchid
                               3 :gold
                               4 :dark-seagreen
                               5 :coral
                               6 :light-green
                               7 :plum
                               8 :khaki
                               9 :salmon
                               10 :cornflower-blue
                               11 :sandy-brown
                               12 :pale-violetred
                               13 :medium-aquamarine
                               14 :light-coral
                               15 :rosy-brown)
                             {:a 0.7}]}})}))

(do :hooks

    (defn use-scroll-sync []
      (let [content-ref (uix/use-ref nil)
            top-ruler-ref (uix/use-ref nil)
            left-ruler-ref (uix/use-ref nil)]
        (uix/use-effect (fn []
                          (let [container @content-ref
                                top-ruler @top-ruler-ref
                                left-ruler @left-ruler-ref]
                            (when (and container top-ruler left-ruler)
                              (let [handle-scroll
                                    (fn []
                                      (when top-ruler
                                        (aset top-ruler "scrollLeft" (.-scrollLeft container)))
                                      (when left-ruler
                                        (aset left-ruler "scrollTop" (.-scrollTop container))))]
                                (.addEventListener container "scroll" handle-scroll)
                                (fn []
                                  (.removeEventListener container "scroll" handle-scroll)))))))
        {:content-ref content-ref
         :top-ruler-ref top-ruler-ref
         :left-ruler-ref left-ruler-ref}))

    (defn use-resizable []
      (let [ref (uix/use-ref)
            [size set-size] (uix/use-state [0 0])]
        (uix/use-effect
         (fn []
           (let [update-size
                 (fn []
                   (let [element @ref]
                     (when element
                       (let [rect (.getBoundingClientRect element)]
                         (set-size [(.-width rect) (.-height rect)])))))]

             (when @ref
               (update-size))

             (.addEventListener js/window "resize" update-size)
             (fn []
               (.removeEventListener js/window "resize" update-size)))) [])
        {:ref ref :size size})))

(defui scrollable-pianoroll
  [{:as props
    :keys [scrollbars cell-size bounds]}]

  (let [{:keys [content-ref top-ruler-ref left-ruler-ref]} (use-scroll-sync)
        {container-ref :ref [visible-width visible-height] :size} (use-resizable)
        n-ticks (- (get-in bounds [:time 1]) (get-in bounds [:time 0]))
        width (* (cell-size 0) n-ticks)
        n-rows (- (get-in bounds [:pitch 1]) (get-in bounds [:pitch 0]))
        height (* (cell-size 1) n-rows)
        bar-width (:thickness scrollbars)
        bars (quot n-ticks (get-in scrollbars [:resolution 0]))
        octaves (quot n-rows (get-in scrollbars [:resolution 1]))]

    (assert (= 0
               (rem n-ticks (get-in scrollbars [:resolution 0]))
               (rem n-rows (get-in scrollbars [:resolution 1])))
            "grid resolutions should be modulos of piano-roll ranges.")

    (c {:ref container-ref
        :style {:size :full
                :position :relative
                :overflow :hidden
                :rounded 2}}

       (sc {:size [bar-width bar-width]
            :bg {:color :grey10}})

       (c {:ref top-ruler-ref
           :style {:z-index 20
                   :position [:absolute {:left bar-width :top 0}]
                   :size [(- visible-width bar-width) bar-width]
                   :bg {:color :grey8}
                   :overflow :hidden}}
          (sc {:size [(- width bar-width) bar-width]
               :flex [:row {:items :center}]}
              (mapv (fn [i]
                      (c {:key i
                          :style {:p 0.5
                                  :m {:left 0.5 :right 0.5}
                                  :bg {:color :grey5}
                                  :rounded 1
                                  :flex-grow 1}}))
                    (range bars))))

       (c {:ref left-ruler-ref
           :style {:z-index 20
                   :position [:absolute {:top bar-width :left 0}]
                   :size [bar-width (- visible-height bar-width)]
                   :bg {:color :grey8}
                   :overflow :hidden}}
          (sc {:size [bar-width (- height bar-width)]
               :flex [:column {:items :center}]}
              (mapv (fn [i]
                      (c {:key i
                          :style {:p 0.5
                                  :m {:top 0.5 :bottom 0.5}
                                  :bg {:color :grey5}
                                  :rounded 1
                                  :flex-grow 1}}))
                    (range octaves))))

       (c {:ref content-ref
           :style {:size [(- visible-width bar-width) (- visible-height bar-width)]
                   :position [:absolute {:top bar-width :left bar-width}]
                   :overflow :scroll}}
          (sc {:size [(- width bar-width) (- height bar-width)]}
              (uix/$ piano-roll props))))))

(do :demos

    (def sample-score
      (score dur4
             (rep 4 d1)
             (chans d3
                    (each (tup d0 d3- d4-))
                    o1-)))

    (defn prepare-score [score]
      (keep (fn [e] (when (:pitch e)
                      (update e :pitch hc/hc->chromatic-value)))
            score))

    (comment :demo1

             (def frame (state/init-frame {:id :pr
                                           :tree {}
                                           :db {:bounds {:pitch [48 72] :time [0 20]}
                                                :resolution {:time 2}
                                                :limits {:time {:min 1}
                                                         :pitch {:min 0 :max 127}}}}))

             (defui num-input
               [{:keys [limits step value set-value]}]
               (c :input {:type "number"
                          :step step
                          :default-value value
                          :on-change (fn [v] (set-value (js/parseInt (.-value (.-target v)))))
                          :style {:p {:left 1}
                                  :color :grey8
                                  :rounded 1
                                  :text [:sm :bold]
                                  :bg {:color :grey2}
                                  :border [0 :white]
                                  :width 50}
                          :& (or limits {})}))

             (defui piano-roll-controls
               [{:keys [limits resolution subscribe dispatch]}]
               (sc {:m {:bottom 1} :flex [:row {:gap 1}]}
                   (mapv (fn [path]
                           (sc {:p 0.5
                                :bg {:color :grey1}
                                :flex [:row {:gap 1}]
                                :rounded 1}
                               (sc {:text [:md :bold]
                                    :color :grey10
                                    :p {:left 0.5}}
                                   (name (last path)))
                               (c num-input
                                  {:value (subscribe [:get (conj path 0)])
                                   :set-value (fn [v] (dispatch [:put (conj path 0) v]))
                                   :step (get resolution (last path) 1)
                                   :limits (get limits (last path))})
                               (c num-input
                                  {:value (subscribe [:get (conj path 1)])
                                   :set-value (fn [v] (dispatch [:put (conj path 1) v]))
                                   :step (get resolution (last path) 1)
                                   :limits (get limits (last path))})))
                         [[:bounds :pitch]
                          [:bounds :time]])))

             (defui demo []
               (let [[<< >>] frame]
                 (println "rerender demo")
                 (sc {:p 5
                      :size [:full 400]}
                     (c piano-roll-controls {:limits (<< [:get [:limits]])
                                             :resolution (<< [:get [:resolution]])
                                             :dispatch >> :subscribe <<})
                     (c piano-roll {:bounds (<< [:get :bounds])
                                    :resolution (<< [:get :resolution])
                                    :notes (prepare-score sample-score)
                                    :get-styles (partial get-styles piano-roll-theme)})))))

    (comment :demo2

             (defui demo []
               (let [size [1000 1270]]
                 (sc {:size [300 (size 1)]
                      :overflow-x :scroll
                      :overflow-y :hidden
                      :position :relative}
                     (sc {:position [:sticky {:top 0}]
                          :size [(size 0) 20]
                          :bg {:color :tomato}}
                         "top 0")
                     (sc {:size [(size 0) 300]
                          :overflow-y :scroll
                          :overflow-x :hidden
                          :position :sticky
                          :flex :row}
                         (sc {:position [:sticky {:left 0}]
                              :size [20 (size 1)]
                              :bg {:color :light-skyblue}}
                             "x")
                         (sc {:size size
                              :overflow :hidden}
                             (c piano-roll {:bounds {:time [0 20] :pitch [0 127]}
                                            :resolution {:time 1}
                                            :notes (prepare-score sample-score)
                                            :get-styles (partial get-styles piano-roll-theme)})))))))

    (comment :scrollable
             (defui demo []

               (sc {:height "100vh"
                    :p 2}
                   (c scrollable-pianoroll
                      {:cell-size [50 12]
                       :scrollbars {:thickness 15
                                    :resolution [4 12]}
                       :bounds {:time [0 20] :pitch [0 120]}
                       :resolution {:time 2}
                       :notes (prepare-score sample-score)
                       :get-styles (partial get-styles piano-roll-theme)}))))

    (do :note-kinds

        (defn score->harmonies [score & [strict]]

          (let [event-->harmony
                (fn [e] (dissoc (:pitch e) :position))

                validate
                (fn [xs]
                  (when strict
                    (assert
                     (every? (fn [[a b]] (<= (:position b) (+ (:position a) (:duration a))))
                             (partition 2 1 xs))
                     "overlapping harmonies"))
                  xs)]

            (->> score
                 (sort-by :position)
                 (partition-by event-->harmony)
                 (map (fn [xs]
                        (let [pos (:position (first xs))
                              dur (- (score/score-duration (set xs)) pos)]
                          {:position (:position (first xs))
                           :duration dur
                           :harmonic-ctx (event-->harmony (first xs))})))
                 (validate))))

        (defn score->notes [score]

          (->> (filter :pitch score)
               (sort-by :position)
               (map (fn [{:as e
                          p :pitch}]
                      (assoc (select-keys e [:position :duration :channel])
                             :pitch (hc/hc->chromatic-value p)
                             :kind (cond
                                     (hc/tonic-equivalent? p) :tonic
                                     (hc/structural-equivalent? p) :structural
                                     (hc/diatonic-equivalent? p) :diatonic
                                     :else :chromatic))))))

        (defn score->piano-roll-data
          "Turns a score into a datastructure suitable for elisp piano-roll display.
           `x` can be a score or something that holds a :score entry in metadata."
          [x]
          (if-let [score (if (score/score? x) x (some-> (meta x) :score))]
            {:notes (score->notes score)
             :harmonies (score->harmonies score)}))

        (defui demo []

          (let [{:keys [notes harmonies]} (score->piano-roll-data sample-score)]
            (sc {:height "100vh"
                 :p 2}
                (c scrollable-pianoroll
                   {:cell-size [50 12]
                    :scrollbars {:thickness 15
                                 :resolution [4 12]}
                    :bounds {:time [0 20] :pitch [0 120]}
                    :resolution {:time 1}
                    :notes notes
                    :get-styles (partial get-styles piano-roll-theme)}))))))
