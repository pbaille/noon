(ns noon.client.ui.hooks
  (:require [uix.core :as uix]))

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

(defn use-window-size []
  (let [[size set-size] (uix/use-state [0 0])]
    (uix/use-layout-effect
     (fn []
       (let [update-size #(set-size [(.-innerWidth js/window) (.-innerHeight js/window)])]
         (.addEventListener js/window "resize" update-size)
         (update-size)
         #(do (.removeEventListener js/window "resize" update-size))))
     [])
    size))
