(ns noon.client.ui.utils
  (:require [uix.core :as uix]
            [noon.client.constants :as constants]))

(defn with-extra-props [component extra-props]
  (let [children (.-children (.-props component))]
    #_(js/console.log children (array? children))
    (uix/$ (.-type component)
           (merge (js->clj (.-argv (.-props component)))
                  extra-props)
           children)))

(defn level->header-keyword [level]
  (case level 1 :h1 2 :h2 3 :h3 :h4))

(defn set-url-hash-no-scroll [element-id]
  (let [hash (str "#" element-id)]
    (.pushState js/history nil "" hash)))
