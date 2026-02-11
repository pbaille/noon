(ns noon.client.ui.misc
  (:require [uix.core :as uix :refer [defui]]
            [uic.component :refer [c sc]]
            [clojure.string :as str]))

(defui badge [{:keys [color size text]}]
  (sc {:text size
       :color color
       :rounded 3
       :bg {:color [color {:a 0.1}]}
       :p [1 0]}
      text))

(defui raw [{:keys [html]}]
  (c :div {:dangerouslySetInnerHTML #js {:__html html}}))

;; ── Hiccup → HTML string ────────────────────────────────────────
;; Converts Clojure hiccup data to an HTML string for use with
;; dangerouslySetInnerHTML. Used to render piano-roll SVG output.

(defn- style-map->str [m]
  (->> m
       (map (fn [[k v]] (str (name k) ":" v)))
       (str/join ";")))

(defn hiccup->html
  "Recursively render Clojure hiccup vectors to an HTML string."
  [form]
  (cond
    (nil? form)    ""
    (string? form) form
    (number? form) (str form)
    (vector? form)
    (let [[tag & more] form
          [attrs children] (if (map? (first more))
                             [(first more) (next more)]
                             [nil more])
          tag-name (name tag)
          attr-str (when attrs
                     (str (when-let [s (:style attrs)]
                            (str " style=\"" (style-map->str s) "\""))
                          (->> (dissoc attrs :style)
                               (map (fn [[k v]] (str " " (name k) "=\"" v "\"")))
                               (apply str))))]
      (str "<" tag-name attr-str ">"
           (apply str (map hiccup->html children))
           "</" tag-name ">"))
    (seq? form) (apply str (map hiccup->html form))
    :else (str form)))
