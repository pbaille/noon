(ns noon.viz.clay
  "Clay/Kindly integration for embedding interactive noon editor widgets.
   Produces `kind/hiccup` output with external JS dependencies.

   Usage in a Clay notebook:
     (require '[noon.viz.clay :as clay])

     ;; Single interactive editor
     (clay/noon-editor \"(play (tup s0 s1 s2))\")

     ;; With label
     (clay/noon-editor \"(score (tup c0 c1 c2))\"
                       {:label \"Chromatic steps\"})

     ;; Group of labeled editors (stacked)
     (clay/noon-editor-group
       [[\"chromatic\" \"(score (tup c0 c1 c2 c3 c4 c5 c6))\"]
        [\"diatonic\"  \"(score (tup d0 d1 d2 d3 d4 d5 d6))\"]])

   The widget JS bundle is loaded from the noon GitHub Pages CDN."
  (:require [clojure.string :as str]))

;; ── Configuration ───────────────────────────────────────────────

(def ^:dynamic *widget-base-url*
  "Base URL for the noon widget JS bundle.
   Override with `binding` for local development."
  "https://pbaille.github.io/noon/js/widget")

;; ── Helpers ─────────────────────────────────────────────────────

(defn- html-escape [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

;; ── Public API ──────────────────────────────────────────────────

(defn noon-editor
  "Return a `kind/hiccup` form that renders an interactive noon editor widget.

   The widget includes:
   - A CodeMirror editor with the given source code
   - An Eval & Play button (runs via SCI in the browser)
   - A piano roll visualization
   - Audio playback via Web Audio API

   Options:
     :label   - optional label displayed above the editor

   The widget auto-evaluates on load to show the piano roll immediately."
  ([source] (noon-editor source nil))
  ([source {:keys [label]}]
   ^{:kindly/kind :kind/hiccup
     :kindly/hide-code true}
   [:div
    [:div (cond-> {:data-noon-widget ""}
            label (assoc :data-label label))
     [:pre {:class "noon-source"
            :style {:display "none"}}
      source]]
    [:script {:src (str *widget-base-url* "/noon-widget.js")}]]))

(defn noon-editor-group
  "Return a `kind/hiccup` form that renders a group of stacked noon editor widgets.

   entries is a vector of [label source-code] pairs:
     [[\"major\"   \"(score (tup d0 d1 d2 d3 d4 d5 d6 d7))\"]
      [\"dorian\"  \"(score (scale :dorian) (tup d0 d1 d2 d3 d4 d5 d6 d7))\"]]

   Each entry gets its own editor, eval button, and piano roll."
  [entries]
  (let [entries-json (str "["
                          (->> entries
                               (map (fn [[label source]]
                                      (str "{\"label\":" (pr-str label)
                                           ",\"source\":" (pr-str source) "}")))
                               (str/join ","))
                          "]")]
    ^{:kindly/kind :kind/hiccup
      :kindly/hide-code true}
    [:div
     [:div {:data-noon-widget-group ""
            :data-entries entries-json}]
     [:script {:src (str *widget-base-url* "/noon-widget.js")}]]))
