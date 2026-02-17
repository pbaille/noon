(ns noon.viz.clay
  "Clay/Kindly integration for embedding interactive noon editor widgets.
   Produces `kind/hiccup` output that mounts the noon code-editor component.

   Usage in a Clay notebook:
     (require '[noon.viz.clay :as nclay])
     (nclay/editor \"(play (tup s0 s1 s2))\")

   The widget JS is part of the main noon client build (shadow-cljs).")

(def ^:dynamic *widget-js-path*
  "Path to the noon client JS bundle.
   Default points to the local shadow-cljs dev server.
   Override with `binding` for production or custom setups."
  "http://localhost:8999/js/main.js")

(defn editor
  "Return a `kind/hiccup` form that renders an interactive noon editor widget.

   The widget includes:
   - A CodeMirror editor with the given source code
   - An Eval & Play button (runs via SCI in the browser)
   - A piano roll visualization
   - Audio playback via Web Audio API"
  [source]
  ^{:kindly/kind :kind/hiccup
    :kindly/hide-code true}
  [:div
   [:div {:data-noon-widget ""}
    [:pre {:class "noon-source"
           :style {:display "none"}}
     source]]
   [:script {:src *widget-js-path*}]
   [:script "noon.widget.init();"]])
