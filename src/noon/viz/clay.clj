(ns noon.viz.clay
  "Clay/Kindly integration for embedding interactive noon editor widgets.
   Produces `kind/hiccup` output that mounts the noon code-editor component.

   Usage in a Clay notebook:
     (require '[noon.viz.clay :as nclay])
     (nclay/editor \"(play (tup s0 s1 s2))\")

   The widget JS is served from noon's GitHub Pages (built via shadow-cljs :widget).
   Override `*widget-js-url*` for local dev or custom deployments.")

(def ^:dynamic *widget-js-url*
  "URL to the noon widget JS bundle.
   Default: production URL on GitHub Pages.
   Override with `binding` for local dev:
     (binding [*widget-js-url* \"http://localhost:8997/js/widget/noon-widget.js\"]
       (editor \"(play (tup s0 s1 s2))\"))"
  "https://pbaille.github.io/noon/js/widget/noon-widget.js")

(defn editor
  "Return a `kind/hiccup` form that renders an interactive noon editor widget.

   The widget includes:
   - A CodeMirror editor with the given source code
   - An Eval & Play button (runs via SCI in the browser)
   - A piano roll visualization
   - Audio playback via Web Audio API

   Options:
   - :show-piano-roll? — show piano roll on eval (default: true)
   - :theme            — :auto (default), :light, or :dark
                         :auto uses prefers-color-scheme media query"
  ([source] (editor source {}))
  ([source {:keys [show-piano-roll? theme] :or {show-piano-roll? true theme :auto}}]
   (let [js-url *widget-js-url*]
     ^{:kindly/kind :kind/hiccup
       :kindly/hide-code true}
     [:div
      [:div {:data-noon-widget ""
             :data-noon-options (pr-str {:show-piano-roll? show-piano-roll?
                                         :theme theme})}
       [:pre {:class "noon-source"
              :style {:display "none"}}
        source]]
      ;; Load the widget JS once, then mount all widgets.
      ;; The script loader is idempotent — subsequent calls just re-scan for new widgets.
      [:script
       (str "
(function() {
  if (typeof noon !== 'undefined' && noon.widget && noon.widget.init) {
    noon.widget.init();
  } else if (!document.querySelector('script[data-noon-widget-js]')) {
    var s = document.createElement('script');
    s.src = '" js-url "';
    s.setAttribute('data-noon-widget-js', 'true');
    s.onload = function() { noon.widget.init(); };
    document.head.appendChild(s);
  } else {
    // Script is loading, set up a poll to init once ready
    var check = setInterval(function() {
      if (typeof noon !== 'undefined' && noon.widget && noon.widget.init) {
        clearInterval(check);
        noon.widget.init();
      }
    }, 100);
  }
})();
")]])))
