---
type: impl
tags: [piano-roll, visualization, hiccup, svg]
status: active
---

# noon.viz.piano-roll namespace + article integration

## What was done

### Phase 1 complete: `noon.viz.piano-roll`

Created a pure `.cljc` namespace that renders noon scores as piano roll SVGs in hiccup format. This replaces the fragile static screenshot pipeline (Clojure → JSON → Chrome → Pillow → PNG) with a single function call.

**Namespace**: `noon.viz.piano-roll`
**Commit**: `e63bb7c` (noon)

#### Public API

```clojure
(require '[noon.viz.piano-roll :as pr])
(require '[noon.eval :refer [score]])

;; Single score
(pr/piano-roll (score (lin I IV V I) (each (tup s0 s1 s2)))
               {:title "I-IV-V-I"})

;; Multiple scores stacked
(pr/piano-roll-group
  [{:label "major"  :score (score (tup d0 d1 d2 d3 d4 d5 d6 d7))}
   {:label "dorian" :score (score (scale :dorian) (tup d0 d1 d2 d3 d4 d5 d6 d7))}]
  {:shared-pitch-range true})
```

#### Features
- **Cross-platform** — works in both CLJ and CLJS
- **Kindly-ready** — output has `^{:kindly/kind :kind/hiccup}` metadata, renders inline in Clay
- **Layer classification** — notes colored by harmonic layer (tonic/structural/diatonic/chromatic)
- **Harmony boundaries** — dashed lines + alternating tint for chord progressions
- **Piano keyboard** — left edge with note labels at octave boundaries
- **Adaptive layout** — auto-scales time axis and pitch range
- **Consolidates** `score->notes` and `score->harmonies` from the 3 previous copies (emacs, client, generate.clj)

#### Options (both functions)
- `:target-width` — pixel width for time axis (default 500)
- `:show-keyboard` — piano keys on left (default true)
- `:show-harmonies` — harmony boundary markers (default true)
- `:show-legend` — color legend (default true)
- `:padding` — pitch range padding in semitones (default 1)
- `:title` — title string (`piano-roll` only)
- `:shared-pitch-range` — shared vertical range (`piano-roll-group` only)

### Civitas article updated

Replaced the 6 static PNG images with live `pr/piano-roll` / `pr/piano-roll-group` calls in the article. Clay evaluates the noon expressions and renders SVG piano rolls inline.

**Commit**: `efba240` (clojurecivitas.github.io, branch `noon-intro-article`)

Changes:
- Added noon as git dep in `deps.edn`
- Replaced `![...](noon_introduction/*.png)` markdown refs with `^:kindly/hide-code` evaluated expressions
- Deleted 6 static PNGs from `src/music/noon_introduction/`
- Image captions converted to italic paragraphs

### How porting worked

The JS renderer in `doc/visuals/piano-roll.html` (~200 lines) was ported to Clojure hiccup:
- `buildRoll()` → `build-roll` + helper fns (`svg-grid-rows`, `svg-harmonies`, `svg-keyboard`, `svg-notes`)
- `renderSingle()` → `piano-roll`
- `renderGrouped()` → `piano-roll-group`
- `buildLegend()` → `svg-legend`
- `Math.ceil` → reader conditional `#?(:clj Math/ceil :cljs js/Math.ceil)`
- `.indexOf` on vectors → `layer-order` map for cross-platform sort

## Files created/modified

```
noon:
  src/noon/viz/piano_roll.cljc       — NEW: the namespace (358 lines)
  test/noon/viz/piano_roll_test.cljc — NEW: 5 tests, 20 assertions

clojurecivitas.github.io:
  deps.edn                           — added noon git dep
  src/music/noon_introduction.clj    — live piano rolls instead of PNGs
  src/music/noon_introduction/*.png  — DELETED (6 files)
```

## Next steps

### Phase 2: Article "Act 3"
A closing section with a small but complete musical example tying all concepts together (scales, degrees, structural arpeggios, diatonic passing tones).

### Phase 3: Audio playback
Use `kind/audio` to embed WAV playback alongside piano rolls — readers could see AND hear each example.

### Other ideas
- Use `noon.viz.piano-roll` in noon's own client UI (replace the UIx `piano_roll.cljs`)
- Add hover tooltips (note name, MIDI number, layer kind) — would need JS/CLJS interop
- Consider making the old `doc/visuals/` pipeline obsolete (keep for reference?)
