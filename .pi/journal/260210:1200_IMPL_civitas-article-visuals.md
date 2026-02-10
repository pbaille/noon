---
type: impl
tags: [article, civitas, piano-roll, visualization]
status: active
---

# Civitas article + piano roll visuals

## What was done

### Article: "Noon — Composing Music with Clojure"

Wrote and published a Civitas article covering noon's core concepts:
- Core model (scores as sets of event maps, transformations as functions)
- Building blocks (`lin`, `par`, `tup`, `rep`, `each`, `chans`)
- The four harmonic layers (chromatic → diatonic → structural → tonic)
- Steps at each layer, context changes (scale, structure, degree)
- Mixing layers (structural arpeggios with diatonic passing tones)

**PR**: https://github.com/ClojureCivitas/clojurecivitas.github.io/pull/320
**Branch**: `noon-intro-article` on `pbaille/clojurecivitas.github.io`
**File**: `src/music/noon_introduction.clj`

### Piano roll visualization pipeline

Built a pipeline to generate piano roll diagrams from evaluated noon expressions:

1. **`doc/visuals/generate.clj`** — Clojure script that evaluates article examples via `noon.eval/score`, classifies each note by layer kind using `hc/tonic-equivalent?`, `hc/structural-equivalent?`, `hc/diatonic-equivalent?`, then emits JSON.

2. **`doc/visuals/piano-roll.html`** — Standalone HTML/SVG renderer (~10KB, no deps). Features:
   - Adaptive time scale per example
   - Grouped layouts (shared or independent pitch ranges)
   - Harmony boundary markers (dashed lines + alternating tint for chord progressions)
   - Piano keyboard on left edge
   - Color-coded layers: tonic (dark `#1a56db`) → structural (`#3b82f6`) → diatonic (`#93c5fd`) → chromatic (`#d1d5db`)

3. **`doc/visuals/generate-images.sh`** — Full pipeline: Clojure eval → Chrome render via browser-tools → Pillow crop → PNGs

4. **6 generated images** in `doc/visuals/images/`:
   - `steps.png` — chromatic/diatonic/structural/tonic side by side
   - `scales.png` — major/dorian/hungarian (shared pitch range)
   - `structures.png` — triad vs tetrad (shared pitch range)
   - `progression.png` — I-IV-V-I with harmony boundaries
   - `mixing.png` — structural + diatonic passing tones
   - `harmonic-minor.png` — harmonic minor I-IV-VII-I

### Key existing code leveraged

- `noon.harmonic-context/hc->chromatic-value` — converts pitch context to MIDI number
- `noon.harmonic-context/{tonic,structural,diatonic}-equivalent?` — classifies notes by layer
- `noon.eval/score` macro — evaluates DSL expressions via SCI without playback
- The classification logic was already in `emacs/piano_roll.clj` and `client/noon/client/xp/piano_roll.cljs`

## Next steps

### Phase 1: CLJS piano roll renderer (priority)

The current approach uses static screenshots — fragile, not interactive, requires Chrome + Pillow. The goal is a **CLJS function** that renders piano rolls as SVG, usable as:

1. **A Clay custom renderer** in the Civitas article — evaluated noon expressions render inline as interactive SVGs instead of static PNGs
2. **A reusable component** in noon's own client UI

**Plan:**
- Create a `.cljc` namespace (e.g. `noon.viz.piano-roll`) with a pure function: `(piano-roll score options) → hiccup SVG`
- The function takes a noon score, calls `score->notes` (already done), produces hiccup/SVG
- Port the rendering logic from `piano-roll.html` (JS) → CLJS
- Register as a Kindly renderer so Clay can use it directly
- Then the Civitas article can `(require '[noon.eval :refer [score]])` and inline `(score (tup s0 s1 s2))` expressions that render as piano rolls

**Key question**: how does Clay/Kindly handle custom visual renderers? Need to check `kind/hiccup` or custom `kind` registration.

### Phase 2: Article "Act 3"

The article draft has a TODO for a closing section — a small but complete musical example tying all concepts together. Something like an 8-bar piece that uses scales, degrees, structural arpeggios, and diatonic passing tones.

### Phase 3: Audio playback in Civitas

The violin article (`clojure_norway/meetup_2025_12/violin.clj`) uses `kind/audio` for WAV playback. Noon could:
- Render a score to a WAV/MP3 file
- Embed it with `kind/audio` alongside the piano roll visual
- Readers could see AND hear each example

This requires noon's MIDI→audio export (check `noon.output`).

## Files modified

```
noon/doc/civitas-article-draft.md          — markdown draft with embedded image refs
noon/doc/visuals/piano-roll.html           — SVG renderer template
noon/doc/visuals/generate.clj              — Clojure data generation script
noon/doc/visuals/generate-images.sh        — Full pipeline script
noon/doc/visuals/README.md                 — Regeneration docs
noon/doc/visuals/images/*.png              — 6 generated images

clojurecivitas.github.io/
  src/music/noon_introduction.clj          — Civitas article (Clay format)
  src/music/noon_introduction/*.png        — Images for article
```
