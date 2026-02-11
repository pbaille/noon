---
type: impl
tags: [piano-roll, playground, visualization, shadow-cljs, uic, refactoring, palettes]
status: in-progress
branch: piano-roll-playground
---

# Piano Roll Playground — Implementation Journal

## What was done

### 1. Piano roll refactoring (COMMITTED on main, merged into branch)

Commit `16a280e` on `main`, then branch `piano-roll-playground` created from it.

**File**: `src/noon/viz/piano_roll.cljc`

Refactored for clean separation of concerns:
- **`compute-layout`** — single function derives all spatial values (x0, grid-w, grid-h, min/max pitch, time-scale, svg-w, svg-h)
- **Uniform `svg-*` signatures** — all take `layout` map as first arg
- **`default-opts`** — single source of truth, merged in both public fns
- **`legend`** renamed from `svg-legend` (it's HTML, not SVG)
- **Extracted HTML components**: `title-bar`, `label-bar`, `separator`, `wrap-hiccup`
- **Multi-arity** public fns instead of `& [{...}]`
- **Style constants**: `mono-font`, `sans-font` extracted

### 2. Named palettes added (COMMITTED on main)

Same commit. 8 named palettes as public data:

```clojure
(def palettes
  {:ocean {...}
   :indigo-teal {...}
   :purple-gold {...}
   :rose-cyan {...}
   :ember-ocean {...}
   :forest-berry {...}
   :sapphire-amber {...}
   :slate-coral {...}})
```

- `:palette` option accepts keyword or custom map
- `layer-labels` separated from colors
- Colors flow through layout (`:colors` key in layout map)
- No globals mutated

### 3. Harmony zone bug fix (COMMITTED on main)

`score->harmonies` was using `(dissoc (:pitch e) :position)` to group events — this included `origin.c` (chromatic offset), so `(transpose c1)` fragmented harmony zones.

**Fix**: Group by `[scale, structure, (:d origin)]` only — diatonic identity. Chromatic shifts no longer create spurious zone boundaries.

### 4. Playground (ON BRANCH, not committed yet)

Branch: `piano-roll-playground`

**New files:**
- `public/playground.html` — HTML entry with stylefy `<style>` tags
- `client/noon/client/playground.cljs` — main playground component
- `shadow-cljs.edn` — added `:playground` build target (port 8998)

**Architecture:**
- Uses `uic` (`c`/`sc` macros) for styling — consistent with existing client
- Uses `stylefy` (initialized in `init`)
- Piano roll rendered via `hiccup->html` string + `dangerouslySetInnerHTML`
- Eval via `noon.eval/eval-string-async` (SCI, async in CLJS)
- Playback via `noon.output.midi` (Web Audio / smplr)
- CodeMirror with `@nextjournal/lang-clojure` + `quietlight` theme
- `react-icons` (`vsc`, `tb`) for play/stop icons
- `react-spinners/BeatLoader` for loading state

**Current state: WORKING**
- Layout renders correctly (side-by-side: editor left, piano roll right)
- Code editing works (CodeMirror, always in edit mode)
- Eval & Play works — evaluates code, shows piano roll SVG, plays audio
- Palette switching works live (no re-eval needed)
- Example presets work (7 examples, click to load code)
- Error display works
- Stop button works

**Known issues / TODOs:**
- The `play` macro returns a playback result (with `:id`), not a score directly. Currently the playground extracts the score from the result or its metadata. The `score` macro returns a score directly. Both paths work.
- `hiccup->html` is a simple recursive converter in the playground ns — could be extracted to a util if needed elsewhere
- The editor panel takes full height but could be more refined (the existing doc code-editor has a nice editing/viewing toggle with syntax highlight — we kept it simpler here: always-editing)
- No keyboard shortcut for eval yet (Cmd+Enter would be nice)

## File inventory

```
# Modified (committed on main, in branch)
src/noon/viz/piano_roll.cljc       # refactored + palettes + harmony fix

# New (on branch, uncommitted)  
public/playground.html              # HTML entry point
client/noon/client/playground.cljs  # Playground UI component

# Modified (on branch, uncommitted)
shadow-cljs.edn                     # Added :playground build target
```

## How to run

```bash
cd /Users/pierrebaille/Code/WIP/noon
git checkout piano-roll-playground
npx shadow-cljs watch playground
# Open http://localhost:8998/playground.html
```

## Key design decisions

1. **Separate build target** (`:playground`) rather than adding to `:client` — keeps doc viewer and playground independent
2. **`hiccup->html` + innerHTML** rather than hiccup→React conversion — simpler, avoids React/UIx interop issues (we hit `React is not defined` with `createElement` approach)
3. **`uic`/`sc`/`c`** for all styling — consistent with the existing client codebase
4. **Piano roll is `.cljc`** — same namespace works in both CLJ (Clay/Kindly) and CLJS (playground)
5. **Palettes as data, not theme vars** — passed through options, no global mutation

## Dependencies used (all already in package.json)

- `@uiw/react-codemirror` + `@nextjournal/lang-clojure` — editor
- `@uiw/codemirror-themes-all` — `quietlight` theme
- `react-icons` — play/stop icons
- `react-spinners` — loading spinner
- `smplr` — Web Audio MIDI playback
- `stylefy` — CSS-in-JS (via uic)
- `pbaille/uic` — component/styling macros (`c`, `sc`)

## Possible next steps

- Keyboard shortcut (Cmd+Enter) for eval
- Auto-eval on example click
- History of evaluated scores
- Score duration / note count display
- Responsive layout (stack on mobile)
- Integrate into the main doc viewer as an optional mode
- Playhead animation on the piano roll during playback
