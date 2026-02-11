---
type: impl
tags: [piano-roll, doc-viewer, code-editor, visualization, uic, adaptive-layout, hiccup, playground]
status: in-progress
branch: piano-roll-playground
base: 16a280e (main)
---

# Piano Roll — Doc Viewer Integration & Adaptive Sizing

Extends the previous piano-roll refactoring (on `main` at `16a280e`) with browser integration: inline piano roll visualization in the existing doc viewer's code editor, adaptive SVG sizing, and a standalone playground.

## Branch: `piano-roll-playground` (5 commits, not merged)

```
95ee201 fix: piano roll output layout — full-width block with horizontal scroll
72be2bd fix: SVG scaling in code-editor piano roll view
f4e33c3 fix: adaptive row height for piano rolls with wide pitch ranges
aa6ed14 feat: piano roll visualization in doc viewer code editor
34e4bad wip: playground with piano roll visualization
```

## What was done

### 1. Doc viewer integration (`code_editor.cljs`)

When a code block evaluates to a score (via `play` or `score`), the output panel now shows an inline piano roll SVG instead of text.

**Score detection**: `result->score` extracts scores from:
- `play` results — score lives in `(meta result :score)` (play returns a files map with metadata)
- `score` results — the value IS the score directly (`score/score?` check)

**Output panel restructured** for two distinct layouts:
- **Piano roll**: full-width block, close button overlaid top-right (`position: absolute`), horizontal scroll via `overflow-x: auto`. Inner div uses `width: max-content` + `min-width: 100%` so short scores fill the container while long scores scroll.
- **Text output** (errors, non-score results): unchanged side-by-side layout (close button | content).

The previous flex-row layout (`close-button | content`) clipped wide piano rolls because flex items have `min-width: auto` by default — the SVG couldn't shrink and no scrollbar appeared.

### 2. Shared `hiccup->html` utility (`ui/misc.cljs`)

Converts Clojure hiccup vectors to HTML strings for `dangerouslySetInnerHTML`. Handles:
- Nested vectors, seqs, strings, numbers, nil
- Attribute maps with `:style` (converted to inline CSS string)
- SVG-compatible (attribute names pass through as-is)

Used by both the doc viewer's `piano-roll-view` and the standalone playground.

### 3. Adaptive row height (`piano_roll.cljc`)

Fixed large blank vertical zones for polyphonic scores with wide pitch ranges.

**Root cause**: Fixed `row-h = 18px` produced tall grids (e.g., 34 rows × 18px = 612px for a bass+treble score spanning MIDI 36–67).

**Fix**: `row-h` is now adaptive in `compute-layout`:
```clojure
(def ^:private max-row-h 18)
(def ^:private min-row-h 8)
(def ^:private target-grid-h 350.0)  ;; .0 to avoid Clojure ratios in SVG attrs

row-h = clamp(min-row-h, max-row-h, target-grid-h / n-rows)
```

- ≤19 semitones: unchanged (18px rows)
- 20–43 semitones: proportionally shrinks
- ≥44 semitones: capped at 8px minimum

Font sizes and note padding scale proportionally with `row-h`.

### 4. SVG scaling fix (`code_editor.cljs` CSS)

The SVG's `preserveAspectRatio="xMidYMid meet"` was scaling a 2445px-wide SVG down to the ~640px container, centering content vertically with blank bands. Fixed by setting `width: max-content` on the inner wrapper div, letting the SVG render at native pixel dimensions while the outer container handles horizontal scroll.

### 5. Standalone playground (`playground.cljs`)

Separate shadow-cljs build target (`:playground`, port 8998) with:
- CodeMirror editor with Clojure syntax highlighting
- 7 preset examples
- 8 palette selector buttons (live switching, no re-eval)
- Eval & Play / Stop controls
- Error display
- Uses `uic` (`c`/`sc`) for styling — consistent with existing client

## Files changed (on branch vs main)

```
# New
client/noon/client/playground.cljs       — standalone playground UI
public/playground.html                    — playground entry point

# Modified  
client/noon/client/ui/code_editor.cljs   — piano roll in output panel
client/noon/client/ui/misc.cljs          — hiccup->html utility
shadow-cljs.edn                          — :playground build target
src/noon/viz/piano_roll.cljc             — adaptive row-h, target-grid-h
```

## How to test

```bash
git checkout piano-roll-playground

# Doc viewer with piano roll integration
npx shadow-cljs watch client
# Open http://localhost:8999, click ▶ on any play/score block

# Standalone playground  
npx shadow-cljs watch playground
# Open http://localhost:8998/playground.html
```

## Key design decisions

1. **Full-width block layout for piano roll output** — not the flex-row used for text output. Avoids `min-width: auto` clipping, allows proper horizontal scroll.
2. **Overlaid close button** — `position: absolute` top-right with semi-transparent background. Stays visible while scrolling.
3. **`hiccup->html` + innerHTML** — simpler than hiccup→React conversion, avoids React/UIx interop issues (we hit `React is not defined` and attribute name mismatches with the `createElement` approach).
4. **`target-grid-h = 350.0`** (float, not int) — avoids Clojure ratio serialization (`175/17`) in SVG attributes.
5. **Separate build target** for playground — doesn't affect the doc viewer build.
6. **Score detection at eval time** — stored in separate `score*` state, independent of the `return` state used for text output.

## What's left

- [ ] Keyboard shortcut (Cmd+Enter) for eval in playground
- [ ] Auto-eval on example click
- [ ] Consider removing `viewBox` from SVG to prevent scaling in other contexts
- [ ] Edge case: extremely wide pitch ranges (>43 semitones) hit 8px minimum
- [ ] Clean up: `doc/civitas-article-draft.md`, `doc/visuals/`, `src/noon/tries/scratch2.clj` were included in the branch but are unrelated — should be separated or removed before merging
