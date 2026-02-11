---
type: impl
tags: [piano-roll, channel-interaction, z-index, 3-state-toggle, visualization, code-editor, playground]
status: handoff
branch: piano-roll-playground
base: 16a280e (main)
---

# Piano Roll — Channel Interaction Improvements

Handoff journal for the next session. Two features requested on the channel-mode piano roll.

## Branch state

`piano-roll-playground` — 12 commits ahead of main (`f26cd5f`).

```
f26cd5f refine: softer channel colors + unified toggle style
8d5335f feat: vibrant channel colors + refined toggle UI
5c11634 fix: segmented-control mapv arity bug crashing multi-channel piano rolls
181c457 fix: channel mode uses flat color per channel, no kind shading
1083d2b feat: dual color mode (kind/channel) + restore palettes
84c083a feat: channel-based coloring + channel filter for piano roll
26e00a1 journal: piano roll doc viewer integration notes
95ee201 fix: piano roll output layout — full-width block with horizontal scroll
72be2bd fix: SVG scaling in code-editor piano roll view
f4e33c3 fix: adaptive row height for piano rolls with wide pitch ranges
aa6ed14 feat: piano roll visualization in doc viewer code editor
34e4bad wip: playground with piano roll visualization
```

shadow-cljs: `client` build auto-watches, `playground` build does NOT auto-watch — run `npx shadow-cljs compile playground` manually after changes.

## What to implement

### 1. 3-stage channel toggle (visible → dimmed → hidden)

Currently each channel button is binary (visible/hidden). Change to a 3-state cycle:

| State | Button style | Piano roll |
|-------|-------------|------------|
| **visible** | white card + shadow + vivid dot | full opacity (0.92) |
| **dimmed** | no card, medium text + mid-opacity dot | reduced opacity (~0.25) |
| **hidden** | no card, gray text + faded dot | not rendered |

Clicking a channel cycles: visible → dimmed → hidden → visible.

**State model change** — replace `hidden` set with a channel-state map:

```clojure
;; Current (code_editor.cljs)
[hidden set-hidden] (uix/use-state #{})

;; New
[ch-states set-ch-states] (uix/use-state {})  ;; {ch -> :visible | :dimmed | :hidden}, default :visible
```

Cycle logic:
```clojure
(defn- cycle-ch-state [ch-states ch all-channels]
  (let [current (get ch-states ch :visible)
        next-state (case current :visible :dimmed, :dimmed :hidden, :hidden :visible)]
    ;; Don't allow ALL channels to be hidden
    (if (and (= next-state :hidden)
             (every? #(#{:hidden} (get (assoc ch-states ch :hidden) % :visible)) all-channels))
      (assoc ch-states ch :visible)
      (assoc ch-states ch next-state))))
```

Derive from state for piano-roll options:
```clojure
(let [visible-chs  (filterv #(= :visible (get ch-states % :visible)) all-channels)
      dimmed-chs   (filterv #(= :dimmed (get ch-states % :visible)) all-channels)
      shown-chs    (into visible-chs dimmed-chs)]  ;; pass to :channels
  ...)
```

### 2. Last-clicked channel on top (z-index)

Track which channel was last interacted with. That channel's notes render on top (last in SVG = highest z-index).

**State**:
```clojure
[focus-ch set-focus-ch] (uix/use-state nil)
```

On click: `(set-focus-ch ch)` alongside the state cycle.

### Piano roll SVG changes (`src/noon/viz/piano_roll.cljc`)

**`svg-notes` must group by channel** and render in a controllable order:

```clojure
(defn- svg-notes
  [{:keys [x0 time-scale max-pitch row-h colors]} notes
   {:keys [channel-order dimmed-channels]}]
  (let [dimmed? (set (or dimmed-channels []))
        by-ch   (group-by :channel notes)
        order   (or channel-order (sort (keys by-ch)))]
    (into [:g]
          (map (fn [ch]
                 (let [ch-notes (get by-ch ch)
                       opacity  (if (dimmed? ch) 0.25 0.92)]
                   (into [:g {:opacity opacity}]
                         (mapcat (fn [note] (render-single-note layout note)))
                         ch-notes))))
          order)))
```

**New options** for `piano-roll`:
- `:dimmed-channels` — set of channel numbers to render at reduced opacity
- `:channel-order` — vec of channel numbers, last = on top (rendered last in SVG)

**`build-roll` change**: pass the new options through to `svg-notes`.

### Files to change

| File | What |
|------|------|
| `src/noon/viz/piano_roll.cljc` | `svg-notes` grouped by channel, accept `:dimmed-channels` + `:channel-order` |
| `client/noon/client/ui/code_editor.cljs` | `piano-roll-view`: 3-state `ch-states` map, `focus-ch` tracking, pass new options to `pr/piano-roll`, update `channel-toggle` for 3 visual states |
| `client/noon/client/playground.cljs` | Same state/toggle/option changes |

### Testing

```bash
# Doc viewer (auto-reloads)
# Open http://localhost:8999/#/noon/elements/transformations-1/channels
# Click ▷ on the chans example, switch to Channel mode
# Click channel buttons — should cycle visible/dimmed/hidden
# Last clicked channel notes should be on top

# Playground (needs manual compile)
npx shadow-cljs compile playground
# Open http://localhost:8998/playground.html
# Load Polyphonic example, Eval & Play, switch to Channel
```

### UI detail for 3-state button

The `channel-toggle` component needs a `state` prop instead of `active`:

```clojure
(defui channel-toggle [{:keys [ch state color on-click]}]
  ;; state is :visible, :dimmed, or :hidden
  (c :button
     {:style {:flex [:row {:items :center :gap 0.25}]
              :p [0.25 0.5]
              :border {:width 0}
              :rounded 0.4
              :bg {:color (case state :visible "#fff", :dimmed "#fff", :hidden :transparent)}
              :color (case state :visible "#1e293b", :dimmed "#94a3b8", :hidden "#cbd5e1")
              :cursor :pointer
              :font-size "10px"
              :font-weight 500
              :font-family "'SF Mono', 'Fira Code', monospace"
              :transition "all 0.15s ease"
              :box-shadow (case state :visible "0 1px 3px rgba(0,0,0,0.08)", :dimmed "0 1px 2px rgba(0,0,0,0.04)", :hidden nil)}
      :on-click on-click}
     (c :span {:style {:display :inline-block
                        :width "7px" :height "7px"
                        :border-radius "50%"
                        :background color
                        :opacity (case state :visible 1, :dimmed 0.4, :hidden 0.15)}})
     (str "ch " ch)))
```

## Previous session context

- Fixed critical `segmented-control` mapv arity bug that crashed multi-channel scores in doc viewer
- Replaced generic HSL channel colors with hand-picked Tailwind 400-range pastels
- Unified toggle style: pill container with white card active items + colored dots
- All changes on branch `piano-roll-playground`, not merged to main
- Previous journals: `260211:1245_IMPL_piano-roll-doc-viewer-integration.md`, `260211:1035_IMPL_piano-roll-playground.md`
