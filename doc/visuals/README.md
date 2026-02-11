# Article Visuals — Piano Roll Diagrams

Piano roll visualizations for the Civitas article (`doc/civitas-article-draft.md`).
Color-coded by harmonic layer: **tonic** (dark blue) → **structural** (medium blue) → **diatonic** (light blue) → **chromatic** (grey).

## Files

```
doc/visuals/
├── piano-roll.html          # Standalone SVG renderer (the template)
├── generate.clj             # Clojure script: evaluates article examples → JSON
├── generate-images.sh       # Shell script: full regeneration pipeline
├── README.md                # This file
└── images/                  # Generated PNGs (committed)
    ├── steps.png
    ├── scales.png
    ├── structures.png
    ├── progression.png
    ├── mixing.png
    └── harmonic-minor.png
```

## How to regenerate

### Prerequisites
- Clojure CLI (`clojure`)
- Chrome (launched with `--remote-debugging-port=9222`)
- Node.js (for browser-tools)
- Python 3 with Pillow (`pip3 install Pillow`)

### Quick regeneration

```bash
cd /path/to/noon
bash doc/visuals/generate-images.sh
```

### Manual step-by-step

1. **Start a Clojure REPL** in the noon project:
   ```bash
   clojure -M -e "(require '[noon.eval :refer [score]])" -r
   ```

2. **Generate the JSON data** from noon expressions:
   ```bash
   clojure -M doc/visuals/generate.clj
   ```
   This evaluates every article example and writes `doc/visuals/data.json`.

3. **Start Chrome with remote debugging**:
   ```bash
   # Using browser-tools:
   ~/.pi/agent/skills/pi-skills/browser-tools/browser-start.js
   # Or manually:
   /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --remote-debugging-port=9222 --no-first-run
   ```

4. **Render & screenshot** each visual:
   The `generate-images.sh` script handles this — it injects data into `piano-roll.html`,
   opens each visual in Chrome, takes a screenshot, and crops with Python/Pillow.

### Adding a new visual

1. Add the noon expression to `generate.clj` under the `images` map
2. Choose `"single"` (standalone) or add to a `"grouped"` entry
3. For grouped items, set `:sharedPitchRange true` if they should share the same vertical scale
4. Run `generate-images.sh` to regenerate

### Editing the rendering style

The renderer is `piano-roll.html` — a self-contained HTML/SVG file (~10KB, no dependencies).
Key constants to tweak:
- `COLORS` — the layer color palette
- `ROW_H` — vertical height per semitone (default: 18px)
- `adaptiveTimeScale()` — controls horizontal note density
- `HARMONY_COLOR` / `HARMONY_BORDER` — chord boundary styling
