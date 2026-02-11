#!/usr/bin/env bash
set -euo pipefail

# ── Generate piano roll images for the Civitas article ──
#
# Usage:  bash doc/visuals/generate-images.sh
# Run from the noon project root.
#
# Prerequisites:
#   - clojure CLI
#   - Chrome (will be launched if not running on :9222)
#   - node (for browser-tools)
#   - python3 with Pillow

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
BROWSER_TOOLS="$HOME/.pi/agent/skills/pi-skills/browser-tools"
DATA_FILE="$SCRIPT_DIR/data.json"
HTML_FILE="$SCRIPT_DIR/piano-roll.html"
IMG_DIR="$SCRIPT_DIR/images"
TMP_DIR="/tmp/piano-roll-singles"

cd "$PROJECT_DIR"

# ── Step 1: Generate JSON data from Clojure ──
echo "═══ Step 1: Evaluating noon expressions..."
clojure -M "$SCRIPT_DIR/generate.clj"

if [ ! -f "$DATA_FILE" ]; then
  echo "ERROR: $DATA_FILE not generated"
  exit 1
fi

# ── Step 2: Create individual HTML files per visual ──
echo "═══ Step 2: Creating HTML files..."
mkdir -p "$TMP_DIR" "$IMG_DIR"

VISUALS="steps scales structures progression mixing harmonic-minor"

python3 - "$DATA_FILE" "$HTML_FILE" "$TMP_DIR" "$VISUALS" << 'PYEOF'
import json, sys, os

data_file, html_file, tmp_dir, visuals_str = sys.argv[1:5]
visuals = visuals_str.split()

data = json.load(open(data_file))
html = open(html_file).read()

for key in visuals:
    if key not in data:
        print(f"  WARNING: '{key}' not found in data, skipping")
        continue
    single = {key: data[key]}
    inject = f'<script>\nwindow.VISUAL_DATA = {json.dumps(single)};\nwindow.VISUAL_ORDER = {json.dumps([key])};\n</script>'
    out_html = html.replace('</head>', inject + '\n</head>')
    path = os.path.join(tmp_dir, f'{key}.html')
    with open(path, 'w') as f:
        f.write(out_html)
    print(f"  {key}.html")
PYEOF

# ── Step 3: Check Chrome / start if needed ──
echo "═══ Step 3: Checking Chrome..."
if ! curl -s http://localhost:9222/json > /dev/null 2>&1; then
  echo "  Starting Chrome with remote debugging..."
  "$BROWSER_TOOLS/browser-start.js" &
  sleep 3
fi

# ── Step 4: Screenshot each visual ──
echo "═══ Step 4: Taking screenshots..."
for name in $VISUALS; do
  html_path="file://${TMP_DIR}/${name}.html"

  "$BROWSER_TOOLS/browser-nav.js" "$html_path" > /dev/null 2>&1
  sleep 0.5

  SHOT=$("$BROWSER_TOOLS/browser-screenshot.js" 2>/dev/null)
  cp "$SHOT" "${IMG_DIR}/${name}.png"
  echo "  ${name}.png"
done

# ── Step 5: Crop whitespace ──
echo "═══ Step 5: Cropping images..."
python3 - "$IMG_DIR" "$VISUALS" << 'PYEOF'
from PIL import Image, ImageChops
import sys, os

img_dir = sys.argv[1]
visuals = sys.argv[2].split()

for name in visuals:
    path = os.path.join(img_dir, f'{name}.png')
    if not os.path.exists(path):
        continue
    img = Image.open(path).convert('RGB')
    bg = Image.new('RGB', img.size, (255, 255, 255))
    diff = ImageChops.difference(img, bg)
    bbox = diff.getbbox()
    if bbox:
        pad = 16
        x1, y1 = max(0, bbox[0] - pad), max(0, bbox[1] - pad)
        x2, y2 = min(img.width, bbox[2] + pad), min(img.height, bbox[3] + pad)
        cropped = img.crop((x1, y1, x2, y2))
        cropped.save(path)
        print(f"  {name}: {img.size} → {cropped.size}")
PYEOF

# ── Done ──
echo ""
echo "═══ Done! Images written to $IMG_DIR"
ls -lh "$IMG_DIR"/*.png
