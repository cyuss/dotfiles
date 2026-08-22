#!/usr/bin/env bash

set -euo pipefail

AEROSPACE_BIN="${AEROSPACE_BIN:-/opt/homebrew/bin/aerospace}"
if [[ ! -x "$AEROSPACE_BIN" ]]; then
  AEROSPACE_BIN="$(command -v aerospace || true)"
fi
if [[ -z "$AEROSPACE_BIN" ]]; then
  exit 1
fi

log() {
  if [[ "${RESIZE_THIRDS_DEBUG:-0}" == "1" ]]; then
    printf '[resize_thirds] %s\n' "$*" >> /tmp/resize_thirds.log
  fi
}

mode="${1:-}"
if [[ "$mode" != "one-third" && "$mode" != "two-thirds" ]]; then
  echo "usage: $0 one-third|two-thirds" >&2
  exit 2
fi

current_width=""

# First try direct width format (if supported by current AeroSpace version).
current_width="$("$AEROSPACE_BIN" list-windows --focused --format '%{window-width}' 2>/dev/null || true)"
if [[ ! "$current_width" =~ ^[0-9]+$ ]] || (( current_width <= 0 )); then
  current_width=""
fi

# Fallback: derive width from left/right bounds.
if [[ -z "$current_width" ]]; then
  read -r left right < <("$AEROSPACE_BIN" list-windows --focused --format '%{window-left} %{window-right}' 2>/dev/null || true)
  if [[ "${left:-}" =~ ^-?[0-9]+$ ]] && [[ "${right:-}" =~ ^-?[0-9]+$ ]]; then
    w=$(( right - left ))
    if (( w > 0 )); then
      current_width="$w"
    fi
  fi
fi

# Last fallback: JSON output with flexible key lookup.
if [[ -z "$current_width" ]]; then
  json="$("$AEROSPACE_BIN" list-windows --focused --json 2>/dev/null || true)"
  current_width="$(JSON_INPUT="$json" /usr/bin/python3 - <<'PY'
import json
import os
raw = os.environ.get("JSON_INPUT", "").strip()
if not raw:
    print("")
    raise SystemExit(0)
try:
    data = json.loads(raw)
except Exception:
    print("")
    raise SystemExit(0)

node = data[0] if isinstance(data, list) and data else data
if not isinstance(node, dict):
    print("")
    raise SystemExit(0)

def get_path(d, path):
    cur = d
    for p in path:
        if not isinstance(cur, dict) or p not in cur:
            return None
        cur = cur[p]
    return cur

candidates = [
    ("window-width",),
    ("windowWidth",),
    ("frame", "w"),
    ("frame", "width"),
    ("bounds", "w"),
    ("bounds", "width"),
]
for path in candidates:
    v = get_path(node, path)
    if isinstance(v, (int, float)) and v > 0:
        print(int(v))
        raise SystemExit(0)
print("")
PY
)"
fi

if [[ ! "$current_width" =~ ^[0-9]+$ ]] || (( current_width <= 0 )); then
  log "failed to read current width"
  exit 1
fi

# We call this script right after "balance-sizes", so current_width ~= 1/2.
# Target:
# - one-third: 1/3 of split => 2/3 of current half width
# - two-thirds: 2/3 of split => 4/3 of current half width
if [[ "$mode" == "one-third" ]]; then
  target_width=$(( (current_width * 2) / 3 ))
else
  target_width=$(( (current_width * 4) / 3 ))
fi

if (( target_width < 120 )); then
  target_width=120
fi

delta=$(( target_width - current_width ))
if (( delta == 0 )); then
  log "delta is zero, nothing to resize"
  exit 0
fi

log "mode=$mode current_width=$current_width target_width=$target_width delta=$delta"
if (( delta > 0 )); then
  "$AEROSPACE_BIN" resize width "+$delta"
else
  "$AEROSPACE_BIN" resize width "$delta"
fi
