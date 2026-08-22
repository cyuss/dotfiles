#!/usr/bin/env bash

set -euo pipefail

AEROSPACE_BIN="${AEROSPACE_BIN:-/opt/homebrew/bin/aerospace}"
if [[ ! -x "$AEROSPACE_BIN" ]]; then
  AEROSPACE_BIN="$(command -v aerospace || true)"
fi
if [[ -z "$AEROSPACE_BIN" ]]; then
  exit 0
fi

CONFIG="${HOME}/.config/aerospace/aerospace.toml"
if [[ ! -f "$CONFIG" ]]; then
  exit 0
fi

NO_MARGIN_FLAG="${HOME}/.config/aerospace/.no_margin_mode"
if [[ -f "${NO_MARGIN_FLAG}" ]]; then
  target_top=0
else
  # Compute top gap from SketchyBar geometry, then add a small safety buffer.
  SKETCHY_RC="${HOME}/.config/sketchybar/sketchybarrc"
  bar_height=45
  bar_margin=8
  bar_y_offset=0

  if [[ -f "$SKETCHY_RC" ]]; then
    parsed_height="$(sed -nE 's/^[[:space:]]*height=([0-9]+).*$/\1/p' "$SKETCHY_RC" | head -n 1)"
    parsed_margin="$(sed -nE 's/^[[:space:]]*margin=([0-9]+).*$/\1/p' "$SKETCHY_RC" | head -n 1)"
    parsed_y_offset="$(sed -nE 's/^[[:space:]]*y_offset=(-?[0-9]+).*$/\1/p' "$SKETCHY_RC" | head -n 1)"

    if [[ "$parsed_height" =~ ^[0-9]+$ ]]; then
      bar_height="$parsed_height"
    fi
    if [[ "$parsed_margin" =~ ^[0-9]+$ ]]; then
      bar_margin="$parsed_margin"
    fi
    if [[ "$parsed_y_offset" =~ ^-?[0-9]+$ ]]; then
      bar_y_offset="$parsed_y_offset"
    fi
  fi

  extra_top_buffer=6
  positive_y_offset=0
  if (( bar_y_offset > 0 )); then
    positive_y_offset="$bar_y_offset"
  fi

  target_top=$((bar_height + bar_margin + positive_y_offset + extra_top_buffer))
fi

current_top="$(sed -nE 's/^[[:space:]]*outer\.top[[:space:]]*=[[:space:]]*([0-9]+)[[:space:]]*$/\1/p' "$CONFIG" | head -n 1)"
if [[ "$current_top" == "$target_top" ]]; then
  exit 0
fi

tmp="$(mktemp)"
sed -E "s/^([[:space:]]*outer\.top[[:space:]]*=[[:space:]]*)[0-9]+([[:space:]]*)$/\1${target_top}\2/" "$CONFIG" > "$tmp"
mv "$tmp" "$CONFIG"

"$AEROSPACE_BIN" reload-config >/dev/null 2>&1 || true
