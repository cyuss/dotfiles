#!/usr/bin/env bash
# Lance a chaque changement de workspace par AeroSpace.
#   $1 = workspace focalise    $2 = workspace precedent
# Doit rester RAPIDE : il s'execute a chaque bascule.
set -uo pipefail

focused="${1:-}"

# SketchyBar : rafraichit l'indicateur de space (s'il tourne)
if pgrep -xq sketchybar; then
  sketchybar --trigger aerospace_workspace_change FOCUSED="$focused" 2>/dev/null \
    || sketchybar --update 2>/dev/null || true
fi

# borders : accent different selon le contexte de travail
if pgrep -xq borders; then
  case "$focused" in
    02_Coding) active="0xffe3b778" ;;   # ambre  — code
    05_I.A)    active="0xffc4a6dd" ;;   # violet — IA
    08_Ops)    active="0xffd2696b" ;;   # rouge  — prod, on fait attention
    06_Web)    active="0xff7fa5cf" ;;   # bleu   — web
    *)         active="0xff8fb884" ;;   # vert   — defaut
  esac
  borders active_color="$active" inactive_color=0xff3a404c width=5.0 2>/dev/null || true
fi
