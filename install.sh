#!/usr/bin/env bash
# ══════════════════════════════════════════════════════════════════════
#  install.sh — relie les dotfiles de $HOME vers ce depot.
#
#  Idempotent : relancer ne casse rien. Tout fichier existant qui n'est
#  pas deja le bon lien est sauvegarde en <nom>.backup-<horodatage>.
#  Aucune installation de paquet, aucune modification hors de $HOME.
# ══════════════════════════════════════════════════════════════════════
set -euo pipefail

CONFIG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
STAMP="$(date +%Y%m%d-%H%M%S)"

# cible_dans_le_depot -> nom dans $HOME
LINKS=(
  "zsh/.zshrc:.zshrc"
  "zsh/.zshenv:.zshenv"
  "zsh/.zprofile:.zprofile"
  "zsh/.zsh_plugins.txt:.zsh_plugins.txt"
  "zsh/.zsh_plugins_fpath.txt:.zsh_plugins_fpath.txt"
  "git/gitconfig:.gitconfig"
  "git/gitignore_global:.gitignore_global"
)

link() {
  local src="$CONFIG_DIR/$1" dst="$HOME/$2"

  if [[ ! -e $src ]]; then
    printf '  \033[33mabsent\033[0m   %s (ignore)\n' "$1"
    return
  fi
  if [[ -L $dst && "$(readlink "$dst")" == "$src" ]]; then
    printf '  \033[2mdeja ok\033[0m  ~/%s\n' "$2"
    return
  fi
  if [[ -e $dst || -L $dst ]]; then
    mv "$dst" "$dst.backup-$STAMP"
    printf '  \033[33msauve\033[0m    ~/%s -> ~/%s.backup-%s\n' "$2" "$2" "$STAMP"
  fi
  ln -s "$src" "$dst"
  printf '  \033[32mlie\033[0m      ~/%s -> %s\n' "$2" "$1"
}

printf '\n  \033[1mdotfiles\033[0m — depot : %s\n\n' "$CONFIG_DIR"
for entry in "${LINKS[@]}"; do
  link "${entry%%:*}" "${entry#*:}"
done

# ── Identite Doom : non versionnee ──────────────────────────────────
if [[ -f "$CONFIG_DIR/doom/private.el.example" && ! -f "$CONFIG_DIR/doom/private.el" ]]; then
  printf '\n  \033[33mA faire\033[0m  cp doom/private.el.example doom/private.el\n'
  printf '           puis y mettre ton nom et ton e-mail.\n'
fi

printf '\n  Termine. Ouvre un nouveau shell pour prendre en compte les changements.\n\n'
