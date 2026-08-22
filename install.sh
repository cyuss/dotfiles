#!/usr/bin/env bash
# ══════════════════════════════════════════════════════════════════════
#  install.sh — installe cette configuration sur une machine macOS.
#
#  Trois choses separees, qu'on peut faire independamment :
#
#    1. LIENS     relier ~/.zshrc, ~/.gitconfig… vers ce depot
#    2. PAQUETS   installer les outils, par groupes (brew bundle)
#    3. SUITE     les etapes qui ne s'automatisent pas bien (doom sync,
#                 identite, LaunchAgent) — annoncees, jamais faites
#                 dans ton dos
#
#  Rien n'est detruit : tout fichier existant est sauvegarde avant
#  d'etre remplace, et --dry-run montre l'integralite du plan.
# ══════════════════════════════════════════════════════════════════════
set -uo pipefail

CONFIG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BREW_DIR="$CONFIG_DIR/install/brew"
GROUPS_CONF="$CONFIG_DIR/install/groups.conf"
STAMP="$(date +%Y%m%d-%H%M%S)"

DO_LINKS=1 DO_PACKAGES=0 DRY=0 ASSUME_YES=0 GROUPS_ARG="" MODE=interactive

# ── Presentation ─────────────────────────────────────────────────────
if [[ -t 1 ]]; then
  B=$'\033[1m'; D=$'\033[2m'; G=$'\033[32m'; Y=$'\033[33m'
  R=$'\033[31m'; C=$'\033[36m'; X=$'\033[0m'
else
  B= D= G= Y= R= C= X=
fi
say()  { printf '%s\n' "$*"; }
step() { printf '\n%s▸ %s%s\n' "$B" "$*" "$X"; }
ok()   { printf '  %s✓%s %s\n' "$G" "$X" "$*"; }
warn() { printf '  %s!%s %s\n' "$Y" "$X" "$*"; }
err()  { printf '  %s✗%s %s\n' "$R" "$X" "$*" >&2; }
note() { printf '  %s%s%s\n' "$D" "$*" "$X"; }
run()  { if [[ $DRY -eq 1 ]]; then printf '  %s$ %s%s\n' "$D" "$*" "$X"; else eval "$@"; fi; }

usage() {
  cat <<USAGE

  ${B}install.sh${X} — configuration macOS

  ${B}Usage courant${X}
    ./install.sh                 liens + choix des groupes (interactif)
    ./install.sh --links         liens seulement, aucun paquet
    ./install.sh --all           liens + TOUS les groupes
    ./install.sh --recommended   liens + les groupes marques par defaut
    ./install.sh --groups a,b    liens + ces groupes precis

  ${B}Inspection${X}
    ./install.sh --list          liste les groupes et leur contenu
    ./install.sh --check         diagnostic : ce qui manque, sans rien installer
    ./install.sh --dry-run ...   montre le plan complet sans agir

  ${B}Options${X}
    --no-links                   sauter les liens (paquets seulement)
    --yes, -y                    ne pas demander confirmation
    --help, -h                   cette aide

  ${B}Exemples${X}
    ${D}# Je regarde ce depot, je ne veux rien casser${X}
    ./install.sh --dry-run --all

    ${D}# Je veux juste ma config zsh sur un serveur${X}
    ./install.sh --links

    ${D}# Machine neuve, je veux tout${X}
    ./install.sh --all --yes

    ${D}# J'ai deja mes outils, je veux ajouter les TUIs${X}
    ./install.sh --no-links --groups tui,data

USAGE
}

# ── Groupes ──────────────────────────────────────────────────────────
all_groups()  { awk -F'|' '!/^#/ && NF {print $1}' "$GROUPS_CONF"; }
def_groups()  { awk -F'|' '!/^#/ && NF && $2==1 {print $1}' "$GROUPS_CONF"; }
group_desc()  { awk -F'|' -v g="$1" '!/^#/ && $1==g {print $3}' "$GROUPS_CONF"; }
group_file()  { echo "$BREW_DIR/$1.Brewfile"; }

group_exists() { [[ -f "$(group_file "$1")" ]]; }

cmd_list() {
  printf '\n  %sGroupes disponibles%s  %s(defaut = preselectionne)%s\n\n' "$B" "$X" "$D" "$X"
  while IFS='|' read -r name def desc; do
    [[ -z ${name:-} || $name == \#* ]] && continue
    local mark="   "; [[ $def == 1 ]] && mark=" ${G}*${X} "
    local n; n=$(grep -cE '^(brew|cask) ' "$(group_file "$name")" 2>/dev/null || echo 0)
    printf '%s%s%-9s%s %s%2d paquets%s  %s\n' "$mark" "$C" "$name" "$X" "$D" "$n" "$X" "$desc"
  done < "$GROUPS_CONF"
  printf '\n  %sDetail d'"'"'un groupe :%s  cat install/brew/<groupe>.Brewfile\n\n' "$D" "$X"
}

# ── Diagnostic ───────────────────────────────────────────────────────
cmd_check() {
  printf '\n  %sDiagnostic%s\n' "$B" "$X"

  step "Prerequis"
  command -v brew >/dev/null 2>&1 && ok "Homebrew $(brew --version | head -1 | cut -d' ' -f2)" \
                                  || warn "Homebrew absent — https://brew.sh"
  [[ $(uname) == Darwin ]] && ok "macOS $(sw_vers -productVersion 2>/dev/null)" \
                           || warn "systeme non-macOS : les groupes wm/term/fonts ne s'appliquent pas"

  step "Liens"
  local n_ok=0 n_ko=0
  while IFS=: read -r src dst; do
    [[ -z ${src:-} ]] && continue
    if [[ -L "$HOME/$dst" && "$(readlink "$HOME/$dst")" == "$CONFIG_DIR/$src" ]]; then
      n_ok=$((n_ok+1))
    else
      n_ko=$((n_ko+1)); warn "~/$dst n'est pas lie a ce depot"
    fi
  done <<< "$(links_table)"
  [[ $n_ko -eq 0 ]] && ok "$n_ok liens en place"

  step "Paquets, par groupe"
  local g miss elsewhere total
  for g in $(all_groups); do
    miss=""; elsewhere=""; total=0
    while read -r kind pkg; do
      [[ -z ${pkg:-} ]] && continue
      total=$((total+1))
      local short="${pkg##*/}"
      case "$(presence "$kind" "$short")" in
        brew)  : ;;
        other) elsewhere="$elsewhere $short" ;;
        *)     miss="$miss $short" ;;
      esac
    done <<< "$(parse_brewfile "$g")"
    if [[ -z $miss && -z $elsewhere ]]; then
      ok "$(printf '%-9s' "$g") complet ($total)"
    elif [[ -z $miss ]]; then
      ok "$(printf '%-9s' "$g") complet ($total)$D — hors brew :$elsewhere$X"
    else
      warn "$(printf '%-9s' "$g") manque :$miss${elsewhere:+$D — hors brew :$elsewhere$X}"
    fi
  done

  step "Suite"
  [[ -f "$CONFIG_DIR/doom/private.el" ]] && ok "doom/private.el present" \
     || warn "doom/private.el absent — cp doom/private.el.example doom/private.el"
  printf '\n'
}

# Un paquet peut etre la sans venir de brew : une .app telechargee, un
# binaire pose a la main, une police copiee dans ~/Library/Fonts. Le
# declarer « manquant » serait faux et pousserait a une reinstallation
# en double. On distingue donc trois etats : brew / other / absent.
presence() {
  local kind="$1" pkg="$2"
  if [[ $kind == cask ]]; then
    brew list --cask "$pkg" >/dev/null 2>&1 && { echo brew; return; }
  else
    brew list --formula "$pkg" >/dev/null 2>&1 && { echo brew; return; }
    command -v "$pkg" >/dev/null 2>&1 && { echo other; return; }
  fi

  # Polices : cherchees par leur nom de famille dans les dossiers systeme.
  if [[ $pkg == font-* ]]; then
    local fam="${pkg#font-}"; fam="${fam%-nerd-font}"; fam="${fam//-/ }"
    if ls ~/Library/Fonts /Library/Fonts 2>/dev/null \
       | grep -qi "$(echo "$fam" | tr -d ' ' | cut -c1-8)"; then
      echo other; return
    fi
  fi

  # Casks : une application du meme nom, installee autrement.
  local app
  for app in "/Applications" "$HOME/Applications"; do
    [[ -d $app ]] || continue
    if ls "$app" 2>/dev/null | grep -qi "^${pkg//-/[- ]}\.app$"; then
      echo other; return
    fi
  done
  # Karabiner s'installe sous un nom different de son cask.
  case "$pkg" in
    karabiner-elements) [[ -d /Applications/Karabiner-Elements.app ]] && { echo other; return; } ;;
  esac
  echo absent
}

parse_brewfile() {
  local f; f="$(group_file "$1")"
  [[ -f $f ]] || return 0
  sed -nE 's/^(brew|cask)[[:space:]]+"([^"]+)".*/\1 \2/p' "$f"
}

# ── Liens ────────────────────────────────────────────────────────────
links_table() {
  cat <<'TABLE'
zsh/.zshrc:.zshrc
zsh/.zshenv:.zshenv
zsh/.zprofile:.zprofile
zsh/.zsh_plugins.txt:.zsh_plugins.txt
zsh/.zsh_plugins_fpath.txt:.zsh_plugins_fpath.txt
git/gitconfig:.gitconfig
git/gitignore_global:.gitignore_global
TABLE
}

do_link() {
  local src="$CONFIG_DIR/$1" dst="$HOME/$2"
  if [[ ! -e $src ]]; then warn "$1 absent du depot — ignore"; return; fi
  if [[ -L $dst && "$(readlink "$dst")" == "$src" ]]; then note "deja lie   ~/$2"; return; fi
  if [[ -e $dst || -L $dst ]]; then
    run "mv '$dst' '$dst.backup-$STAMP'"
    warn "sauve      ~/$2 -> ~/$2.backup-$STAMP"
  fi
  run "ln -s '$src' '$dst'"
  ok "lie        ~/$2"
}

cmd_links() {
  step "Liens symboliques"
  while IFS=: read -r src dst; do
    [[ -z ${src:-} ]] && continue
    do_link "$src" "$dst"
  done <<< "$(links_table)"
}

# ── Paquets ──────────────────────────────────────────────────────────
install_group() {
  local g="$1" f; f="$(group_file "$g")"
  if [[ ! -f $f ]]; then err "groupe inconnu : $g"; return 1; fi
  printf '\n  %s%s%s  %s%s%s\n' "$C" "$g" "$X" "$D" "$(group_desc "$g")" "$X"
  run "brew bundle --file='$f' --no-upgrade"
}

# ── Selection interactive ────────────────────────────────────────────
select_groups() {
  local avail; avail=$(all_groups)
  local defaults; defaults=$(def_groups | tr '\n' ',')

  if command -v gum >/dev/null 2>&1; then
    local lines=() g
    for g in $avail; do lines+=("$(printf '%-9s %s' "$g" "$(group_desc "$g")")"); done
    local sel
    sel=$(printf '%s\n' "${lines[@]}" | gum choose --no-limit \
            --header="Groupes a installer — espace pour cocher, entree pour valider" \
            --selected="$(def_groups | while read -r g; do
                            printf '%-9s %s\n' "$g" "$(group_desc "$g")"; done | paste -sd, -)" \
          ) || return 1
    printf '%s\n' "$sel" | awk '{print $1}'
    return 0
  fi

  # Repli sans gum : une invite texte, aucune dependance.
  printf '\n  %sGroupes%s  %s(* = recommande)%s\n\n' "$B" "$X" "$D" "$X" >&2
  local i=1 names=()
  for g in $avail; do
    names+=("$g")
    local mark=" "; def_groups | grep -qx "$g" && mark="*"
    printf '   %s%2d%s %s %s%-9s%s %s%s%s\n' "$B" "$i" "$X" "$mark" \
           "$C" "$g" "$X" "$D" "$(group_desc "$g")" "$X" >&2
    i=$((i+1))
  done
  printf '\n  Numeros separes par des espaces, %sa%s = tout, %sentree%s = les recommandes : ' \
         "$B" "$X" "$B" "$X" >&2
  local answer; read -r answer
  if [[ -z ${answer:-} ]]; then def_groups; return 0; fi
  if [[ $answer == a || $answer == all ]]; then all_groups; return 0; fi
  local n
  for n in $answer; do
    [[ $n =~ ^[0-9]+$ ]] && [[ $n -ge 1 && $n -le ${#names[@]} ]] && echo "${names[$((n-1))]}"
  done
}

# ── Suite ────────────────────────────────────────────────────────────
cmd_next_steps() {
  step "Ce qu'il reste a faire, a la main"
  if [[ ! -f "$CONFIG_DIR/doom/private.el" ]]; then
    printf '  %s1.%s Identite Doom (nom, e-mail) — non versionnee :\n' "$B" "$X"
    printf '     %scp doom/private.el.example doom/private.el%s\n\n' "$C" "$X"
  fi
  if [[ -d "$CONFIG_DIR/emacs" ]] || command -v emacs >/dev/null 2>&1; then
    printf '  %s2.%s Doom Emacs :\n' "$B" "$X"
    printf '     %sgit clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs%s\n' "$C" "$X"
    printf '     %s~/.config/emacs/bin/doom install%s\n\n' "$C" "$X"
  fi
  printf '  %s3.%s Ouvrir un nouveau shell — ou %sexec zsh%s.\n\n' "$B" "$X" "$C" "$X"
  note "Diagnostic a tout moment :  ./install.sh --check"
}

# ── Arguments ────────────────────────────────────────────────────────
while [[ $# -gt 0 ]]; do
  case "$1" in
    --links|--links-only) MODE=links;        shift ;;
    --all)                MODE=all;          shift ;;
    --recommended)        MODE=recommended;  shift ;;
    --groups)             MODE=groups; GROUPS_ARG="${2:-}"; shift 2 ;;
    --no-links)           DO_LINKS=0;        shift ;;
    --list)               cmd_list; exit 0 ;;
    --check|--doctor)     cmd_check; exit 0 ;;
    --dry-run|-n)         DRY=1;             shift ;;
    --yes|-y)             ASSUME_YES=1;      shift ;;
    --help|-h)            usage; exit 0 ;;
    *)                    err "option inconnue : $1"; usage; exit 2 ;;
  esac
done

# ── Deroulement ──────────────────────────────────────────────────────
printf '\n  %sdotfiles%s  %s%s%s\n' "$B" "$X" "$D" "$CONFIG_DIR" "$X"
[[ $DRY -eq 1 ]] && printf '  %sMODE SIMULATION — rien ne sera modifie.%s\n' "$Y" "$X"

case "$MODE" in
  links)       DO_PACKAGES=0 ;;
  all)         DO_PACKAGES=1; GROUPS_ARG="$(all_groups | paste -sd, -)" ;;
  recommended) DO_PACKAGES=1; GROUPS_ARG="$(def_groups | paste -sd, -)" ;;
  groups)      DO_PACKAGES=1 ;;
  interactive) DO_PACKAGES=1 ;;
esac

[[ $DO_LINKS -eq 1 ]] && cmd_links

if [[ $DO_PACKAGES -eq 1 ]]; then
  if ! command -v brew >/dev/null 2>&1; then
    step "Paquets"
    err "Homebrew absent. Installe-le d'abord : https://brew.sh"
    err "Puis relance :  ./install.sh --no-links ${GROUPS_ARG:+--groups $GROUPS_ARG}"
    exit 1
  fi

  chosen=""
  if [[ $MODE == interactive ]]; then
    chosen="$(select_groups | tr '\n' ' ')"
    [[ -z ${chosen// /} ]] && { warn "aucun groupe choisi"; cmd_next_steps; exit 0; }
  else
    chosen="${GROUPS_ARG//,/ }"
  fi

  bad=""
  for g in $chosen; do group_exists "$g" || bad="$bad $g"; done
  [[ -n $bad ]] && { err "groupe(s) inconnu(s) :$bad"; say; cmd_list; exit 2; }

  step "Paquets"
  note "groupes : $(echo "$chosen" | tr ' ' ',' | sed 's/,$//')"
  if [[ $ASSUME_YES -eq 0 && $DRY -eq 0 && -t 0 ]]; then
    printf '\n  Installer ces groupes ? [O/n] '
    read -r reply
    [[ ${reply:-o} =~ ^[nN] ]] && { warn "annule"; exit 0; }
  fi
  for g in $chosen; do install_group "$g"; done
fi

cmd_next_steps
