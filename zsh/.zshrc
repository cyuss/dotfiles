# ══════════════════════════════════════════════════════════════════════
#  ~/.zshrc
#  Principe : rien de coûteux au démarrage. Les gestionnaires de versions
#  (nvm, jenv, chruby) sont chargés au premier usage, pas à chaque shell.
# ══════════════════════════════════════════════════════════════════════

# ── PATH / environnement ──────────────────────────────────────────────
# Tout est dans ~/.zshenv, lu AUSSI par les shells non interactifs
# (scripts Emacs, herdr, launchd, `zsh -c`). Ne rien remettre ici.

# ── Prompt ────────────────────────────────────────────────────────────
eval "$(oh-my-posh init zsh --config ~/oh-my-posh-themes/amro.omp.json)"

# ══════════════════════════════════════════════════════════════════════
#  Plugins (antidote) — remplace antigen, non maintenu depuis 2021
#  Liste : ~/.zsh_plugins.txt et ~/.zsh_plugins_fpath.txt
#  antidote génère un fichier statique ; il n'est relancé que si la liste
#  a changé, donc le coût au démarrage est celui d'un simple `source`.
# ══════════════════════════════════════════════════════════════════════
ANTIDOTE_LIB=/opt/homebrew/opt/antidote/share/antidote/antidote.zsh

_antidote_static() {           # $1 = nom de base (sans extension)
  local txt="$HOME/$1.txt" zsh="$HOME/$1.zsh"
  [[ -f $txt ]] || return 0
  if [[ ! $zsh -nt $txt ]]; then
    [[ -f $ANTIDOTE_LIB ]] && source "$ANTIDOTE_LIB"
    antidote bundle <"$txt" >|"$zsh"
  fi
  source "$zsh"
}

# 1. completions seules (fpath) — obligatoirement avant compinit
_antidote_static .zsh_plugins_fpath

# ── Completions ───────────────────────────────────────────────────────
# Complétions générées par les outils eux-mêmes (uv, ruff, herdr, jj, mise…)
# que carapace ne couvre pas. OBLIGATOIREMENT avant compinit : celui-ci ne
# relit pas le fpath une fois qu'il est passé.
# Régénérables avec : zsh-completions-refresh
fpath=("${XDG_CONFIG_HOME:-$HOME/.config}/zsh/completions" $fpath)

# Le dump n'est revérifié qu'une fois par jour ; sinon compinit -C (rapide).
autoload -Uz compinit
_zcompdump="${ZDOTDIR:-$HOME}/.zcompdump"
if [[ -n ${_zcompdump}(#qN.mh+24) ]]; then
  compinit -d "$_zcompdump"
else
  compinit -C -d "$_zcompdump"
fi
unset _zcompdump

# Completions maison (Azure, k8s, helm…) — chargées si présentes
for _c in az kompose helm kubectl kustomize k3d; do
  [[ -r "$HOME/.oh-my-zsh/custom/$_c.zsh" ]] && source "$HOME/.oh-my-zsh/custom/$_c.zsh"
done
unset _c

# 2. plugins normaux — après compinit (fzf-tab l'exige)
_antidote_static .zsh_plugins

# 3. complétion : couverture (carapace + complétions générées), présentation
#    (fzf-tab et ses aperçus) et suggestions en ligne. Doit venir APRÈS
#    compinit et APRÈS les plugins — voir l'en-tête du fichier.
[[ -r "$HOME/.config/zsh/completion.zsh" ]] && source "$HOME/.config/zsh/completion.zsh"

# ══════════════════════════════════════════════════════════════════════
#  Gestionnaires de versions — chargement paresseux
#  Avant : nvm 593 ms + jenv 153 ms à CHAQUE shell (chaque pane tmux).
#  Maintenant : le binaire est dans le PATH tout de suite, et l'outil
#  lui-même ne se charge qu'au premier appel de `nvm` / `jenv` / `chruby`.
# ══════════════════════════════════════════════════════════════════════

# --- Node : PATH immédiat, nvm paresseux ---

_nvm_load() {
  unfunction nvm node npm npx yarn pnpm corepack 2>/dev/null
  [[ -s "$NVM_DIR/nvm.sh" ]] && source "$NVM_DIR/nvm.sh"
  [[ -s "$NVM_DIR/bash_completion" ]] && source "$NVM_DIR/bash_completion"
}
nvm()      { _nvm_load; nvm "$@"; }
node()     { _nvm_load; node "$@"; }
npm()      { _nvm_load; npm "$@"; }
npx()      { _nvm_load; npx "$@"; }
yarn()     { _nvm_load; yarn "$@"; }
pnpm()     { _nvm_load; pnpm "$@"; }
corepack() { _nvm_load; corepack "$@"; }

# --- Java : les shims sont déjà dans le PATH, jenv paresseux ---
jenv() {
  unfunction jenv
  eval "$(command jenv init -)"
  jenv "$@"
}

# --- Ruby : chruby paresseux (auto.sh pose un hook sur chaque `cd`) ---
_chruby_load() {
  unfunction chruby ruby gem bundle rake ruby-install 2>/dev/null
  source /opt/homebrew/opt/chruby/share/chruby/chruby.sh
  source /opt/homebrew/opt/chruby/share/chruby/auto.sh
}
chruby()       { _chruby_load; chruby "$@"; }
ruby()         { _chruby_load; ruby "$@"; }
gem()          { _chruby_load; gem "$@"; }
bundle()       { _chruby_load; bundle "$@"; }
rake()         { _chruby_load; rake "$@"; }
ruby-install() { _chruby_load; ruby-install "$@"; }

# --- Python : pyenv paresseux ---
#
# Mesure : `eval "$(pyenv init - zsh)"` coûtait 155 ms à CHAQUE shell —
# 45 % du démarrage total. Le détail, chronométré séparément :
#   98 ms  `command pyenv rehash`, qui reconstruit tous les shims
#   ~40 ms un sous-processus `bash --norc` pour dédoublonner le PATH
#   ~15 ms le source des complétions
#
# Or le rehash n'est utile qu'APRÈS avoir installé une version de Python
# ou un paquet qui pose un script. Le payer à chaque pane herdr, chaque
# `zsh -c`, chaque sous-shell est du pur gaspillage.
#
# Et c'est sans risque ici : ~/.zshenv met déjà "$PYENV_ROOT/shims" en
# TÊTE du PATH. `python`, `pip`, `uv` fonctionnent donc immédiatement,
# sans que `pyenv init` ait jamais tourné. Seule la FONCTION `pyenv`
# (celle qui rend `pyenv shell <version>` possible en modifiant le shell
# courant) doit être créée — d'où le même motif que nvm et jenv ci-dessus.
#
# `pyenv rehash` reste disponible à la main après une installation.
#
# UNE chose que `pyenv init` faisait et qu'il faut reproduire : ses deux
# premières lignes retirent les shims du PATH puis les REMETTENT EN TÊTE,
# devant /opt/homebrew/bin. Sans ça, 29 binaires changent de résolution —
# vérifié : python3, pip3, black, isort, flake8, pyright, pydoc3 et les
# outils spark existent des deux côtés. `python3` serait passé de 3.10.7
# (pyenv) à 3.14.7 (homebrew), en silence.
#
# On le fait ici, dans .zshrc, et PAS dans .zshenv : c'est exactement la
# portée qu'avait `pyenv init`, qui n'était appelé que pour les shells
# interactifs. Les shells non interactifs (scripts Emacs, herdr, launchd)
# gardent donc le PATH qu'ils avaient déjà — aucun changement pour eux.
#
# Coût : une opération sur un tableau zsh, aucun sous-processus.
path=("$PYENV_ROOT/shims" ${path:#$PYENV_ROOT/shims})

pyenv() {
  unfunction pyenv
  eval "$(command pyenv init - zsh)"
  pyenv "$@"
}

# ══════════════════════════════════════════════════════════════════════
#  mise — INSTALLÉ MAIS PAS ACTIVÉ, et c'est un choix mesuré
#  (recommandation nº 2, 2026-08-22)
# ══════════════════════════════════════════════════════════════════════
#
#  mise remplace pyenv + nvm + jenv + chruby par un seul outil, sans
#  shims, avec un `mise.toml` ou `.tool-versions` par projet. C'était bien
#  l'idée. Mais après avoir rendu pyenv paresseux (bloc ci-dessus), la
#  mesure retourne l'argument :
#
#    coût de `eval "$(mise activate zsh)"`      +30,8 ms à chaque shell
#    gain du retrait des 4 chargeurs paresseux   −0,11 ms au total
#    projets ayant un mise.toml/.tool-versions        0
#
#  Autrement dit : migrer aujourd'hui coûterait +30 ms par shell — 23 %
#  de régression sur les 130 ms actuels — pour zéro gain fonctionnel. Le
#  bénéfice de vitesse que mise promettait a déjà été encaissé par le
#  chargement paresseux de pyenv, qui valait 155 ms.
#
#  Et il ne prendrait même pas la main sur les deux projets qui pinnent
#  Node : `mise settings get idiomatic_version_file_enable_tools` répond
#  `[]` — mise IGNORE .nvmrc, .python-version, .ruby-version et
#  .java-version tant qu'on ne les active pas explicitement.
#
#  Ce qui resterait vrai en faveur de mise : un seul outil au lieu de
#  quatre, un fichier de version par projet, et le changement automatique
#  en entrant dans un dossier. C'est un gain de simplicité, pas de vitesse.
#
#  ── Pour basculer, quand tu le décideras ───────────────────────────
#  1. Enseigner à mise les versions que tu utilises déjà :
#       mise use -g node@24 python@3.10.7 java@temurin-20
#  2. Lui faire lire tes .nvmrc existants (mon-garage-auto: 22, shortio: 20) :
#       mise settings set idiomatic_version_file_enable_tools "node,python"
#     NB: mon-garage-auto pinne Node 22, que nvm N'A PAS installé
#     (18, 20, 24 seulement). mise le règlerait ; `nvm install 22` aussi.
#  3. Décommenter la ligne ci-dessous.
#  4. Retirer alors les blocs pyenv / nvm / jenv / chruby ci-dessus, et
#     le glob nvm de ~/.zshenv — sinon les deux systèmes se marchent
#     dessus (les shims pyenv/jenv restent devant dans le PATH).
#  5. Contrôler qu'aucun binaire ne change de résolution :
#       for b in python python3 pip3 node npm java black isort pyright; do
#         printf '%-10s %s\n' $b "$(command -v $b)"; done
#     29 binaires existent à la fois en shim pyenv et dans homebrew.
#
# eval "$(mise activate zsh)"
#
#  mise reste utilisable à la main sans activation :
#    mise ls-remote node · mise install node@22 · mise exec node@22 -- node -v

# conda retiré : uv fait le travail.
# L'installation est toujours là (~/miniconda3, 7,9 Go, env `ml-utils`).
# Pour la restaurer :   eval "$(~/miniconda3/bin/conda shell.zsh hook)"
# Pour la supprimer :   rm -rf ~/miniconda3 ~/.condarc ~/.conda

# ══════════════════════════════════════════════════════════════════════
#  Environnement
# ══════════════════════════════════════════════════════════════════════
export MANPAGER="sh -c 'col -bx | bat -l man -p'"


# fzf : une seule définition, avec preview et respect du .gitignore
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git'
export FZF_DEFAULT_OPTS="
  --height 60% --layout=reverse --border=rounded --margin=1,2
  --preview 'bat --color=always --style=numbers --line-range :300 {} 2>/dev/null || eza --tree --level=2 --icons {}'
  --preview-window 'right:55%:wrap:hidden'
  --bind 'ctrl-/:toggle-preview,ctrl-u:preview-half-page-up,ctrl-d:preview-half-page-down'
"

# Flags de compilation llvm@11 (2020) retirés du global : ils s'appliquaient
# à TOUTE compilation (pip, cargo, cgo, npm rebuild). À remettre par projet
# via direnv (.envrc) si un build en a réellement besoin :
#   export PATH="/opt/homebrew/opt/llvm@11/bin:$PATH"
#   export LDFLAGS="-L/opt/homebrew/opt/llvm@11/lib"
#   export CPPFLAGS="-I/opt/homebrew/opt/llvm@11/include"

# Hadoop retiré : /opt/homebrew/Cellar/hadoop/3.3.4 n'existe plus.
# Si tu réinstalles : brew install hadoop && export HADOOP_HOME=/opt/homebrew/opt/hadoop/libexec

[[ -f "$HOME/.ghcup/env" ]] && source "$HOME/.ghcup/env"
[[ -f "$HOME/.local/bin/env" ]] && source "$HOME/.local/bin/env"

# ══════════════════════════════════════════════════════════════════════
#  Clavier (readline / emacs)
# ══════════════════════════════════════════════════════════════════════
bindkey -e
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line
bindkey -M viins '^A' beginning-of-line
bindkey -M viins '^E' end-of-line

# ══════════════════════════════════════════════════════════════════════
#  Alias & fonctions
#  Dans ~/.config/zsh/aliases.zsh (versionne avec le reste de la config).
#  `za` pour l'editer, `zr` pour recharger le shell.
# ══════════════════════════════════════════════════════════════════════
[[ -r ~/.config/zsh/aliases.zsh ]] && source ~/.config/zsh/aliases.zsh

# ── Raccourcis clavier fzf : la souris devient inutile ────────────────
# Widgets zsh -> touche. Ctrl+G = Git, Ctrl+O = Open, Ctrl+P = Projet.
_fzf_widget() { zle -I; "$1"; zle reset-prompt; }
fe-widget()   { _fzf_widget fe }   ; zle -N fe-widget
fcd-widget()  { _fzf_widget fcd }  ; zle -N fcd-widget
fbr-widget()  { _fzf_widget fbr }  ; zle -N fbr-widget
fpj-widget()  { _fzf_widget fpj }  ; zle -N fpj-widget
fmod-widget() { _fzf_widget fmod } ; zle -N fmod-widget

bindkey '^O' fe-widget      # Ctrl+O  ouvrir un fichier
bindkey '^G' fbr-widget     # Ctrl+G  changer de branche git
bindkey '^P' fpj-widget     # Ctrl+P  sauter dans un projet
bindkey '^F' fcd-widget     # Ctrl+F  changer de dossier
bindkey '^N' fmod-widget    # Ctrl+N  éditer un fichier modifié

# ══════════════════════════════════════════════════════════════════════
#  Intégrations
# ══════════════════════════════════════════════════════════════════════
eval "$(zoxide init zsh)"
eval "$(atuin init zsh)"
[[ -r "$HOME/.config/broot/launcher/bash/br" ]] && source "$HOME/.config/broot/launcher/bash/br"
command -v direnv >/dev/null && eval "$(direnv hook zsh)"

compdef __start_kubectl k 2>/dev/null

# ══════════════════════════════════════════════════════════════════════
#  Bannière — une seule fois par fenêtre de terminal
#  (la variable est exportée, donc les sous-shells et les panes tmux
#  ouverts depuis cette session ne la réaffichent pas)
# ══════════════════════════════════════════════════════════════════════
if [[ -o interactive && -z $FASTFETCH_SHOWN ]] && command -v fastfetch >/dev/null; then
  export FASTFETCH_SHOWN=1
  fastfetch
fi
