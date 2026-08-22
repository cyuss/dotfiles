# ══════════════════════════════════════════════════════════════════════
#  ~/.config/zsh/aliases.zsh
#  Construit à partir de ton historique atuin réel :
#  brew 517 · make 220 · ll 215 · cd 189 · doom 149 · nvim 67 · git 57
#  Objectif : moins de frappe, moins de souris.
#  Astuce : `alias | grep <mot>` pour retrouver un alias oublié.
# ══════════════════════════════════════════════════════════════════════

# ── Navigation ────────────────────────────────────────────────────────
alias ls='eza --icons --group-directories-first --git'
alias l='eza -1 --icons --git'
alias ll='eza -lh --icons --group-directories-first --git'
alias la='eza -lha --icons --group-directories-first --git'
alias lt='eza --tree --level=2 --icons'
alias lt3='eza --tree --level=3 --icons'
alias ltg='eza --tree --level=3 --icons --git-ignore'   # ignore ce que git ignore
alias lm='eza -lh --icons --sort=modified --reverse'    # derniers modifiés
alias lsize='eza -lh --icons --sort=size --reverse'     # plus gros d'abord

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias -- -='cd -'                                       # revenir au dossier précédent
alias home='cd ~'
alias dl='cd ~/Downloads'
alias pj='cd ~/Desktop/projects'
alias cfg='cd ~/.config'

alias j='z'        # zoxide : saute vers un dossier déjà visité
alias ji='zi'      # zoxide interactif (fzf)
alias md='mkdir -p'
mkcd() { mkdir -p "$1" && cd "$1"; }                    # crée ET entre dedans

# ── Recherche / inspection ────────────────────────────────────────────
# On ne masque PAS grep, cat, du, ps : leurs remplacants ont des flags
# differents et ca casse scripts et reflexes (rg -E n'est pas grep -E).
alias rgi='rg -i'                                       # insensible à la casse
alias rgf='rg --files | rg'                             # cherche dans les NOMS
alias rgh='rg --hidden --no-ignore'                     # y compris cachés/ignorés
alias b='bat --style=plain'                             # cat joli
alias bn='bat --style=numbers'
alias tree='eza --tree --icons'
# `dust` s'utilise directement (arbre de tailles lisible)
alias df='df -h'
alias top='btop'
alias path='echo $PATH | tr ":" "\n"'                   # PATH lisible, une ligne par entrée
alias now='date "+%Y-%m-%d %H:%M:%S"'

# ── Git — d'après ton usage (pull > status > push) ────────────────────
alias g='git'
alias gs='git status -sb'
alias gp='git pull --rebase'                            # rebase : historique linéaire
alias gpu='git push'
alias gpf='git push --force-with-lease'                 # JAMAIS --force nu : --with-lease
                                                        # refuse d'écraser le travail d'autrui
alias ga='git add'
alias gaa='git add -A'
alias gc='git commit'
alias gcm='git commit -m'
alias gca='git commit --amend --no-edit'
alias gco='git checkout'
alias gsw='git switch'
alias gb='git branch'
alias gd='git diff'                                     # passe par delta
alias gds='git diff --staged'
alias gdt='git dft'                                     # diff syntaxique (AST)
alias gl='git log --oneline --graph -20'
alias glog="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias gst='git stash'
alias gstp='git stash pop'
alias gab='git absorb --and-rebase'                     # range les hunks dans les bons commits
alias gwip='git add -A && git commit -m "wip"'
alias gundo='git reset --soft HEAD~1'                   # défait le commit, garde le travail
alias groot='cd "$(git rev-parse --show-toplevel)"'     # remonte à la racine du dépôt
alias lg='lazygit'

# ── GitHub ────────────────────────────────────────────────────────────
alias ghpr='gh pr create --web'
alias ghprv='gh pr view --web'
alias ghrv='gh repo view --web'
alias ghs='gh pr status'

# ── brew — ta commande n°1 (517 appels) ───────────────────────────────
alias bi='brew install'
alias bs='brew search'
alias bif='brew info'
alias brm='brew uninstall'
alias bsv='brew services'
alias bup='brew update && brew upgrade'                 # ton enchaînement le plus fréquent
alias bclean='brew cleanup --prune=all && brew autoremove'
alias bleaves='brew leaves'                             # paquets installés explicitement
alias bdeps='brew deps --tree --installed'

# ── make / just (220 + 46 appels) ─────────────────────────────────────
# fzf-make : le catalogue interactif des commandes du projet. Il lit
# Makefile, justfile, package.json (npm/pnpm/yarn) et Taskfile, affiche
# la recette en apercu, et garde un historique des cibles lancees.
# Deja installe, jamais utilise — c'est la reponse a « quelles commandes
# ce projet propose-t-il ? » quand on ne veut pas taper `make help`.
alias mk='fzf-make'

alias m='make'
alias mr='make run'
alias mdev='make dev'
alias mg='make gui'
alias mf='make format'
alias mc='make clean'
alias mt='make test'
alias ju='just'
alias jl='just --list'
# Liste les cibles d'un Makefile, même sans cible `help`
mtargets() {
  make -qp 2>/dev/null | awk -F: '/^[a-zA-Z0-9][^$#\/\t=]*:([^=]|$)/ {print $1}' | sort -u
}

# ── Python / uv ───────────────────────────────────────────────────────
alias uvr='uv run'
alias uvs='uv sync'
alias uva='uv add'
alias uvad='uv add --dev'
alias py='uv run python'
alias pt='uv run pytest -q'   # pas 'pytest' : casserait hors projet uv
alias ruffc='uv run ruff check --fix . && uv run ruff format .'
alias venv='uv venv && source .venv/bin/activate'

# ── Node ──────────────────────────────────────────────────────────────
alias ni='npm install'
alias nci='npm ci'
alias nr='npm run'
alias nrd='npm run dev'
alias nrb='npm run build'
alias nrt='npm test'

# ── Éditeurs & agents ─────────────────────────────────────────────────
alias v='nvim'
alias vi='nvim'
alias e="emacsclient -nw -a ''"
alias ec="emacsclient -c -n -a ''"
alias enw="emacsclient -nw -a ''"
alias edaemon='emacs --fg-daemon'
alias doom='~/.config/emacs/bin/doom'
alias ds='~/.config/emacs/bin/doom sync'
alias dd='~/.config/emacs/bin/doom doctor'
alias oc='opencode'
alias cl='claude'
alias cx='codex'

# ── Outils ────────────────────────────────────────────────────────────
alias ldo='lazydocker'
# `mux' (tmuxinator) et `mx' (tmux) retirés le 2026-08-22 : le multiplexeur
# ici est herdr (`hr' recharge sa config, prefix+w ouvre l'arborescence).
# Garder ces deux-là entretenait un réflexe qui renvoie vers l'ancien outil.
# tmux reste installé et appelable par son nom si un projet en a besoin.
alias yz='yazi'
alias k='kubectl'
alias wx='watchexec --clear --restart'
alias wxt='watchexec --clear --restart --exts py,js,ts,rs,go'
alias bench='hyperfine --warmup 3'
# `python3' traversait le shim pyenv à chaque appel, et servait donc la
# version pyenv globale plutôt que celle du projet. `uv run' résout un
# interpréteur directement, sans shim ni indirection.
alias serve='uv run python -m http.server 8000'        # sert le dossier courant

# ── Rechargement de config ────────────────────────────────────────────
alias zr='exec zsh'                                     # recharge le shell
alias ze='$EDITOR ~/.zshrc'
alias zev='$EDITOR ~/.zshenv'
alias za='$EDITOR ~/.config/zsh/aliases.zsh'
alias ar='aerospace reload-config && echo "aerospace ok"'
alias hr='herdr server reload-config'

# ══════════════════════════════════════════════════════════════════════
#  Réponses aux questions que tout dev se pose
# ══════════════════════════════════════════════════════════════════════

# « Qui occupe le port 3000 ? »
port() { lsof -nP -iTCP:"$1" -sTCP:LISTEN; }
# « Tue ce qui occupe le port 3000 »
killport() {                       # BSD xargs n'a pas -r : on teste avant
  local pids; pids=$(lsof -ti tcp:"$1")
  [[ -z $pids ]] && { echo "port $1 : rien à tuer"; return 1; }
  echo "$pids" | xargs kill -9 && echo "port $1 libéré"
}
# « Quels ports sont ouverts ? »
alias ports='lsof -nP -iTCP -sTCP:LISTEN'

# « C'est quoi cette commande, un alias, une fonction, un binaire ? »
alias whichall='type -a'

# « Quelle est mon IP ? »
alias myip='curl -s https://ifconfig.me && echo'
alias localip='ipconfig getifaddr en0'

# « Combien pèse ce dossier ? »
alias dush='du -sh * 2>/dev/null | sort -rh | head -20'

# « Qu'est-ce qui a changé récemment ici ? »
alias recent='eza -lh --icons --sort=modified --reverse | head -20'

# « Combien de lignes de code dans ce projet ? »
alias loc='tokei'

# « Ce JSON est-il valide, et à quoi ressemble-t-il ? »
alias jsonpp='jq .'
alias yamlpp='yq .'

# « Quel est le poids de mes dépendances ? »
alias nsize='du -sh node_modules 2>/dev/null || echo "pas de node_modules"'

# « Combien de temps prend cette commande ? » → bench
# « Quelle version de tout ? »
# `command node' et non `node' : la fonction paresseuse `node()' définie
# dans ~/.zshrc déclencherait `_nvm_load' (593 ms mesurés) juste pour lire
# un numéro de version. Le binaire est déjà dans le PATH via le glob de
# ~/.zshenv — `command' court-circuite la fonction et va droit au binaire.
versions() {
  printf "%-12s %s\n" node "$(command node --version 2>/dev/null)" \
    python "$(python3 --version 2>&1 | cut -d' ' -f2)" \
    uv "$(uv --version 2>/dev/null | cut -d' ' -f2)" \
    git "$(git --version | cut -d' ' -f3)" \
    nvim "$(nvim --version | head -1 | cut -d' ' -f2)" \
    emacs "$(emacs --version | head -1 | cut -d' ' -f3)"
}

# « Extraire cette archive » — sans se souvenir des flags
extract() {
  [[ -f $1 ]] || { echo "fichier introuvable: $1"; return 1; }
  case "$1" in
    *.tar.bz2) tar xjf "$1" ;;  *.tar.gz)  tar xzf "$1" ;;
    *.tar.xz)  tar xJf "$1" ;;  *.tar)     tar xf  "$1" ;;
    *.bz2)     bunzip2 "$1" ;;  *.gz)      gunzip  "$1" ;;
    *.zip)     unzip   "$1" ;;  *.7z)      7z x    "$1" ;;
    *.rar)     unrar x "$1" ;;
    *) echo "format non géré: $1"; return 1 ;;
  esac
}

# « Sauvegarder ce fichier avant de le bidouiller »
bak() { cp -a "$1" "$1.$(date +%Y%m%d-%H%M%S).bak" && echo "→ $1.$(date +%Y%m%d-%H%M%S).bak"; }

# ══════════════════════════════════════════════════════════════════════
#  Widgets fzf — c'est ici qu'on arrête vraiment d'utiliser la souris
#  (les raccourcis sont posés dans ~/.zshrc)
# ══════════════════════════════════════════════════════════════════════

# Fichier flou → éditeur
fe() {
  local f
  f=$(fd --type f --hidden --exclude .git \
       | fzf --preview 'bat --color=always --style=numbers --line-range :400 {}') \
    && ${EDITOR:-nvim} "$f"
}

# Dossier flou → cd
fcd() {
  local d
  d=$(fd --type d --hidden --exclude .git \
       | fzf --preview 'eza --tree --level=2 --icons {}') && cd "$d"
}

# Branche git floue → checkout
fbr() {
  local b
  b=$(git branch -a --sort=-committerdate --format='%(refname:short)' \
       | fzf --preview 'git log --oneline --color=always -20 {}') \
    && git switch "$(sed 's|^origin/||' <<<"$b")"
}

# Commit flou → montre le diff
fshow() {
  local c
  c=$(git log --oneline --color=always -300 \
       | fzf --ansi --preview 'git show --color=always {1}' | awk '{print $1}') \
    && git show "$c"
}

# Processus flou → kill
fkill() {
  local p
  p=$(procs --no-header 2>/dev/null | fzf --header 'processus à tuer' | awk '{print $1}') \
    && kill -${1:-15} "$p" && echo "signal ${1:-15} → $p"
}

# Projet flou → cd (dans ~/Desktop/projects)
fpj() {
  local d
  d=$(fd --type d --max-depth 1 . ~/Desktop/projects \
       | fzf --preview 'eza -lh --icons --git {}') && cd "$d"
}

# Fichier modifié (git) flou → éditeur
fmod() {
  local f
  f=$(git status --porcelain | awk '{print $2}' \
       | fzf --preview 'git diff --color=always -- {}') \
    && ${EDITOR:-nvim} "$f"
}

# ══════════════════════════════════════════════════════════════════════
#  taproom · navi · television — installés le 2026-08-20
# ══════════════════════════════════════════════════════════════════════

# ── taproom : TUI Homebrew (ta commande n°1, 517 appels) ──────────────
alias tap='taproom'
alias tapo='taproom --filters Outdated'       # ce qui est à mettre à jour
alias tapi='taproom --filters Installed'      # ce qui est installé
alias tape='taproom --filters "Expl. Installed"'  # installé explicitement
alias tapc='taproom --filters Casks'
alias tapsz='taproom --sort-column Size'      # trier par poids disque

# ── television : sélecteur flou universel ─────────────────────────────
alias tvf='tv files'
alias tvd='tv dirs'
alias tvg='tv git-log'
alias tvb='tv git-branch'
alias tvr='tv git-repos'
alias tvt='tv text'          # recherche plein texte dans les fichiers
alias tvh='tv bash-history'
alias tve='tv env'
alias tvdk='tv docker-images'
# television → éditeur / cd
tve_() { local f; f=$(tv files) && [[ -n $f ]] && ${EDITOR:-nvim} "$f"; }
tvcd() { local d; d=$(tv dirs)  && [[ -n $d ]] && cd "$d"; }

# ── navi : antisèches interactives ────────────────────────────────────
alias cheat='navi'
alias cheats='navi --print'
alias nvedit='${EDITOR:-nvim} ~/.config/navi/cheats'

# ── atuin : historique ────────────────────────────────────────────────
alias hist='atuin search -i'
alias hstats='atuin stats'
alias hsync='atuin sync'

# ── leetcode ──────────────────────────────────────────────────────────
alias lc='cd ~/Desktop/projects/leetcode-challenges && emacsclient -c -n -a "" --eval "(leetcode)"'
alias lcd='cd ~/Desktop/projects/leetcode-challenges'
# Statistiques : résolus par pattern
lcs() {
  local root=~/Desktop/projects/leetcode-challenges/solutions
  local d n                       # declare AVANT la boucle : en zsh, `local n`
                                  # sans affectation AFFICHE la variable si elle
                                  # existe deja -> "n=0" a chaque tour
  printf "\n  \033[1mLeetCode — résolus par pattern\033[0m\n\n"
  for d in "$root"/*/; do
    n=$(find "$d" -type f \( -name '*.py' -o -name '*.go' -o -name '*.rs' -o -name '*.sql' \) 2>/dev/null | wc -l | tr -d ' ')
    [[ $n -gt 0 ]] && printf "  %-18s %3d\n" "$(basename "$d")" "$n"
  done
  printf "\n  \033[2mtotal : %s\033[0m\n\n" \
    "$(find "$root" -type f \( -name '*.py' -o -name '*.go' -o -name '*.rs' -o -name '*.sql' \) 2>/dev/null | wc -l | tr -d ' ')"
}
# Nouveau problème depuis le template
lcn() {
  [[ $# -lt 3 ]] && { echo "usage: lcn <pattern> <id> <slug>"; return 1; }
  local root=~/Desktop/projects/leetcode-challenges
  local dst="$root/solutions/$1/$2-$3.py"
  [[ -d "$root/solutions/$1" ]] || { echo "pattern inconnu : $1"; ls "$root/solutions"; return 1; }
  [[ -e $dst ]] && { echo "existe déjà : $dst"; return 1; }
  sed -e "s/{ID}/$2/" -e "s/{SLUG}/$3/" -e "s/{PATTERN}/$1/" \
      -e "s/{TITLE}/$(tr '-' ' ' <<<"$3")/" "$root/templates/solution.py" > "$dst"
  ${EDITOR:-nvim} "$dst"
}
