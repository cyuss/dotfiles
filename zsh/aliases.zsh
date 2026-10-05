# ══════════════════════════════════════════════════════════════════════
#  ~/.config/zsh/aliases.zsh
#  Built from my actual atuin history:
#  brew 517 · make 220 · ll 215 · cd 189 · doom 149 · nvim 67 · git 57
#  Goal: less typing, less mouse.
#  Tip: `alias | grep <word>` to find a forgotten alias.
# ══════════════════════════════════════════════════════════════════════

# ── Navigation ────────────────────────────────────────────────────────
alias ls='eza --icons --group-directories-first --git'
alias l='eza -1 --icons --git'
alias ll='eza -lh --icons --group-directories-first --git'
alias la='eza -lha --icons --group-directories-first --git'
alias lt='eza --tree --level=2 --icons'
alias lt3='eza --tree --level=3 --icons'
alias ltg='eza --tree --level=3 --icons --git-ignore'   # skip what git ignores
alias lm='eza -lh --icons --sort=modified --reverse'    # recently modified
alias lsize='eza -lh --icons --sort=size --reverse'     # biggest first

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias -- -='cd -'                                       # back to previous dir
alias home='cd ~'
alias dl='cd ~/Downloads'
alias pj='cd ~/Desktop/projects'
alias cfg='cd ~/.config'

alias j='z'        # zoxide: jump to a dir you've been to
alias ji='zi'      # interactive zoxide (fzf)
alias md='mkdir -p'
mkcd() { mkdir -p "$1" && cd "$1"; }                    # create AND cd into it

# ── Search / inspect ──────────────────────────────────────────────────
# Don't shadow grep, cat, du, ps: the replacements have different flags
# and that breaks scripts and muscle memory (rg -E is not grep -E).
alias rgi='rg -i'                                       # case-insensitive
alias rgf='rg --files | rg'                             # search file NAMES
alias rgh='rg --hidden --no-ignore'                     # include hidden/ignored
alias b='bat --style=plain'                             # pretty cat
alias bn='bat --style=numbers'
alias tree='eza --tree --icons'
# just use `dust` directly (readable size tree)
alias df='df -h'
alias top='btop'
alias path='echo $PATH | tr ":" "\n"'                   # readable PATH, one entry per line
alias now='date "+%Y-%m-%d %H:%M:%S"'

# ── Git, based on my usage (pull > status > push) ─────────────────────
alias g='git'
alias gs='git status -sb'
alias gp='git pull --rebase'                            # rebase: linear history
alias gpu='git push'
alias gpf='git push --force-with-lease'                 # NEVER bare --force: --with-lease
                                                        # won't clobber someone else's work
alias ga='git add'
alias gaa='git add -A'
alias gc='git commit'
alias gcm='git commit -m'
alias gca='git commit --amend --no-edit'
alias gco='git checkout'
alias gsw='git switch'
alias gb='git branch'
alias gd='git diff'                                     # goes through delta
alias gds='git diff --staged'
alias gdt='git dft'                                     # syntax-aware diff (AST)
alias gl='git log --oneline --graph -20'
alias glog="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias gst='git stash'
alias gstp='git stash pop'
alias gab='git absorb --and-rebase'                     # file hunks into the right commits
alias gwip='git add -A && git commit -m "wip"'
alias gundo='git reset --soft HEAD~1'                   # undo the commit, keep the changes
alias groot='cd "$(git rev-parse --show-toplevel)"'     # go to repo root
alias lg='lazygit'

# ── GitHub ────────────────────────────────────────────────────────────
alias ghpr='gh pr create --web'
alias ghprv='gh pr view --web'
alias ghrv='gh repo view --web'
alias ghs='gh pr status'

# ── brew, my #1 command (517 calls) ───────────────────────────────────
alias bi='brew install'
alias bs='brew search'
alias bif='brew info'
alias brm='brew uninstall'
alias bsv='brew services'
alias bup='brew update && brew upgrade'                 # my most common combo
alias bclean='brew cleanup --prune=all && brew autoremove'
alias bleaves='brew leaves'                             # explicitly installed packages
alias bdeps='brew deps --tree --installed'

# ── make / just (220 + 46 calls) ──────────────────────────────────────
# fzf-make: interactive picker for the project's commands. Reads
# Makefile, justfile, package.json (npm/pnpm/yarn) and Taskfile, shows
# the recipe in a preview and keeps a history of what you ran.
# Answers "what commands does this project have?" without `make help`.
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
# List a Makefile's targets, even with no `help` target
mtargets() {
  make -qp 2>/dev/null | awk -F: '/^[a-zA-Z0-9][^$#\/\t=]*:([^=]|$)/ {print $1}' | sort -u
}

# ── Python / uv ───────────────────────────────────────────────────────
alias uvr='uv run'
alias uvs='uv sync'
alias uva='uv add'
alias uvad='uv add --dev'
alias py='uv run python'
alias pt='uv run pytest -q'   # not 'pytest': would break outside a uv project
alias ruffc='uv run ruff check --fix . && uv run ruff format .'
alias venv='uv venv && source .venv/bin/activate'

# ── Node ──────────────────────────────────────────────────────────────
alias ni='npm install'
alias nci='npm ci'
alias nr='npm run'
alias nrd='npm run dev'
alias nrb='npm run build'
alias nrt='npm test'

# ── Editors & agents ──────────────────────────────────────────────────
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

# ── Tools ─────────────────────────────────────────────────────────────
alias ldo='lazydocker'
# `mux' (tmuxinator) and `mx' (tmux) removed 2026-08-22: the multiplexer
# is herdr now (`hr' reloads its config, prefix+w opens the tree).
# Keeping them kept the old reflex alive. tmux is still installed and
# callable by name if a project needs it.
alias yz='yazi'
alias k='kubectl'
alias wx='watchexec --clear --restart'
alias wxt='watchexec --clear --restart --exts py,js,ts,rs,go'
alias bench='hyperfine --warmup 3'
# `python3' went through the pyenv shim every time, so it served the
# global pyenv version instead of the project's. `uv run' picks an
# interpreter directly, no shim.
alias serve='uv run python -m http.server 8000'        # serve current dir

# ── Config reload ─────────────────────────────────────────────────────
alias zr='exec zsh'                                     # reload the shell
alias ze='$EDITOR ~/.zshrc'
alias zev='$EDITOR ~/.zshenv'
alias za='$EDITOR ~/.config/zsh/aliases.zsh'
alias ar='aerospace reload-config && echo "aerospace ok"'
alias hr='herdr server reload-config'

# ══════════════════════════════════════════════════════════════════════
#  Answers to the questions every dev asks
# ══════════════════════════════════════════════════════════════════════

# "Who's on port 3000?"
port() { lsof -nP -iTCP:"$1" -sTCP:LISTEN; }
# "Kill whatever's on port 3000"
killport() {                       # BSD xargs has no -r, so check first
  local pids; pids=$(lsof -ti tcp:"$1")
  [[ -z $pids ]] && { echo "port $1 : rien à tuer"; return 1; }
  echo "$pids" | xargs kill -9 && echo "port $1 libéré"
}
# "Which ports are open?"
alias ports='lsof -nP -iTCP -sTCP:LISTEN'

# "What is this command: alias, function, binary?"
alias whichall='type -a'

# "What's my IP?"
alias myip='curl -s https://ifconfig.me && echo'
alias localip='ipconfig getifaddr en0'

# "How big is this dir?"
alias dush='du -sh * 2>/dev/null | sort -rh | head -20'

# "What changed here recently?"
alias recent='eza -lh --icons --sort=modified --reverse | head -20'

# "How many lines of code in this project?"
alias loc='tokei'

# "Is this JSON valid, and what does it look like?"
alias jsonpp='jq .'
alias yamlpp='yq .'

# "How heavy are my deps?"
alias nsize='du -sh node_modules 2>/dev/null || echo "pas de node_modules"'

# "How long does this command take?" → bench
# "What version of everything?"
# `command node', not `node': the lazy `node()' function in ~/.zshrc
# would trigger `_nvm_load' (593 ms measured) just to print a version.
# The binary is already on PATH via the ~/.zshenv glob, and `command'
# skips the function and goes straight to it.
versions() {
  printf "%-12s %s\n" node "$(command node --version 2>/dev/null)" \
    python "$(python3 --version 2>&1 | cut -d' ' -f2)" \
    uv "$(uv --version 2>/dev/null | cut -d' ' -f2)" \
    git "$(git --version | cut -d' ' -f3)" \
    nvim "$(nvim --version | head -1 | cut -d' ' -f2)" \
    emacs "$(emacs --version | head -1 | cut -d' ' -f3)"
}

# "Extract this archive" without remembering the flags
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

# "Back up this file before messing with it"
bak() { cp -a "$1" "$1.$(date +%Y%m%d-%H%M%S).bak" && echo "→ $1.$(date +%Y%m%d-%H%M%S).bak"; }

# ══════════════════════════════════════════════════════════════════════
#  fzf widgets, this is where the mouse really stops being needed
#  (keys are bound in ~/.zshrc)
# ══════════════════════════════════════════════════════════════════════

# Fuzzy file → editor
fe() {
  local f
  f=$(fd --type f --hidden --exclude .git \
       | fzf --preview 'bat --color=always --style=numbers --line-range :400 {}') \
    && ${EDITOR:-nvim} "$f"
}

# Fuzzy dir → cd
fcd() {
  local d
  d=$(fd --type d --hidden --exclude .git \
       | fzf --preview 'eza --tree --level=2 --icons {}') && cd "$d"
}

# Fuzzy git branch → checkout
fbr() {
  local b
  b=$(git branch -a --sort=-committerdate --format='%(refname:short)' \
       | fzf --preview 'git log --oneline --color=always -20 {}') \
    && git switch "$(sed 's|^origin/||' <<<"$b")"
}

# Fuzzy commit → show the diff
fshow() {
  local c
  c=$(git log --oneline --color=always -300 \
       | fzf --ansi --preview 'git show --color=always {1}' | awk '{print $1}') \
    && git show "$c"
}

# Fuzzy process → kill
fkill() {
  local p
  p=$(procs --no-header 2>/dev/null | fzf --header 'processus à tuer' | awk '{print $1}') \
    && kill -${1:-15} "$p" && echo "signal ${1:-15} → $p"
}

# Fuzzy project → cd (in ~/Desktop/projects)
fpj() {
  local d
  d=$(fd --type d --max-depth 1 . ~/Desktop/projects \
       | fzf --preview 'eza -lh --icons --git {}') && cd "$d"
}

# Fuzzy modified file (git) → editor
fmod() {
  local f
  f=$(git status --porcelain | awk '{print $2}' \
       | fzf --preview 'git diff --color=always -- {}') \
    && ${EDITOR:-nvim} "$f"
}

# ══════════════════════════════════════════════════════════════════════
#  taproom · navi · television, installed 2026-08-20
# ══════════════════════════════════════════════════════════════════════

# ── taproom: Homebrew TUI (my #1 command, 517 calls) ──────────────────
alias tap='taproom'
alias tapo='taproom --filters Outdated'       # what needs updating
alias tapi='taproom --filters Installed'      # what's installed
alias tape='taproom --filters "Expl. Installed"'  # explicitly installed
alias tapc='taproom --filters Casks'
alias tapsz='taproom --sort-column Size'      # sort by disk size

# ── television: universal fuzzy picker ────────────────────────────────
alias tvf='tv files'
alias tvd='tv dirs'
alias tvg='tv git-log'
alias tvb='tv git-branch'
alias tvr='tv git-repos'
alias tvt='tv text'          # full-text search in files
alias tvh='tv bash-history'
alias tve='tv env'
alias tvdk='tv docker-images'
# television → editor / cd
tve_() { local f; f=$(tv files) && [[ -n $f ]] && ${EDITOR:-nvim} "$f"; }
tvcd() { local d; d=$(tv dirs)  && [[ -n $d ]] && cd "$d"; }

# ── navi: interactive cheatsheets ─────────────────────────────────────
alias cheat='navi'
alias cheats='navi --print'
alias nvedit='${EDITOR:-nvim} ~/.config/navi/cheats'

# ── atuin: history ────────────────────────────────────────────────────
alias hist='atuin search -i'
alias hstats='atuin stats'
alias hsync='atuin sync'

# ── leetcode ──────────────────────────────────────────────────────────
alias lc='cd ~/Desktop/projects/leetcode-challenges && emacsclient -c -n -a "" --eval "(leetcode)"'
alias lcd='cd ~/Desktop/projects/leetcode-challenges'
# Stats: solved per pattern
lcs() {
  local root=~/Desktop/projects/leetcode-challenges/solutions
  local d n                       # declare BEFORE the loop: in zsh, `local n`
                                  # with no value PRINTS the var if it already
                                  # exists -> "n=0" on every pass
  printf "\n  \033[1mLeetCode — résolus par pattern\033[0m\n\n"
  for d in "$root"/*/; do
    n=$(find "$d" -type f \( -name '*.py' -o -name '*.go' -o -name '*.rs' -o -name '*.sql' \) 2>/dev/null | wc -l | tr -d ' ')
    [[ $n -gt 0 ]] && printf "  %-18s %3d\n" "$(basename "$d")" "$n"
  done
  printf "\n  \033[2mtotal : %s\033[0m\n\n" \
    "$(find "$root" -type f \( -name '*.py' -o -name '*.go' -o -name '*.rs' -o -name '*.sql' \) 2>/dev/null | wc -l | tr -d ' ')"
}
# New problem from the template
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
