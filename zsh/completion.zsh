# ─────────────────────────────────────────────────────────────────────
#  Completion: coverage, display, suggestions
#
#  Three separate layers that are easy to mix up:
#
#    1. DATA          what zsh can offer for a command.
#                     zsh-completions covers the classics, carapace
#                     adds 653 modern commands (gh, docker, jj,
#                     kubectl, aws, cargo, ollama...). Tools it
#                     doesn't know generate their own into
#                     ~/.config/zsh/completions.
#
#    2. DISPLAY       how the menu looks. fzf-tab swaps the native
#                     menu for fzf, with a preview on the right.
#                     It used to be loaded with no zstyle at all,
#                     so it ran without any preview.
#
#    3. SUGGESTIONS   the grey text guessing the rest from history.
#                     zsh-autosuggestions, already there.
#
#  Sourced by ~/.zshrc AFTER compinit and AFTER the antidote plugins.
#  fzf-tab needs that order.
# ─────────────────────────────────────────────────────────────────────

# ── 1. Data ─────────────────────────────────────────────────────────

# (the fpath for generated completions is added in ~/.zshrc BEFORE
#  compinit. Adding it here, after, would do nothing: compinit doesn't
#  re-read fpath once it has run.)

# carapace: cached in a static file, like antidote.
# `source <(carapace _carapace)` would spawn a subprocess in every shell,
# which adds up when you open a herdr pane per task. The file is only
# regenerated when the binary is newer.
if (( $+commands[carapace] )); then
  # Bridges: carapace hands off to the existing systems when it has no
  # spec, instead of killing the completion.
  export CARAPACE_BRIDGES='zsh,fish,bash'
  # Don't override zsh completions that are already there and better.
  export CARAPACE_EXCLUDES='kubectl'

  _carapace_cache="${XDG_CACHE_HOME:-$HOME/.cache}/carapace-init.zsh"
  if [[ ! -s $_carapace_cache || $commands[carapace] -nt $_carapace_cache ]]; then
    mkdir -p "${_carapace_cache:h}"
    carapace _carapace zsh >| "$_carapace_cache" 2>/dev/null
  fi
  [[ -s $_carapace_cache ]] && source "$_carapace_cache"
  unset _carapace_cache
fi

# ── 2. Display ──────────────────────────────────────────────────────

# Native zsh menu, as a fallback when fzf-tab doesn't kick in.
zstyle ':completion:*' menu no                     # fzf-tab handles it
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' special-dirs true            # offer ../ and ./

# fzf-tab: window and keys.
zstyle ':fzf-tab:*' fzf-flags --height=45% --layout=reverse --border=rounded \
  --info=inline --prompt='❯ ' --color=hl:#e3b778,hl+:#e3b778,border:#2b303b
zstyle ':fzf-tab:*' switch-group '<' '>'            # switch group
zstyle ':fzf-tab:*' continuous-trigger '/'          # go down into a dir
zstyle ':fzf-tab:*' fzf-min-height 12
zstyle ':fzf-tab:*' prefix ''                       # no · before each line
zstyle ':fzf-tab:*' single-group color header

# Preview, per completion type. This was the missing piece:
# fzf-tab without a preview is just a slightly prettier menu.
zstyle ':fzf-tab:complete:*:*' fzf-preview '
  if [[ -d $realpath ]]; then
    eza --tree --level=2 --color=always --icons --group-directories-first $realpath 2>/dev/null \
      || ls -la --color=always $realpath
  elif [[ -f $realpath ]]; then
    bat --style=numbers --color=always --line-range=:80 $realpath 2>/dev/null \
      || head -80 $realpath
  else
    echo $word
  fi'

# cd: tree only, no files.
zstyle ':fzf-tab:complete:cd:*' fzf-preview \
  'eza --tree --level=2 --color=always --icons $realpath 2>/dev/null || ls -la $realpath'
zstyle ':fzf-tab:complete:z:*'  fzf-preview \
  'eza --tree --level=2 --color=always --icons $realpath 2>/dev/null || ls -la $realpath'

# git: show the actual content, not just the ref name.
zstyle ':fzf-tab:complete:git-(add|diff|restore|checkout|switch):*' fzf-preview \
  'git diff --color=always -- $word | delta 2>/dev/null || git diff --color=always -- $word'
zstyle ':fzf-tab:complete:git-(log|show):*'   fzf-preview 'git log --color=always $word 2>/dev/null'
zstyle ':fzf-tab:complete:git-(branch|checkout|switch):argument-1' fzf-preview \
  'git log --oneline --color=always -20 $word 2>/dev/null'

# claude --resume: the uuid means nothing. The preview is the session
# card: full path on top, then age, size, title and the first exchanges.
# Without it, the generic rule above fell back to `echo $word' and only
# showed the uuid, already visible in the list.
zstyle ':fzf-tab:complete:claude:*' fzf-preview \
  '${XDG_CONFIG_HOME:-$HOME/.config}/workflow-tools/claude-sessions --show $word 2>/dev/null || echo $word'
zstyle ':fzf-tab:complete:claude:*' fzf-flags \
  --height=70% --layout=reverse --border=rounded --info=inline --prompt='❯ ' \
  --color=hl:#e3b778,hl+:#e3b778,border:#2b303b \
  --preview-window='right:55%:wrap'

# Env vars: their value.
zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset|expand):*' \
  fzf-preview 'echo ${(P)word}'

# Processes: full command line, not the truncated name.
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
  '[[ $group == "[process ID]" ]] && ps -p $word -o comm=,args= 2>/dev/null'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:4:wrap

# systemctl-like: nothing to preview on macOS, but brew is.
zstyle ':fzf-tab:complete:brew-(install|uninstall|info|upgrade):*' fzf-preview \
  'brew info $word 2>/dev/null | head -30'

# ── make ─────────────────────────────────────────────────────────────
# By default zsh guesses targets by reading the Makefile itself, which
# misses anything from an `include` or a variable. `call-command` makes
# it ask make for the real list.
zstyle ':completion:*:make:*:targets' call-command true
zstyle ':completion:*:*:make:*' tag-order 'targets variables'

# Preview shows the `## ...` description and the recipe BEFORE running.
# This is where the self-documenting Makefile pattern pays off.
zstyle ':fzf-tab:complete:make:*' fzf-preview \
  'make-target-info $word 2>/dev/null'
zstyle ':fzf-tab:complete:make:*' fzf-flags --preview-window='right:58%:wrap'

# ── just ─────────────────────────────────────────────────────────────
# `just --show <recipe>` prints the recipe and its deps.
zstyle ':fzf-tab:complete:just:*' fzf-preview \
  'just --show $word 2>/dev/null || just --list 2>/dev/null'
zstyle ':fzf-tab:complete:just:*' fzf-flags --preview-window='right:58%:wrap'

# ── 3. Inline suggestions ────────────────────────────────────────────

# zsh-autosuggestions: grey text guessing the rest.
#   history      what you already typed (precise, never surprising)
#   completion   what zsh could complete (handy on a new command)
# Order matters: history first.
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
# Past this, the suggestion gets longer than the line and gets in the way.
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=60
# No suggestions while pasting: recomputing the highlight on every
# pasted char makes a big paste lag.
ZSH_AUTOSUGGEST_MANUAL_REBIND=1
# Grey tuned to the terminal background (premium-noir #101216).
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#5b6272'

# Accept the suggestion: → (end of line) or Ctrl-Space for a word.
bindkey '^ ' autosuggest-accept
bindkey '^[[1;5C' forward-word          # Ctrl-→: one word of the suggestion
