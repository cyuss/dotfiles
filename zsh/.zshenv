# ══════════════════════════════════════════════════════════════════════
#  ~/.zshenv: read by EVERY zsh, non-interactive ones too.
#
#  That's the point. .zshrc is only read by interactive shells, so a
#  script started by Emacs, herdr, launchd, cron or `zsh -c` never saw
#  ~/.local/bin, homebrew or the pyenv shims. Hence the classic "works
#  in my terminal but not from Emacs".
#
#  Only PATH and env vars here. Nothing interactive (no aliases, no
#  prompt, no completion).
# ══════════════════════════════════════════════════════════════════════

# `path` is the array tied to PATH, -U drops duplicates.
# -x is required: when zsh starts with no PATH in the env (env -i, some
# launchd services), `typeset -U PATH` creates the var without export.
# Child processes then get the system default PATH, and `pyenv init`
# (which rebuilds PATH through a bash subshell) spreads that cut-down
# PATH to the whole shell.
typeset -gxU PATH path
typeset -gU fpath

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export PYENV_ROOT="$HOME/.pyenv"
export POETRY_HOME="$HOME/Library/Application Support/pypoetry"
export NVM_DIR="$HOME/.nvm"

path=(
  /opt/homebrew/bin
  /opt/homebrew/sbin
  "$PYENV_ROOT/shims"                 # shims first: python/uv/pip
  "$PYENV_ROOT/bin"
  "$HOME/.local/bin"
  # Don't add "$POETRY_HOME/bin" here: the path has spaces ("Application
  # Support") and `pyenv init` chokes on it, rebuilds PATH from scratch
  # and homebrew vanishes. poetry is symlinked into ~/.local/bin instead.
  "$HOME/.opencode/bin"
  "$HOME/.jenv/shims"
  "$HOME/.lmstudio/bin"
  "$HOME/.config/workflow-tools"
  /opt/homebrew/opt/openjdk/bin
  $path
  # System dirs spelled out: a shell started with no inherited PATH
  # (env -i, some launchd services) has no /usr/sbin and loses stuff
  # like lsof. typeset -U dedupes if path_helper already added them.
  /usr/local/bin
  /usr/bin
  /bin
  /usr/sbin
  /sbin
)

# Node: bin dir of the default version, found by glob (no subprocess).
# Use nvm's `default` alias if it points to a real version, else take the
# newest one. (On = sort descending, [1] = first entry.)
if [[ -d "$NVM_DIR/versions/node" ]]; then
  _nvm_pick=""
  if [[ -r "$NVM_DIR/alias/default" ]]; then
    _nvm_alias="$(<"$NVM_DIR/alias/default")"
    # "v24.5.0" -> direct path, "node"/"lts/*" -> fall back to the glob
    [[ -d "$NVM_DIR/versions/node/$_nvm_alias" ]] && _nvm_pick="$NVM_DIR/versions/node/$_nvm_alias"
    # "24" -> newest one starting with v24
    if [[ -z $_nvm_pick && $_nvm_alias == <-> ]]; then
      _nvm_pick=("$NVM_DIR"/versions/node/v${_nvm_alias}.*(N/On[1]))
    fi
  fi
  [[ -z $_nvm_pick ]] && _nvm_pick=("$NVM_DIR"/versions/node/*(N/On[1]))
  [[ -n $_nvm_pick ]] && path=("${_nvm_pick}/bin" $path)
  unset _nvm_pick _nvm_alias
fi

# ── Editors ───────────────────────────────────────────────────────────
export EDITOR="nvim"
export VISUAL="emacsclient -c -a ''"

# ── Misc ──────────────────────────────────────────────────────────────
export TESSDATA_PREFIX=/opt/homebrew/share/tessdata
export BAT_THEME="ansi"

# ── File descriptors ──────────────────────────────────────────────────
# macOS starts at 256 (launchctl limit maxfiles) and herdr passes its own
# limit (8192) to the panes it spawns. opencode won't start below some
# threshold ("possibly due to low max file descriptors"), so a project's
# "agent" tab opened on an error.
#
# The hard limit is unlimited on this machine, so raising the soft one
# needs no privileges. Here and not in .zshrc because .zshenv is also
# read by non-interactive shells (scripts, launchd, `zsh -c`).
if [[ $(ulimit -n) -lt 65536 ]]; then
  ulimit -n 65536 2>/dev/null || true
fi

# brew upgrade deletes the old herdr version while the server is still
# running from it. macOS can't find the binary anymore and then denies
# ~/Desktop to every pane ("Operation not permitted"). So keep the old
# version on disk (clean up by hand: brew cleanup herdr).
export HOMEBREW_NO_CLEANUP_FORMULAE=herdr
