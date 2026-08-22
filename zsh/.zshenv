# ══════════════════════════════════════════════════════════════════════
#  ~/.zshenv — lu par TOUS les shells zsh, y compris non interactifs.
#
#  C'est la difference qui compte : .zshrc n'est lu que par les shells
#  interactifs. Un script lance par Emacs, herdr, launchd, une tache cron
#  ou `zsh -c` ne le voit jamais — et donc ne trouvait ni ~/.local/bin,
#  ni homebrew, ni les shims pyenv. D'ou les classiques « ca marche dans
#  mon terminal mais pas depuis Emacs ».
#
#  Ici : uniquement le PATH et les variables d'environnement.
#  Rien d'interactif (pas d'alias, pas de prompt, pas de completion).
# ══════════════════════════════════════════════════════════════════════

# `path` est le tableau lie a PATH ; -U supprime les doublons.
# -x est OBLIGATOIRE ici : quand zsh demarre sans PATH dans l'environnement
# (env -i, certains services launchd), `typeset -U PATH` cree la variable
# SANS l'attribut export. Les sous-processus recoivent alors le PATH par
# defaut du systeme — et `pyenv init`, qui reconstruit PATH via un sous-shell
# bash, propage ce PATH ampute a tout le shell.
typeset -gxU PATH path
typeset -gU fpath

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export PYENV_ROOT="$HOME/.pyenv"
export POETRY_HOME="$HOME/Library/Application Support/pypoetry"
export NVM_DIR="$HOME/.nvm"

path=(
  /opt/homebrew/bin
  /opt/homebrew/sbin
  "$PYENV_ROOT/shims"                 # shims avant tout : python/uv/pip
  "$PYENV_ROOT/bin"
  "$HOME/.local/bin"
  # NB: on n'ajoute PAS "$POETRY_HOME/bin" au PATH : ce chemin contient
  # des espaces ("Application Support") et `pyenv init` casse dessus —
  # il reconstruit alors PATH depuis zero et fait disparaitre homebrew.
  # poetry est symlinke dans ~/.local/bin a la place.
  "$HOME/.opencode/bin"
  "$HOME/.jenv/shims"
  "$HOME/.lmstudio/bin"
  "$HOME/.config/workflow-tools"
  /opt/homebrew/opt/openjdk/bin
  $path
  # Repertoires systeme explicites : sans eux, un shell demarre sans PATH
  # herite (env -i, certains services launchd) n'a pas /usr/sbin et perd
  # des binaires comme lsof. typeset -U dedoublonne si path_helper les
  # a deja ajoutes.
  /usr/local/bin
  /usr/bin
  /bin
  /usr/sbin
  /sbin
)

# Node : le bin de la version par defaut, resolu par glob (aucun sous-processus).
# On honore l'alias `default` de nvm s'il pointe une version precise, sinon on
# prend la plus recente. (On = tri decroissant, [1] = premiere entree.)
if [[ -d "$NVM_DIR/versions/node" ]]; then
  _nvm_pick=""
  if [[ -r "$NVM_DIR/alias/default" ]]; then
    _nvm_alias="$(<"$NVM_DIR/alias/default")"
    # "v24.5.0" -> chemin direct ; "node"/"lts/*" -> on retombe sur le glob
    [[ -d "$NVM_DIR/versions/node/$_nvm_alias" ]] && _nvm_pick="$NVM_DIR/versions/node/$_nvm_alias"
    # "24" -> on prend la plus recente qui commence par v24
    if [[ -z $_nvm_pick && $_nvm_alias == <-> ]]; then
      _nvm_pick=("$NVM_DIR"/versions/node/v${_nvm_alias}.*(N/On[1]))
    fi
  fi
  [[ -z $_nvm_pick ]] && _nvm_pick=("$NVM_DIR"/versions/node/*(N/On[1]))
  [[ -n $_nvm_pick ]] && path=("${_nvm_pick}/bin" $path)
  unset _nvm_pick _nvm_alias
fi

# ── Editeurs ──────────────────────────────────────────────────────────
export EDITOR="nvim"
export VISUAL="emacsclient -c -a ''"

# ── Divers ────────────────────────────────────────────────────────────
export TESSDATA_PREFIX=/opt/homebrew/share/tessdata
export BAT_THEME="ansi"
