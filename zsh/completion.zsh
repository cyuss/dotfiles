# ─────────────────────────────────────────────────────────────────────
#  Complétion — couverture, présentation, suggestions
#
#  Trois couches distinctes, souvent confondues :
#
#    1. LES DONNÉES   ce que zsh sait proposer pour une commande.
#                     zsh-completions couvre le classique ; carapace
#                     ajoute 653 commandes modernes (gh, docker, jj,
#                     kubectl, aws, cargo, ollama…). Les outils qu'il
#                     ne connaît pas génèrent la leur dans
#                     ~/.config/zsh/completions.
#
#    2. LA PRÉSENTATION  comment le menu s'affiche. fzf-tab remplace le
#                     menu natif par fzf, avec un aperçu à droite.
#                     Il était chargé mais sans aucun zstyle : il
#                     tournait donc sans aperçu.
#
#    3. LES SUGGESTIONS  la ligne grisée qui devine la suite depuis
#                     l'historique. zsh-autosuggestions, déjà là.
#
#  Ce fichier est sourcé par ~/.zshrc APRÈS compinit et APRÈS les
#  plugins antidote — fzf-tab exige cet ordre.
# ─────────────────────────────────────────────────────────────────────

# ── 1. Données ───────────────────────────────────────────────────────

# (le fpath des complétions générées est ajouté dans ~/.zshrc AVANT
#  compinit — l'ajouter ici, après, n'aurait aucun effet : compinit ne
#  relit pas le fpath une fois passé.)

# carapace : mis en cache dans un fichier statique, comme antidote.
# `source <(carapace _carapace)` lancerait un sous-processus à CHAQUE
# shell — sur une machine où tu ouvres un pane herdr par tâche, ça se
# paie. Le fichier n'est régénéré que si le binaire est plus récent.
if (( $+commands[carapace] )); then
  # Les ponts : carapace délègue aux systèmes existants quand il n'a
  # pas de spec, au lieu de faire disparaître la complétion.
  export CARAPACE_BRIDGES='zsh,fish,bash'
  # Ne pas remplacer les complétions zsh déjà présentes et meilleures.
  export CARAPACE_EXCLUDES='kubectl'

  _carapace_cache="${XDG_CACHE_HOME:-$HOME/.cache}/carapace-init.zsh"
  if [[ ! -s $_carapace_cache || $commands[carapace] -nt $_carapace_cache ]]; then
    mkdir -p "${_carapace_cache:h}"
    carapace _carapace zsh >| "$_carapace_cache" 2>/dev/null
  fi
  [[ -s $_carapace_cache ]] && source "$_carapace_cache"
  unset _carapace_cache
fi

# ── 2. Présentation ──────────────────────────────────────────────────

# Le menu natif de zsh, comme repli quand fzf-tab ne s'applique pas.
zstyle ':completion:*' menu no                     # fzf-tab s'en charge
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' special-dirs true            # propose ../ et ./

# fzf-tab : la fenêtre et les touches.
zstyle ':fzf-tab:*' fzf-flags --height=45% --layout=reverse --border=rounded \
  --info=inline --prompt='❯ ' --color=hl:#e3b778,hl+:#e3b778,border:#2b303b
zstyle ':fzf-tab:*' switch-group '<' '>'            # changer de groupe
zstyle ':fzf-tab:*' continuous-trigger '/'          # descendre dans un dossier
zstyle ':fzf-tab:*' fzf-min-height 12
zstyle ':fzf-tab:*' prefix ''                       # pas de · devant chaque ligne
zstyle ':fzf-tab:*' single-group color header

# L'aperçu, par nature de complétion. C'est ce qui manquait :
# fzf-tab sans aperçu n'est qu'un menu un peu plus joli.
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

# cd : uniquement l'arborescence, on ne veut pas voir de fichiers.
zstyle ':fzf-tab:complete:cd:*' fzf-preview \
  'eza --tree --level=2 --color=always --icons $realpath 2>/dev/null || ls -la $realpath'
zstyle ':fzf-tab:complete:z:*'  fzf-preview \
  'eza --tree --level=2 --color=always --icons $realpath 2>/dev/null || ls -la $realpath'

# git : montrer le contenu réel plutôt que le nom de la ref.
zstyle ':fzf-tab:complete:git-(add|diff|restore|checkout|switch):*' fzf-preview \
  'git diff --color=always -- $word | delta 2>/dev/null || git diff --color=always -- $word'
zstyle ':fzf-tab:complete:git-(log|show):*'   fzf-preview 'git log --color=always $word 2>/dev/null'
zstyle ':fzf-tab:complete:git-(branch|checkout|switch):argument-1' fzf-preview \
  'git log --oneline --color=always -20 $word 2>/dev/null'

# Variables d'environnement : leur valeur.
zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset|expand):*' \
  fzf-preview 'echo ${(P)word}'

# Processus : la ligne de commande complète, pas le nom tronqué.
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
  '[[ $group == "[process ID]" ]] && ps -p $word -o comm=,args= 2>/dev/null'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:4:wrap

# systemctl-like : rien à prévisualiser sur macOS, mais brew si.
zstyle ':fzf-tab:complete:brew-(install|uninstall|info|upgrade):*' fzf-preview \
  'brew info $word 2>/dev/null | head -30'

# ── make ─────────────────────────────────────────────────────────────
# Par défaut, zsh devine les cibles en lisant le Makefile lui-même, ce qui
# rate tout ce qui vient d'un `include` ou d'une variable. `call-command`
# lui fait appeler make pour obtenir la liste réelle.
zstyle ':completion:*:make:*:targets' call-command true
zstyle ':completion:*:*:make:*' tag-order 'targets variables'

# L'aperçu montre la description `## …` et la recette AVANT de lancer.
# C'est là que le motif auto-documenté de tes Makefile paie.
zstyle ':fzf-tab:complete:make:*' fzf-preview \
  'make-target-info $word 2>/dev/null'
zstyle ':fzf-tab:complete:make:*' fzf-flags --preview-window='right:58%:wrap'

# ── just ─────────────────────────────────────────────────────────────
# `just --show <recette>` imprime la recette et ses dépendances.
zstyle ':fzf-tab:complete:just:*' fzf-preview \
  'just --show $word 2>/dev/null || just --list 2>/dev/null'
zstyle ':fzf-tab:complete:just:*' fzf-flags --preview-window='right:58%:wrap'

# ── 3. Suggestions en ligne ──────────────────────────────────────────

# zsh-autosuggestions : la ligne grisée qui devine la suite.
#   history      ce que TU as déjà tapé — précis, jamais surprenant
#   completion   ce que zsh saurait compléter — utile sur une commande neuve
# L'ordre compte : l'historique d'abord.
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
# Au-delà, la suggestion est plus longue que la ligne et gêne la lecture.
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=60
# Ne pas suggérer pendant un collage : la surbrillance recalculée à
# chaque caractère collé fait ramer un gros paste.
ZSH_AUTOSUGGEST_MANUAL_REBIND=1
# Gris accordé au fond du terminal (premium-noir #101216).
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#5b6272'

# Accepter la suggestion : → (fin de ligne) ou Ctrl-Espace pour un mot.
bindkey '^ ' autosuggest-accept
bindkey '^[[1;5C' forward-word          # Ctrl-→ : un mot de la suggestion
