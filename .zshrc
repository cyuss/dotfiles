# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/Users/youcef/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="robbyrussell"
ZSH_THEME="agnoster"

# Suppress the warning prompt
POWERLEVEL9K_INSTANT_PROMPT=quiet

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    bundler
    dotenv
    osx
    rake
    # rbenv
    ruby
    brew
    dash
    docker
    docker-compose
    frontend-search
    pip
    python
    pyenv
    sudo
    sublime
    themes
    web-search
    virtualenv
    timer
    pylint
    mosh
    iterm2
    history
    branch-manager
    # hacker-quotes
    k
    you-should-use
    zsh-autosuggestions
    zsh-command-note
    urltools
    jsontools
    cheatsheet)

# load fzf-tab plugin
source ~/.oh-my-zsh/custom/plugins/fzf-tab/fzf-tab.plugin.zsh
source ~/.oh-my-zsh/custom/plugins/fzf-marks/fzf-marks.plugin.zsh

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='emacs -nw'
# else
#   export EDITOR='emacs -nw'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
# general use
# alias ls='exa'                                                         # ls
# alias l='exa -lbF --git'                                               # list, size, type, git
# alias ll='exa -lbGF --git'                                             # long list
# alias llm='exa -lbGd --git --sort=modified'                            # long list, modified date sort
# alias la='exa -lbhHigUmuSa --time-style=long-iso --git --color-scale'  # all list
# alias lx='exa -lbhHigUmuSa@ --time-style=long-iso --git --color-scale' # all + extended list

# # specialty views
# alias lS='exa -1'                                                      # one column, just names
# alias lt='exa --tree --level=2'                                        # tree

# Move standard ls
# alias ols="ls"
# Base formats
alias lc="colorls -A"           # short, multi-line
alias ll="colorls -1A"          # list, 1 per line
alias ld="ll"                   # ^^^, NOTE: Trying to move to this for alternate hand commands
alias la="colorls -lA"          # list w/ info
# [d] Sort output with directories first
alias lcd="lc --sort-dirs"
alias lld="ll --sort-dirs"
alias ldd="ld --sort-dirs"
alias lad="la --sort-dirs"
# [t] Sort output with recent modified first
alias lct="lc -t"
alias llt="ll -t"
alias ldt="ld -t"
alias lat="la -t"
# [g] Add git status of each item in output
alias lcg="lc --git-status"
alias llg="ll --git-status"
alias ldg="ld --git-status"
alias lag="la --git-status"

# colorls
# alias lc='colorls'                                                     # colorls alias
# emacs
alias enw='emacs -nw'

# LaTeX settings
alias xelatex='/Library/TeX/texbin/xelatex'
alias pdflatex='/Library/TeX/texbin/pdflatex'
alias bibtex='/Library/TeX/texbin/bibtex'

# poetry settings
alias poetry='/Users/youcef/.poetry/bin/poetry'

# default display settings
export BAT_THEME="Dracula"
# export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border --preview 'bat --color=always --style=header,grid --line-range :300 {}' --preview-window 'right:60%' --layout reverse --margin=1,4"
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border --layout reverse --margin=1,4"
alias fzfp="fzf --preview 'bat --color=always --style=header,grid --line-range :300 {}' --preview-window 'right:60%' --layout reverse --margin=1,4"

# source antigen package manager
source ~/antigen.zsh

# load zgen
source "${HOME}/.zgen/zgen.zsh"

# set autoload path
fpath=(~/zsh "${fpath[@]}")
# load custom commands
autoload -Uz bip bup bcp kp


source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/youcef/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/youcef/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/youcef/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/youcef/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

source ~/.zsh-autopair/autopair.zsh
autopair-init

# golang settings
export GOPATH=$HOME/projects
export GOROOT="$(brew --prefix golang)/libexec"
export PATH="$PATH:${GOPATH}/bin:${GOROOT}/bin"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/youcef/.sdkman"
[[ -s "/Users/youcef/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/youcef/.sdkman/bin/sdkman-init.sh"

[[ -s "/Users/youcef/.gvm/scripts/gvm" ]] && source "/Users/youcef/.gvm/scripts/gvm"

command -v vg >/dev/null 2>&1 && eval "$(vg eval --shell zsh)"
if which swiftenv > /dev/null; then eval "$(swiftenv init -)"; fi
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
