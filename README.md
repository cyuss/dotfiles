# dotfiles

Configuration macOS / Apple Silicon, versionnée dans `~/.config`.

Terminal Alacritty, multiplexeur [herdr](https://github.com/cloudmanic/herdr),
pavage AeroSpace, Emacs (Doom) en daemon, zsh.

## Contenu

| Dossier | Rôle |
|---|---|
| `zsh/` | Shell : `.zshenv` (PATH), `.zshrc` (interactif), alias, complétion |
| `alacritty/` | Terminal — thème `premium-noir`, hints, bindings ⌘ |
| `herdr/` | Multiplexeur / workspace manager, prefix `C-b` |
| `aerospace/` | Pavage de fenêtres, bindings Hyper |
| `doom/` | Emacs (Doom) — daemon via LaunchAgent |
| `nvim/` | Neovim (LazyVim) |
| `git/` | `gitconfig`, `gitignore_global` — delta, zdiff3, rerere |
| `workflow-tools/` | Scripts maison (`herdr-*`, `net`, `pk`, `mdv`, …) |
| `opencode/` | Agent CLI : agents, commandes, skills |
| `docs/` | Sources des PDF de référence (`docs/src/build`) |
| `lazygit/` `btop/` `broot/` `navi/` `yazi/` | TUIs |

## Installation

```sh
git clone https://github.com/cyuss/dotfiles.git ~/.config
~/.config/install.sh
```

`install.sh` crée les liens symboliques depuis `$HOME` vers ce dépôt
(`~/.zshrc`, `~/.zshenv`, `~/.zprofile`, `~/.gitconfig`, …). Il ne
touche à rien d'autre et sauvegarde tout fichier existant.

### Après le clonage

```sh
cp doom/private.el.example doom/private.el   # nom et e-mail, non versionnés
~/.config/emacs/bin/doom sync                # si Doom est installé
```

## Le modèle du `.gitignore`

Tout est ignoré par défaut, puis réautorisé dossier par dossier :

```gitignore
*
!.gitignore
!zsh/
!zsh/**
…
```

C'est le seul modèle sûr pour un dossier où des outils tiers écrivent en
permanence : un nouvel outil qui pose un fichier dans `~/.config` n'est
jamais committé par accident — donc jamais un token publié par mégarde.

Sont explicitement exclus, en plus : `doom/private.el` (identité), les
fichiers de projets herdr (générés, ils listent les dépôts locaux) et les
PDF de `docs/` (artefacts régénérables).

## Principes

**Rien de coûteux au démarrage du shell.** Les gestionnaires de versions
(pyenv, nvm, jenv, chruby) sont chargés au premier usage. Démarrage zsh
mesuré : ~130 ms.

**Mesurer avant de régler.** Les commentaires de ce dépôt portent les
chiffres qui justifient chaque choix, et les hypothèses réfutées en
chemin.

**Un fichier de config est de la documentation.** Les commentaires
expliquent *pourquoi*, pas *quoi*.
