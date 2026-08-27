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
| `herdr/launchd/` | LaunchAgent du rafraîchisseur de métadonnées de la sidebar |
| `aerospace/` | Pavage de fenêtres, bindings Hyper |
| `doom/` | Emacs (Doom) — daemon via LaunchAgent |
| `nvim/` | Neovim (LazyVim) |
| `git/` | `gitconfig`, `gitignore_global` — delta, zdiff3, rerere |
| `workflow-tools/` | Scripts maison (`herdr-*`, `net`, `pk`, `mdv`, …) |
| `opencode/` | Agent CLI : agents, commandes, skills |
| `docs/` | Sources des PDF de référence (`docs/src/build`) |
| `zmk/` | Keymap du clavier Corne (42 touches, QWERTY) |
| `install/` | `Brewfile` par groupe + description des groupes |
| `lazygit/` `btop/` `broot/` `navi/` `yazi/` | TUIs |

## Installation

```sh
git clone https://github.com/cyuss/dotfiles.git ~/.config
cd ~/.config && ./install.sh
```

`install.sh` fait trois choses séparées, qu'on peut demander
indépendamment : **relier** les dotfiles depuis `$HOME`, **installer**
les paquets par groupes, et **annoncer** les étapes qui ne s'automatisent
pas bien. Rien n'est détruit : tout fichier existant est sauvegardé en
`<nom>.backup-<horodatage>`.

### Selon ce que tu veux en faire

| Situation | Commande |
|---|---|
| Je découvre le dépôt, je ne veux rien casser | `./install.sh --dry-run --all` |
| Je veux voir ce qui existe | `./install.sh --list` |
| Juste ma config shell sur un serveur | `./install.sh --links` |
| Machine neuve, je veux tout | `./install.sh --all --yes` |
| L'essentiel seulement | `./install.sh --recommended` |
| Je choisis à la main | `./install.sh` *(interactif)* |
| J'ai déjà mes outils, j'ajoute un groupe | `./install.sh --no-links --groups tui,data` |
| Qu'est-ce qui me manque ? | `./install.sh --check` |

Le mode interactif utilise [`gum`](https://github.com/charmbracelet/gum)
s'il est présent, et retombe sinon sur une invite numérotée — aucune
dépendance obligatoire.

### Groupes de paquets

Un `Brewfile` par groupe dans `install/brew/`, décrits dans
`install/groups.conf`. Les cinq premiers sont préselectionnés : sans eux,
les alias et la configuration zsh de ce dépôt ne fonctionnent pas.

| Groupe | Contenu |
|---|---|
| `core` ★ | git, fd, ripgrep, bat, eza, delta, fzf, zoxide, atuin, jq |
| `fonts` ★ | JetBrainsMono Nerd Font, Symbols, SF Pro |
| `shell` ★ | antidote, oh-my-posh, carapace, complétions |
| `term` ★ | Alacritty, herdr |
| `dev` ★ | uv, ruff, just, watchexec, direnv, gh, lazygit, jj |
| `editor` | Emacs (emacs-plus@30), Neovim |
| `wm` | AeroSpace, Karabiner-Elements |
| `tui` | btop, yazi, broot, navi, television, lazydocker, glow, gum |
| `search` | sd, serpl, ast-grep, semgrep |
| `data` | dasel, miller, gron, jless, yq, visidata, pgcli, litecli, lazysql |
| `net` | trippy, gping, xh, doggo, bandwhich, nmap |
| `prose` | languagetool, vale, typioca |

`--check` distingue « absent » de « installé hors brew » : une `.app`
téléchargée à la main n'est pas signalée comme manquante.

### Après le clonage

```sh
cp doom/private.el.example doom/private.el   # nom et e-mail, non versionnés
~/.config/emacs/bin/doom sync                # si Doom est installé
```

### Intégrations hors `~/.config` (optionnelles)

Deux réglages de la sidebar herdr vivent en dehors de ce dépôt, parce que
macOS et Claude Code les lisent ailleurs. Sans eux tout fonctionne, mais
les jetons correspondants restent vides.

**`$limit` et `$model`** — le quota et le modèle en cours n'existent que
dans le JSON que Claude Code passe à sa commande `statusLine`. Ajouter à
`~/.claude/settings.json` :

```json
"statusLine": {
  "type": "command",
  "command": "~/.config/workflow-tools/herdr-usagebar-statusline"
}
```

**`$sync`** — l'écart avec le remote (`↑2 ↓1 ⚑3`) sur les lignes de space :

```sh
cp herdr/launchd/com.youcef.herdr-space-metadata.plist ~/Library/LaunchAgents/
launchctl bootstrap gui/$UID ~/Library/LaunchAgents/com.youcef.herdr-space-metadata.plist
```

`launchctl` n'interprète pas `~` : ce `.plist` est le seul fichier du dépôt
qui code un chemin absolu en dur, à adapter si tu n'es pas sur ce compte.

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
