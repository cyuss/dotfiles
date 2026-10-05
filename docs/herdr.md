# herdr — carte de référence

Préfixe : `Ctrl+B`. On le presse, on le relâche, puis on presse la touche.
Seule la navigation entre panes s'en passe (`Alt+H/J/K/L`).

> Cette carte reflète `~/.config/herdr/config.toml` — pas les défauts herdr.
> Le schéma maison : **tout en minuscules**, aucun Shift dans les gestes courants.

---

## Modèle mental

- **space** → un projet (branche git + statut, c'est la ligne dans la sidebar)
- **tab** → une tâche dans ce projet
- **pane** → un terminal réel, avec ou sans agent

L'état de l'agent est la donnée : *travaille* (laisse-le) / *attend* (décision) / *fini* (notif macOS).
Le serveur survit au terminal : `Ctrl+B q` détache, `herdr` réattache, les agents continuent.

---

## Panes — navigation

| Touche | Action |
|---|---|
| `Alt+H/J/K/L` | Se déplacer — traverse splits nvim ET panes herdr. **Sans préfixe.** Le geste de tous les jours. |
| `Ctrl+B` `i/j/k/l` | Se déplacer façon AeroSpace (`i` haut, `j` gauche, `k` bas, `l` droite) |
| `Ctrl+B` `` ` `` | Pane précédent (aller-retour) |
| `Ctrl+B` `Tab` | Cycler les panes |
| `Ctrl+B` `Alt+1..9` | Sauter sur l'agent n° N |

## Panes — disposition

| Touche | Action |
|---|---|
| `Ctrl+B` `v` | Split vertical (`:vs`) |
| `Ctrl+B` `s` | Split horizontal (`:sp`) |
| `Ctrl+B` `z` | Zoom plein écran |
| `Ctrl+B` `r` | Mode redimensionnement |
| `Ctrl+B` `x` | Fermer le pane |
| `Ctrl+B` `Shift+P` | Renommer le pane |
| `Ctrl+B` `e` | Ouvrir le scrollback dans nvim |

## Onglets

| Touche | Action |
|---|---|
| `Ctrl+B` `c` | Nouvel onglet (nommé automatiquement) |
| `Ctrl+B` `1..9` | Aller à l'onglet N |
| `Ctrl+B` `,` / `.` | Précédent / suivant (`<` et `>` sur les mêmes touches) |
| `Ctrl+B` `;` | **Renommer** (fige le nom auto) |
| `Ctrl+B` `Shift+X` | Fermer |

## Spaces & worktrees

| Touche | Action |
|---|---|
| `Ctrl+B` `n` | Nouveau space |
| `Ctrl+B` `Shift+G` | Nouveau worktree git (space enfant, branche isolée) |
| `Ctrl+B` `Shift+1..9` | Aller au space N |
| `Ctrl+B` `[` / `]` | Précédent / suivant, sans picker |
| `Ctrl+B` `g` | Sélecteur de space (picker flou) |
| `Ctrl+B` `Shift+W` | Mode *goto* — hjkl partout |
| `Ctrl+B` `/` | **Renommer** |
| `Ctrl+B` `'` | Fermer (confirmation) |

## Agents

| Touche | Action |
|---|---|
| `Ctrl+B` `Alt+,` / `Alt+.` | Agent précédent / suivant |
| `Ctrl+B` `Alt+1..9` | Sauter sur l'agent n° N |
| `Ctrl+B` `o` | Sauter au pane qui vient de notifier |

## Plugins

| Touche | Plugin |
|---|---|
| `Ctrl+B` `a` | **Palette** — toutes les actions, en fuzzy |
| `Ctrl+B` `f` | **Fichiers** — arborescence git-aware en split |
| `Ctrl+B` `y` | **yazi** — explorateur de fichiers |
| `Ctrl+B` `d` | **Review** — le diff de l'agent, commentable |
| `Ctrl+B` `m` | **Memex** — chercher dans les conversations passées |
| `Ctrl+B` `t` / `u` | **Termscope** — fichier / lien visible à l'écran |
| `Ctrl+B` `w` | **Navigator** — sauter vers n'importe quoi (`Alt+W` en pane latéral, `Alt+I` pour revenir) |
| `Ctrl+B` `h` | **Quotidien** — le space hors-projet (dépôts, système, fichiers, màj) |
| `Ctrl+B` `p` | **Projects** — monter un workspace complet depuis un template |
| `Ctrl+B` `Alt+A` | **Quick actions** — lanceur flou dans le répertoire courant |
| `Ctrl+B` `Alt+Z` | **zoxide** — répertoire fréquent → workspace |
| `Ctrl+B` `Alt+V` | **lazygit** en pane latéral |
| `Ctrl+B` `Alt+U` | **Usage** — contexte et limites de taux des agents |
| `Ctrl+B` `Alt+T` | **Tokens** — dépense en direct |
| `Ctrl+B` `Alt+L` | **llmtrim** — économie de contexte |
| `Ctrl+B` `Alt+P` | **Gestionnaire de plugins** |
| `Ctrl+B` `Alt+R` | **claude-auto-retry** — statut |

## Popups outils

| Touche | Outil |
|---|---|
| `Ctrl+B` `Alt+G` | lazygit |
| `Ctrl+B` `Alt+D` | lazydocker |
| `Ctrl+B` `Alt+B` | btop |
| `Ctrl+B` `Alt+K` | **pk** — chercher un process par son nom et le tuer |

## Session

| Touche | Action |
|---|---|
| `Ctrl+B` `?` | Aide — tous les bindings actifs |
| `Ctrl+B` `b` | Replier / déplier la sidebar |
| `Ctrl+B` `Shift+R` | Recharger config.toml |
| `Ctrl+B` `q` | Détacher (les agents continuent) |

> **Réglages** : `Ctrl+B s` est pris par le split horizontal. Passer par la
> palette (`Ctrl+B a` → « settings »).

---

## Templates de workspace (l'équivalent tmuxinator)

| tmuxinator | herdr |
|---|---|
| `~/.config/tmuxinator/*.yml` | `~/.config/herdr/plugins/config/cloudmanic.herdr-plus/projects/*.toml` |
| `mux start <projet>` | `Ctrl+B p` → fuzzy → `Enter` |
| `windows:` | `[[tabs]]` |
| panes d'une window | `[[tabs.panes]]` (4 max, `split = "down"` / `"right"`) |
| `root:` | `working_dir` |
| — | `group` : regroupe les projets sous un intertitre dans le picker |

Un fichier = un projet. Le nom du fichier n'a aucune importance, seul le contenu compte.

```toml
name = "my-app"
description = "…"
group = "python"
working_dir = "~/Desktop/projects/my-app"

[[tabs]]
name = "agent"
command = "opencode"

[[tabs]]
name = "server"

[[tabs.panes]]
command = "make run"

[[tabs.panes]]
command = "make logs"
split = "right"
```

**Créer depuis le navigator** — `Ctrl+B w` puis `Ctrl+N` :
`⚡ scratch` monte un space jetable (`~/scratch/<horodatage>`, shell + opencode, aucun
nom à taper) · `+ space ici` en monte un sur le répertoire du pane focalisé, recalculé
à chaque ouverture · `+ session <nom>` ouvre une **nouvelle fenêtre Alacritty** sur une
session nommée, et apparaît sous `Ctrl+L` à côté des sessions existantes.
Présets : `~/.config/workflow-tools/herdr-session-presets`.

**Le space hors-projet** — `Ctrl+B h` ouvre `00-quotidien.toml` : un shell nu, l'état
de **tous** les dépôts (`repos-status`), btop + un shell, yazi, et `brew outdated` +
`herdr integration status`. Le raccourci passe par `herdr-open-project`, qui **focalise**
le space s'il existe déjà — `herdr-plus open` en créerait un second à chaque appui.

**Génération automatique** — `herdr-sync-projects` écrit un template par dépôt git
de `~/Desktop/projects`, avec un tab `run` adapté à la stack détectée (make /
compose / node / …) :

```bash
herdr-sync-projects --dry-run   # montre sans écrire
herdr-sync-projects             # synchronise
```

Il ne touche **que** les fichiers portant son en-tête `GENERATED-BY`. Retirer cette
ligne fige le template : il devient à toi, la synchro le laisse tranquille.
(C'est le cas de `00-config.toml`, le template des dotfiles.)

**Worktrees** — même schéma de tabs, appliqué automatiquement à la création d'un
worktree (`Ctrl+B Shift+G`) : `~/.config/herdr-plus/worktrees/*.toml`, avec un
champ `repo` (obligatoire, insensible à la casse) et un `branch` optionnel.

---

## Renommer

| Quoi | Touche | CLI |
|---|---|---|
| pane | `Ctrl+B Shift+P` | — |
| tab | `Ctrl+B ;` | `herdr tab rename w1:t1 "nom"` |
| space | `Ctrl+B /` | `herdr workspace rename w1 "nom"` |
| session | — | `herdr-rename-session <ancien> <nouveau>` |

Renommer un tab **fige** son nom : le plugin `herdr-automatic-rename` cesse de le
piloter. Pour lui rendre la main : palette → « Reset tab to automatic naming ».

Les sessions nommées n'ont pas de commande `rename` : une session **est** un dossier
`~/.config/herdr/sessions/<nom>/`. Le script `herdr-rename-session` arrête le
serveur, déplace le dossier, et rappelle la commande de réattache. La session
`default` (`~/.config/herdr`, celle qu'ouvre `herdr` tout court) n'a pas de nom.

```bash
herdr session list                  # nom, statut, dossier, socket
herdr --session perso               # ouvre / réattache un univers parallèle
herdr-rename-session perso client   # arrête, renomme
herdr session delete <nom>          # une fois arrêtée
```

---

## Touches internes des plugins

**File viewer** (`Ctrl+B f`)
`f` chercher · `]` `[` fichier modifié suiv./préc. · `v` changer de vue · `p` épingler ·
`a` annoter · `L` copier `path:ligne` · `b` base du diff · `W` autre worktree ·
`e` éditer dans nvim · `Z` plein écran · `?` aide

**Reviewr** (`Ctrl+B d`)
`1` `2` `3` onglets · `u` `b` `t` portée (non-commité / branche / dernier tour) ·
`]` `[` hunk suiv./préc. · `v` sélectionner des lignes · `c` commenter ·
`l` lister · **`s` envoyer les commentaires à l'agent** · `?` aide · `q` quitter

**Navigator** (`Ctrl+B w`)
`Ctrl+W` **W**orkspaces · `Ctrl+A` **A**gents · `Ctrl+L` sessions ·
`Ctrl+G` **G**ateways (serveurs distants) · `Ctrl+N` **N**ouveau · `Ctrl+P` **P**rojets ·
`Tab` cycler les filtres · `Ctrl+U` tout effacer · `Enter` ouvrir · `Ctrl+X` fermer le space ·
`Ctrl+B` marquer · `Ctrl+O` preview · `?` aide · `!nom` agent, `@statut` statut, `/chemin` cwd

> **Ctrl+S est interdit ici** : `stty` montre `ixon` actif et `stop = ^S` — c'est
> le XOFF du contrôle de flux. Il gèle la sortie du terminal, on tape et rien ne
> s'affiche (`Ctrl+Q` dégèle). Les sessions sont donc sur `Ctrl+L`, le défaut du
> plugin. Même raison pour `^Q`, `^Z`, `^C`, `^D`, `^H`.

> Trois étages, dans cet ordre : ce qui **existe** (spaces, agents, sessions,
> serveurs) · ce qui se **crée** · ce qui n'est qu'un **modèle** (projets).
> Sources zoxide, roots et quick actions désactivées dans
> `plugins/config/herdr-navigator/config.toml` : chacune a déjà sa touche
> (`Alt+Z`, `p`, `Alt+A`) et elles noyaient le reste.

**Memex** (`Ctrl+B m`)
`memex search "terme"` · `--semantic` concepts flous · `--hybrid` les deux ·
`memex usage --source claude` pour les coûts

**Automatic rename** — aucune touche, tourne en fond.
Désactiver la numérotation `[2] api` : `AUTO_INDEX=0` dans
`~/.config/herdr-automatic-rename/config.sh`

---

## Workflow optimal

La boucle, à partir de 3 agents :

1. `Ctrl+B Shift+G` — un worktree par tâche, branches isolées, zéro conflit
2. Brief les trois, puis **pars**. Ne reste pas à regarder.
3. Notification → `Ctrl+B o` saute sur le pane concerné
4. `Ctrl+B d` — lis le diff, commente, `s` renvoie à l'agent.
   Ne lis jamais le code dans le chat.
5. `Ctrl+B Alt+G` — lazygit, commit, retour. L'agent suivant t'attend.

Réflexes :
- En cas de doute sur un raccourci → `Ctrl+B a` (palette). Toujours.
- Démarrer un projet par `Ctrl+B p`, revenir à la base par `Ctrl+B h`.
- Détacher (`Ctrl+B q`) au lieu de fermer.
- Viser l'agent par son numéro (`Ctrl+B Alt+1`) plutôt que naviguer pane par pane.
- Sortie trop longue → `Ctrl+B e` l'envoie dans nvim au lieu de scroller.

**La règle** : un agent en train de travailler est un agent qu'on ne regarde pas.
Ton temps se dépense à la review, pas à l'attente.

---

## Entretien

```bash
herdr status                    # client + serveur
herdr integration status        # les agents sont-ils branchés
herdr plugin list               # les plugins installés
herdr config check              # valider config.toml avant de recharger
herdr server reload-config      # ou Ctrl+B Shift+R

herdr update                    # herdr lui-même
herdr plugin install owner/repo # réinstaller = mettre à jour

herdr server stop && herdr      # si ça se coince (session restaurée)
tail -f ~/.config/herdr/herdr-server.log
```

Les intégrations sont versionnées (v7, v9…). Après une mise à jour de herdr,
si `integration status` dit *outdated* → relancer `herdr integration install <agent>`.
