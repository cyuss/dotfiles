# Journal des modifications

Ce dépôt n'a pas de versions : c'est une configuration vivante. Les
entrées sont donc datées, les plus récentes en tête. Chaque commit porte
le détail et la mesure ; ce fichier donne la vue d'ensemble.

Le format s'inspire de [Keep a Changelog](https://keepachangelog.com/fr/1.1.0/).

---

## 2026-08-31 — Ménage

### Retiré
- Les HTML intermédiaires de `docs/src/` (`<nom>-body.html`,
  `<nom>-cover.html`) sortent du suivi. Ce sont des artefacts, réécrits par
  `<nom>_doc.py` à chaque `./build`, au même titre que les PDF déjà exclus.
  Minifiés sur une seule ligne, ils produisaient un diff pleine page à
  chaque régénération pour un simple changement de date.
- Au passage, `workflow-body.html` et `workflow-cover.html` : leur
  générateur est archivé depuis longtemps, le dépôt annonçait donc un
  document que `./build` ne sait plus construire.
- Caches Python orphelins sur le disque (`docs/src/__pycache__`,
  `workflow-tools/cheatsheets/` qui ne contenait plus que du bytecode d'un
  module supprimé).

### Ajouté
- Ce journal, et un sommaire de la documentation dans le `README`.

### Corrigé
- Le `README` affirmait que le `.plist` du LaunchAgent était le seul
  fichier à coder un chemin absolu en dur. C'est faux : ils sont **six**,
  et le navigator herdr pointe dans le vide sur un autre compte. La liste
  et la commande de reprise remplacent l'affirmation.

---

## 2026-08-27 — herdr : la sidebar devient lisible

Une passe complète sur l'interface du multiplexeur : ce qu'on voit, dans
quel ordre, et avec quelles couleurs.

### Ajouté
- **Panneau des agents** repensé en trois lignes : emplacement, agent et
  modèle, puis budget.

  ```
  ◐ [1] ~
  claude  Opus 5
  ⛁ 31% (311k)  5h 54%
  ```

  Une couleur par nature d'information — pêche pour l'identité, teal pour
  l'agent, mauve pour le modèle, et surtout ambre/sauge pour le budget :
  chaud = ce qui part, froid = ce qui reste, donc le rapport se lit sans
  lire les chiffres.
- **Jeton `$sync`** sur les lignes de space : l'écart avec l'amont, que
  rien n'affichait — `git_status` couvre l'arbre de travail, pas les
  commits oubliés avant un push.

  ```
  ↑2 d'avance   ↓1 de retard   ⚑3 stashes
  ```

  Publié par `workflow-tools/herdr-space-metadata` via un LaunchAgent. Le
  TTL vaut 4× l'intervalle : les jetons s'effacent seuls si le démon meurt,
  plutôt que d'afficher indéfiniment une valeur fausse.
- **Jeton `$model`**, le modèle Claude en cours. Il n'est porté par aucun
  jeton herdr — il n'existe que dans le JSON que Claude Code passe à sa
  commande `statusLine`.
- `status_indicators = "symbols"` : la forme porte l'état d'un agent, la
  couleur ne fait que la renforcer. En `"dots"`, l'information la plus
  critique de l'interface reposait sur le canal le plus fragile — thème
  quasi monochrome, opacité 0.55 et flou dégradent chacun la
  discrimination des couleurs.
- Indicateur de zoom dans la barre d'onglets : rien ne signalait qu'un
  pane était zoomé.
- Titre de fenêtre `{workspace} · {terminal_title}`, qui fait remonter la
  tâche de l'agent dans Mission Control.

### Modifié
- Sidebar élargie de 28 à 40 colonnes. Le terminal en mesure 155 : elle
  n'en prenait que 18 %, elle en prend 26 % et laisse ~113 colonnes au
  contenu.
- `pane_outer_borders = false` — ce cadre est redondant avec le bord du
  terminal et la sidebar.
- Palette : `yellow` et `green` remontés en intensité, `sidebar_bg` en
  `"reset"` par cohérence avec `panel_bg` (elle restait opaque contre un
  terminal à 0.55).

### Corrigé
- **`$limit` restait vide.** `usagebar` n'a aucun moyen de connaître les
  fenêtres de quota Claude tout seul ; la donnée n'existe que dans le
  `stdin` de la `statusLine`. `herdr-usagebar-statusline` s'y branche,
  publie `$model` et `$limit`, puis transmet le JSON **intact** au script
  du plugin.
- **Doublon dans le panneau des agents** : le plugin `automatic-rename`
  renomme l'onglet d'après le titre du terminal, donc la même phrase
  sortait en ligne 2 et en ligne 3.
- **Panneau tout gris** : `dim = true` écrasait la teinte. La
  documentation herdr précise qu'un champ de style omis « préserve le
  défaut contextuel » — `dim = false` doit donc être écrit franchement.
- Quatre `.pyc` étaient versionnés. Ils embarquent le chemin absolu du
  source, donc une information de compte qui n'a rien à faire là. Ils
  passaient à cause des allowlists du `.gitignore` : les règles
  d'exclusion doivent être posées **après** elles, la dernière règle qui
  matche l'emportant.

### Suspendu
Le préfixe `Ctrl+b` a cessé de répondre en cours de journée. Cause non
identifiée : restauration d'abord, isolation ensuite. Sont neutralisés en
commentaire dans `herdr/config.toml`, tous marqués `[SUSPENDED 2026-08-27]` :

| Réglage | Touche |
|---|---|
| `swap_pane_up/left/down/right` | `prefix+shift+i/j/k/l` |
| `move_tab_previous` / `move_tab_next` | `prefix+<` / `prefix+>` |
| `edit_scrollback` | `prefix+e` |
| `usagebar.refresh` | `prefix+shift+u` |
| `experimental.pane_history` | — |

Le démon `herdr-space-metadata` est arrêté pour la même raison, ce qui
laisse la 3ᵉ ligne des spaces vide. Ont été éliminés de l'enquête : aucune
ligne de raccourci supprimée par erreur, `config check` valide toutes les
graphies, herdr protège son propre préfixe, aucun plugin ne déclare
`ctrl+b`, aucune erreur de keymap dans les journaux.

### Retiré
- Le hook `UserPromptSubmit` et `workflow-tools/herdr-agent-topic`.
  Constat vérifié : `sessionTitle` ne met **pas** à jour `terminal_title`,
  il ne touche que le titre interne de Claude Code. On ne garde pas un
  hook qui tourne à chaque message pour rien.

---

## 2026-08-23 — Socle initial

Première mise en ligne du poste, en commits thématiques.

### Ajouté
| Domaine | Contenu |
|---|---|
| `zsh/` | Shell, `PATH`, alias, complétion. Démarrage mesuré ~130 ms |
| `git/` | `gitconfig` global — delta, zdiff3, rerere |
| `alacritty/` | Terminal, thème `premium-noir`, hints |
| `herdr/` | Multiplexeur, préfixe `C-b`, plugins et quick actions |
| `aerospace/` | Pavage de fenêtres, bindings Hyper, 9 workspaces |
| `doom/` | Emacs (Doom) en daemon |
| `nvim/` | Neovim (LazyVim) |
| `opencode/` | Agents, commandes et skills |
| `lazygit/` `btop/` `broot/` `navi/` `yazi/` | TUIs |
| `docs/` | Sources des quatre références PDF |
| `install/` | Installateur par groupes, avec modes |

Trois outils maison notables :

- **`mdv`** — lecture Markdown en terminal, avec sommaire et aperçu vif.
- **`prose`** — relecture grammaticale française.
- **`typing-report`** — analyse de frappe confrontée au keymap réel du
  clavier Corne, plutôt qu'à un clavier théorique.
