# Correctifs locaux sur des plugins herdr

## navigator-tab-children.patch

Trois corrections dans un seul correctif.

Ajoute au plugin `herdr-navigator` la **vue arborescente** : les tabs
d'un space apparaissent comme lignes enfant dans la source `open`,
comme `choose-tree` dans tmux.

```
[3] project: mon-garage-auto        2 tabs · 3 panes
  ├ agent                           tab 1
  └ run                             tab 4 · 2 panes
```

### Pourquoi un correctif et pas une configuration

Le plugin expose `[[integrations]]` pour ajouter des sources, mais le
champ `kind` d'une entree d'integration n'est interprete que pour trois
valeurs (`src/integrations/command.rs`) :

| kind | source |
|---|---|
| `server`, `remote-terminal` | `Source::Server` |
| `session` | `Source::Session` |
| tout le reste | `Source::Integration` |

Aucun chemin ne mene a `Source::Workspace`, c'est-a-dire a la section
`open`. Une integration ne peut donc pas poser de lignes sous les spaces.
La documentation du plugin (`docs/plugin-integrations.md`) dit meme
« Navigator does not interpret it », ce qui est inexact.

Le plugin lui-meme indique la marche a suivre : *« Use a built-in adapter
when opening needs Herdr-specific behavior. »*

### Ce que le correctif touche

| Fichier | Modification |
|---|---|
| `src/model.rs` | champ `tab_id: Option<String>` sur `Entry` — distingue une ligne de tab d'une ligne de space |
| `src/config.rs` | reglages `picker.tab_children` (defaut `true`) et `picker.tab_children_min` (defaut `2`) |
| `src/sources.rs` | `herdr tab list` ; lignes enfant inserees juste apres leur space ; `tab_entry()` et `strip_index_prefix()` ; 4 tests |

### Numerotation des tabs : position, pas compteur

Le champ `number` d'un tab est un compteur de CREATION : il suit le
`tab_id` (« wC:t4 » -> 4) et garde les trous laisses par les tabs fermes.
herdr, lui, numerote la barre d'onglets par POSITION. Un space dont on a
ferme un tab affiche donc « [2] » sur un tab dont `number` vaut 4.

La ligne enfant affiche la POSITION. Afficher `number` etait trompeur a
deux titres : ca ne correspondait pas a la barre d'onglets, et
`switch_tab = "prefix+1..9"` est une liaison INDEXEE, qui vise elle aussi
la position — lire « tab 4 » et taper prefix+4 ne menait nulle part.

Couvert par `child_rows_number_by_position_not_by_creation_counter`.
| `src/app.rs` | passage des reglages ; **Ctrl-X sur une ligne enfant ferme le tab**, pas le space |
| `src/tui.rs` | cadre propre au plugin rendu optionnel (`picker.own_frame`, defaut `false`) |
| `src/main.rs` | **auto-reparation de `prefix+w`** : un pane picker perime est ferme puis rouvert |

### 2. Le cadre en double

herdr encadre deja le pane du plugin et y inscrit son libelle
« Herdr Navigator ». Le plugin redessinait par-dessus son propre cadre
titre du meme nom : deux boites imbriquees, dont l'interieure
n'apportait rien. Le cadre du plugin est desormais eteint par defaut.
Le remettre : `own_frame = true` dans `[picker]` — utile seulement si
`ui.pane_borders = false` cote herdr.

### 3. `prefix+w` inerte

Symptome : apres avoir lance un processus long dans un pane, `prefix+w`
ne fait plus rien et il faut la souris pour se deplacer.

Cause : quand le processus `herdr-navigator ui` meurt sans que son pane
soit ferme, herdr laisse un SHELL dans ce pane — mais le pane garde son
libelle « Herdr Navigator ». `picker_pane_decision` le retrouvait alors
indefiniment et focalisait ce shell mort au lieu d'ouvrir le picker.

Correction : avant de focaliser, on verifie via
`herdr pane process-info --pane <id>` qu'un processus `herdr-navigator`
est bien au premier plan. Sinon le pane est perime : on le ferme
(`herdr pane close`, et non `herdr plugin pane close` qui repond
`plugin_pane_not_found` des que le processus du plugin est mort) et on
rouvre un picker neuf.

Sans reponse du serveur, on garde le comportement d'origine plutot que
de fermer un pane peut-etre vivant.

L'ordre de l'arbre tient parce que le tri du picker (`app::apply_filter`)
retombe sur l'ordre d'insertion a score egal.

### Etat des tests

`cargo test` : **99 reussis, 3 echecs**. Les 3 echecs sont **anterieurs**
au correctif — verifie en comparant avec le checkout d'origine, memes
trois noms :

- `app::tests::close_target_matches_entry_kind`
- `app::tests::source_specific_reuse_distinguishes_same_path_workspaces`
- `sources::tests::persisted_workspace_kind_survives_label_changes_and_legacy_labels_migrate`

### Reappliquer apres une mise a jour du plugin

Une mise a jour ecrase le checkout **et le binaire**. Il faut refaire :

```
cd ~/.config/herdr/plugins/github/herdr-navigator-*
git apply ~/.config/herdr/patches/navigator-tab-children.patch
cargo build --release
herdr server reload-config
```

Si le correctif ne s'applique plus (le code amont a bouge), le repli est
`tab_children` absent : le plugin d'origine fonctionne normalement, sans
l'arborescence. Rien ne casse.

Base du correctif : `b94ef1e`.
