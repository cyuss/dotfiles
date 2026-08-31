# Documentation du poste

Quatre références PDF, une par outil. Chacune décrit la configuration
**telle qu'elle est réellement installée**, avec la raison de chaque
réglage — pas la documentation générique de l'outil.

| Fichier | Sujet | Pages |
|---|---|---|
| `doom-reference.pdf` | Doom Emacs — daemon, Evil, eglot/basedpyright, dape, avy, combobulate | 16 |
| `herdr-reference.pdf` | herdr — spaces, panes, navigator, agents, 21 plugins | 10 |
| `aerospace-reference.pdf` | AeroSpace — arbre de tuiles, Hyper, 9 workspaces, 4 modes | 9 |
| `terminal-reference.pdf` | Terminal & zsh — complétion, fzf, atuin, alias, TUIs | 11 |

Notes Markdown complémentaires : `herdr.md`, `aerospace.md`,
`aerospace-workflow.md`, `aerospace-workflow-full.md`, `doom-snippets.md`.

Les lire en terminal : `mdv ~/.config/docs` — ou `Ctrl+b Alt+m` depuis herdr.

## Régénérer

```
cd ~/.config/docs/src
./build              # les quatre
./build doom         # un seul : doom | herdr | aerospace | terminal
```

Chaîne : Python → HTML → Chrome (CDP `Page.printToPDF`) → `pdfunite`.
La couverture est rendue sans pied de page, le corps avec numérotation ;
les deux sont recollés pour que la pagination commence à la section 01.

## Structure de `src/`

| Fichier | Rôle |
|---|---|
| `lib.py` | Le système de design : CSS, gabarits de colonnes, composants |
| `<nom>_doc.py` | Le contenu d'un document. Produit `<nom>-cover.html` et `<nom>-body.html` |
| `topdf.mjs` | HTML → PDF via Chrome DevTools Protocol |
| `build` | Orchestration |
| `archive/` | Générateurs de l'ancienne série (opencode, workflow), non construits |

Ni les PDF ni les HTML intermédiaires ne sont versionnés : `./build` les
reconstruit. Après un clone, `docs/` ne contient donc que du Markdown
jusqu'au premier `build`.

### Deux règles de mise en page à connaître

**Largeurs de colonnes fixes.** Tous les tableaux utilisent
`table-layout: fixed` avec un `<colgroup>` explicite, tiré des gabarits
`W_*` de `lib.py`. C'est ce qui fait que les touches et les descriptions
démarrent au même x d'un bout à l'autre du document.

**Deux colonnes = un tableau, jamais une grille.** `keys_pair()`,
`cmd_pair()` et `mixed_pair()` produisent un seul tableau à quatre
colonnes. Une grille CSS ne se coupe pas entre deux pages (le bloc saute
entier et laisse une demi-page blanche) et `column-count` reprend en
colonne 1 de la page suivante (la colonne 2 reste vide). Chrome, lui,
coupe une ligne de tableau et fait reprendre chaque cellule dans **sa**
colonne. C'est le seul comportement correct des trois.
