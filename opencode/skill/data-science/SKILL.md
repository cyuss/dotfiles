---
name: data-science
description: Analyse de données, notebooks, machine learning. À utiliser dès qu'un .ipynb, pandas, numpy, scikit-learn, polars, torch ou un dossier data/ est présent.
---

# Data science

## Reproductibilité — la règle qui prime sur tout

Un résultat qu'on ne peut pas rejouer n'est pas un résultat.

- Seed fixée partout : `numpy`, `random`, le framework ML, et le split.
- Versions des dépendances figées (`uv.lock`).
- Aucun chemin absolu. `Path(__file__).parent` ou une racine configurable.
- Les données brutes ne sont **jamais** modifiées en place. Lecture seule,
  transformations dans un nouveau fichier.

## Notebooks

- Le notebook explore, il ne produit pas. Dès qu'une fonction marche, elle
  part dans un `.py` importable — sinon elle n'est ni testable ni réutilisable.
- Un notebook doit tourner **de haut en bas après un restart du kernel**.
  Si ce n'est pas le cas, il est faux : l'état caché ment.
- Pas de sorties volumineuses committées (`nbstripout` si le repo le prévoit).

## pandas — les pièges qui donnent des résultats faux

- **`SettingWithCopyWarning` n'est jamais à ignorer.** Utilise `.loc` ou
  `.copy()` explicite. C'est un des rares warnings qui signale un vrai bug.
- `inplace=True` : évite. Peu lisible et souvent pas plus rapide.
- `merge` : vérifie la taille **avant et après**. Une jointure qui multiplie
  les lignes est le bug silencieux le plus courant de l'analyse de données.
- `df.apply(axis=1)` est lent : cherche la version vectorisée d'abord.
- Vérifie les `dtype` après un `read_csv` — une colonne d'ID lue en float
  perd sa précision.

## Machine learning

- Split **avant** toute transformation ajustée sur les données. Normaliser
  avant de splitter, c'est de la fuite de données, et le score obtenu est
  faux.
- Baseline triviale d'abord (classe majoritaire, moyenne). Un modèle qui ne
  la bat pas ne sert à rien.
- Rapporte la métrique qui correspond au problème : l'accuracy sur des
  classes déséquilibrées ne veut rien dire.
- Toujours indiquer sur quel jeu une métrique a été mesurée.
