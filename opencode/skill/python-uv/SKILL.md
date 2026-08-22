---
name: python-uv
description: Projets Python. À utiliser dès qu'un pyproject.toml, requirements.txt, .py, setup.py, pytest.ini ou tox.ini est présent. Couvre uv, ruff, pyright, pytest, la gestion de venv et les pièges d'empaquetage.
---

# Python — chaîne uv

## Outils de cette machine

`uv` (paquets + venv) · `ruff` (lint **et** format) · `pyright` (types) ·
`pytest` (tests) · `pyenv` (versions d'interpréteur).

Pas de poetry ni de conda pour un nouveau projet : `uv` remplace les deux.

## Commandes

| Besoin | Commande |
|---|---|
| Créer le projet | `uv init` |
| Ajouter une dépendance | `uv add <pkg>` (jamais `pip install`) |
| Dépendance de dev | `uv add --dev <pkg>` |
| Lancer quelque chose | `uv run <cmd>` — pas besoin d'activer le venv |
| Synchroniser | `uv sync` |
| Tests | `uv run pytest -q` |
| Lint + format | `uv run ruff check --fix . && uv run ruff format .` |
| Types | `uv run pyright` |

`uv run` résout le venv seul. Ne jamais écrire `source .venv/bin/activate`
dans un script ou une commande que tu proposes.

## Règles

- **N'édite jamais `uv.lock` à la main.** Il se régénère avec `uv lock`.
- Les dépendances vivent dans `pyproject.toml`, pas dans un `requirements.txt`
  écrit à la main. Si le projet en a un legacy, propose la migration mais ne
  la fais pas sans qu'on te le demande.
- Annotations de types sur toute fonction publique. `Any` doit être justifié.
- Pas d'`except:` nu ni d'`except Exception: pass`. Attrape le type précis.
- Chemins : `pathlib.Path`, pas de concaténation de chaînes.
- Pour un CLI : `argparse` (stdlib) ou `typer` si déjà présent — n'ajoute pas
  une dépendance pour trois arguments.

## Tests

- `pytest`, pas `unittest`, sauf si le projet utilise déjà `unittest`.
- Un test par comportement, nommé d'après ce qu'il vérifie —
  `test_rejette_un_email_sans_arobase`, pas `test_1`.
- Correction de bug = test qui **échoue avant** le correctif. Écris-le,
  fais-le échouer, corrige, montre-le passer.
- `pytest.raises` pour les erreurs attendues, avec `match=` sur le message.
- Fixtures dans `conftest.py`. Pas de fixture qui fait plus d'une chose.

## Pièges fréquents

- Argument par défaut mutable (`def f(x=[])`) — utilise `None` puis initialise.
- `datetime.now()` sans timezone dans du code qui persiste des dates.
- Ouvrir un fichier sans `with`.
- `os.environ["X"]` qui lève un `KeyError` en prod — préfère
  `os.environ.get("X")` avec une valeur par défaut explicite, ou échoue tôt
  avec un message clair au démarrage.
