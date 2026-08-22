# Snippets privés — `$DOOMDIR/snippets`

`TAB` après la clé (evil insert), ou `M-x yas-insert-snippet` (`SPC i s`) pour
parcourir. Ces snippets **priment** sur ceux de `doom-snippets` : aucune clé ci-
dessous n'entre en collision avec les leurs (`def`, `cl`, `try`, `main`, `np`…).

Après ajout/édition d'un fichier : `M-x yas-reload-all`.

## python (`python-mode` + `python-ts-mode`)

### typage
| clé | ce que ça pose |
|---|---|
| `future` | `from __future__ import annotations` |
| `dcls` | `@dataclass(slots=True, frozen=True)` + docstring |
| `pyd` | modèle pydantic v2 (`ConfigDict(frozen, extra="forbid")` + `Field`) |
| `senum` | `StrEnum` (3.11+) |
| `proto` | `@runtime_checkable class X(Protocol)` |
| `tdict` | `TypedDict` |
| `tcheck` | `if TYPE_CHECKING:` — import sans coût runtime |

### contrôle
| clé | ce que ça pose |
|---|---|
| `match` | `match/case` + `case _` qui lève |
| `guard` | clause de garde (`if not cond: raise`) |

### stdlib
| clé | ce que ça pose |
|---|---|
| `pth` | `Path(__file__).resolve().parent` |
| `cache` | `@functools.cache` |
| `ctxm` | `@contextmanager` avec `try/finally` |

### async
| clé | ce que ça pose |
|---|---|
| `adef` | `async def` typée |
| `arun` | `async def main()` + `asyncio.run(main())` |
| `tgroup` | `asyncio.TaskGroup` (3.11+) |

### tests
| clé | ce que ça pose |
|---|---|
| `test` | test pytest arrange / act / assert |
| `param` | `@pytest.mark.parametrize` avec `pytest.param(..., id=...)` |
| `fix` | fixture `yield` typée |
| `raises` | `pytest.raises(..., match=r"...")` |
| `given` | `@given(...)` hypothesis |

### observabilité
| clé | ce que ça pose |
|---|---|
| `lgr` | setup loguru complet (stderr coloré + fichier avec rotation) |
| `timer` | chrono `perf_counter` + log |
| `bpt` | `breakpoint()` |
| `nd` | docstring numpydoc (Parameters / Returns / Raises / Examples) |

### data
| clé | ce que ça pose |
|---|---|
| `plz` | pipeline polars **lazy** (`scan → filter → group_by → agg → collect`) |
| `pdp` | chaîne pandas (`rename → assign → query → sort`) |
| `stapp` | squelette streamlit (`set_page_config`, `@st.cache_data`, sidebar) |
| `pxfig` | figure plotly express + `update_layout` propre |

### web / script
| clé | ce que ça pose |
|---|---|
| `fapi` | FastAPI : `lifespan` + `/health` |
| `route` | endpoint typé avec `Annotated[..., Depends(...)]` |
| `cli` | `parse_args()` + `main() -> int` + `raise SystemExit(main())` |

## emacs-lisp (édition de ta config Doom)
| clé | ce que ça pose |
|---|---|
| `usep` | `(use-package! ... :defer :commands :hook :init :config)` |
| `after` | `(after! ...)` |
| `mapl` | `(map! :leader (:prefix ...))` |
| `hookd` | `(add-hook! ...)` |
| `pkg` | `(package! ... :recipe (:host github ...))` |

## yaml
| clé | ce que ça pose |
|---|---|
| `gha` | GitHub Actions Python : uv + matrice + ruff + mypy + pytest-cov |
| `svc` | service docker compose (healthcheck, depends_on condition, restart) |
| `pc` | `.pre-commit-config.yaml` ruff + hooks + mypy |

## toml
| clé | ce que ça pose |
|---|---|
| `pyproject` | PEP 621 complet : `[dependency-groups]`, hatchling, ruff, mypy strict, pytest |
| `ruff` | bloc `[tool.ruff]` avec `per-file-ignores` pour `tests/` |

## sh
| clé | ce que ça pose |
|---|---|
| `strict` | en-tête bash strict (`set -euo pipefail`, `IFS`), `usage()`, `getopts` |

## markdown
| clé | ce que ça pose |
|---|---|
| `fence` | bloc de code |
| `details` | `<details><summary>` repliable |
| `mermaid` | diagramme mermaid |

## org
| clé | ce que ça pose |
|---|---|
| `srcpy` | `#+begin_src python :session :results output :exports both` |
| `todo` | entrée TODO avec DEADLINE et `:CREATED:` datés automatiquement |

## dockerfile
| clé | ce que ça pose |
|---|---|
| `pyuv` | image python multi-stage avec `uv` + cache mounts + user non-root |
