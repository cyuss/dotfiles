---
name: rust
description: Projets Rust. À utiliser dès qu'un Cargo.toml ou un fichier .rs est présent. Couvre cargo, clippy, la gestion d'erreurs et l'emprunt.
---

# Rust

## Commandes

| Besoin | Commande |
|---|---|
| Vérifier (rapide) | `cargo check` |
| Lint | `cargo clippy --all-targets -- -D warnings` |
| Format | `cargo fmt` |
| Tests | `cargo test` |
| Build release | `cargo build --release` |

`cargo check` avant `cargo build` : dix fois plus rapide pour valider que ça
compile.

## Gestion d'erreurs

- **Pas d'`unwrap()` ni d'`expect()`** sur un chemin faillible en code de
  production. Dans un test ou un prototype, c'est acceptable — dis-le.
- Bibliothèque : type d'erreur propre (`thiserror`).
  Binaire : `anyhow::Result` et le `?` partout.
- `?` plutôt qu'un `match` qui ne fait que propager.

## Emprunt — les réflexes

- Prends `&str` en paramètre, pas `String`, sauf si tu dois posséder la valeur.
- `&[T]` plutôt que `&Vec<T>`.
- Si le borrow checker résiste, c'est presque toujours un problème de
  conception, pas un obstacle à contourner : ne colle pas des `.clone()`
  partout pour le faire taire. Repense la propriété des données.
- `Rc<RefCell<T>>` est un signal d'alerte : demande-toi si un index ou une
  restructuration ne serait pas plus simple.

## Tests

- Tests unitaires dans le module, sous `#[cfg(test)] mod tests`.
- Tests d'intégration dans `tests/`.
- `assert_eq!` avec un message quand l'échec ne serait pas parlant.
