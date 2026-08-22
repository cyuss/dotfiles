---
name: go
description: Projets Go. À utiliser dès qu'un go.mod ou un fichier .go est présent. Couvre les commandes go, la gestion d'erreurs, la concurrence et les tests table-driven.
---

# Go

## Commandes

| Besoin | Commande |
|---|---|
| Build | `go build ./...` |
| Tests | `go test ./...` |
| Tests + race detector | `go test -race ./...` |
| Vet | `go vet ./...` |
| Format | `gofmt -w .` |
| Dépendances | `go mod tidy` |

Lance `go test -race` dès qu'il y a des goroutines. Le race detector trouve
en une seconde ce qui prend une journée à reproduire.

## Erreurs

- Vérifie **chaque** erreur. Un `_` sur une erreur doit être commenté.
- Enrichis le contexte : `fmt.Errorf("lecture de %s: %w", path, err)`.
  Le `%w` préserve la chaîne pour `errors.Is` / `errors.As`.
- Pas de `panic` dans une bibliothèque. Retourne l'erreur.

## Concurrence

- Toute goroutine doit avoir une fin claire : `context.Context` ou un canal
  de fermeture. Une goroutine sans sortie est une fuite.
- `defer wg.Done()` en première ligne de la goroutine.
- Mutex : `defer mu.Unlock()` juste après `mu.Lock()`.
- Ne passe pas de `context.Context` dans une struct — c'est un paramètre.

## Tests

- Table-driven, c'est l'idiome :

```go
tests := []struct{
    name string
    in   string
    want int
}{
    {"vide", "", 0},
    {"un mot", "salut", 1},
}
for _, tt := range tests {
    t.Run(tt.name, func(t *testing.T) { ... })
}
```

- `t.Helper()` dans les fonctions d'assertion maison.
- `t.Cleanup()` plutôt qu'un `defer` pour le nettoyage.
