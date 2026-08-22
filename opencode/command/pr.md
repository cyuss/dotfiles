---
description: Rédige et ouvre une pull request
---

Prépare une pull request pour la branche courante.

Branche et divergence :
!`git status -sb | head -3 && git log --oneline @{u}..HEAD 2>/dev/null || git log --oneline -10`

Diff complet vs la base :
!`git diff $(git merge-base HEAD origin/HEAD 2>/dev/null || echo HEAD~5)..HEAD --stat`

Le titre suit le style des commits du repo. La description répond à trois
questions et rien d'autre : **quoi**, **pourquoi**, **comment tester**.
Pas de liste exhaustive des fichiers touchés — le diff est là pour ça.

Montre-moi le texte avant de lancer `gh pr create`.
