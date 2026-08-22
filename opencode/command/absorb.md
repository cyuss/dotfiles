---
description: Range les modifications en cours dans les bons commits de la pile
---

Utilise `git-absorb` pour répartir les changements non commités dans les
commits existants auxquels ils appartiennent.

Non commité :
!`git diff --stat`

Pile de commits concernée :
!`git log --oneline -15`

Marche à suivre : mets en scène ce qui doit l'être, lance
`git absorb --and-rebase --dry-run` d'abord, montre-moi ce qui serait
réparti, et n'exécute la vraie commande qu'après mon accord. Ce qui ne
peut être rattaché à aucun commit reste non commité — dis-le.
