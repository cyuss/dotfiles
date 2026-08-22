---
description: Relit les changements non commités
agent: reviewer
---

Relis les changements ci-dessous et applique ta grille de relecture.

Diff non commité :
!`git diff HEAD --stat && echo "---" && git diff HEAD`

Fichiers non suivis :
!`git ls-files --others --exclude-standard`
