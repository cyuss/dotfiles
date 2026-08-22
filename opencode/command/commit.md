---
description: Rédige et crée un commit à partir du staging
---

Analyse ce qui est mis en scène et crée un seul commit.

Staged :
!`git diff --cached --stat && echo "---" && git diff --cached`

Style des commits récents du repo (à imiter) :
!`git log --oneline -12`

Règles : sujet à l'impératif ≤ 72 caractères, corps seulement s'il apporte le
*pourquoi*. Si rien n'est staged, dis-le et arrête-toi. N'ajoute aucune
signature ni co-auteur. Ne push pas.
