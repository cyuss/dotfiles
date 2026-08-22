---
description: Diagnostique une erreur puis corrige la cause racine
agent: debugger
---

Diagnostique ceci, puis propose le correctif : $ARGUMENTS

État du dépôt :
!`git status -sb 2>/dev/null | head -20`

Modifications récentes (souvent la piste la plus courte) :
!`git log --oneline -8 2>/dev/null`
