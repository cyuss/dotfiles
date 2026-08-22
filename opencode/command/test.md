---
description: Lance les tests du projet et corrige ce qui casse
---

Détecte le runner de tests du projet (package.json, Makefile, pyproject.toml,
Cargo.toml, go.mod…), lance la suite, et corrige les échecs un par un.

Contexte :
!`ls -a | head -40`

Pour chaque échec : cite la sortie réelle, explique la cause, corrige, relance.
Ne modifie pas un test pour le faire passer sans dire explicitement pourquoi le
test était faux.

$ARGUMENTS
