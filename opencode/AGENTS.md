# Conventions globales

Instructions par défaut pour toutes les sessions. Un `AGENTS.md` de projet
prime toujours sur ce fichier.

## Méthode

- Lis le code existant avant d'écrire. Le style du fichier fait loi :
  nommage, densité de commentaires, gestion d'erreurs, structure des tests.
- Une tâche = un changement cohérent. Pas de refactor opportuniste non demandé.
- Si la demande est ambiguë sur un point qui change le résultat, pose la
  question. Sinon, choisis, avance, et signale l'hypothèse retenue.
- Termine ce qui est demandé. Si une partie est bloquée, livre le reste et
  dis explicitement ce qui manque et pourquoi.

## Code

- Pas de commentaire qui paraphrase le code. Un commentaire explique un
  *pourquoi* non évident.
- Erreurs gérées explicitement : pas de `catch` silencieux, pas de `except:`
  nu, pas d'`unwrap()` sur un chemin faillible.
- Pas de secret, token ou clé en dur. Variables d'environnement uniquement.
- Types stricts quand le langage le permet (`strict` TS, annotations Python,
  pas d'`any` sans justification).
- Nouvelle dépendance = à justifier ; préférer la stdlib et ce qui est déjà
  dans le projet.

## Vérification

- Après une modification : lance le linter/formatter du projet puis les tests
  qui touchent le code changé. Rapporte la sortie réelle, y compris en échec.
- Ne déclare jamais « ça marche » sans avoir exécuté quelque chose qui le prouve.
- Bug corrigé = test qui échouait avant et passe après.

## Git

- Ne commit et ne push que si on te le demande.
- Messages de commit à l'impératif, une ligne de sujet ≤ 72 caractères,
  le *pourquoi* dans le corps si ce n'est pas évident.
- Jamais de `git push --force` sur une branche partagée, jamais de
  `git reset --hard` sans confirmation.

## Shell

- Outils disponibles et à privilégier : `rg` (pas `grep -r`), `fd` (pas
  `find`), `bat`, `eza`, `gh`, `lazygit`, `fzf`, `zoxide`.
- Chemins cliquables dans les rapports : `chemin/fichier.ts:42`.
