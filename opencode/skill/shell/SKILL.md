---
name: shell
description: Scripts shell (bash, zsh, sh) et fichiers de configuration de shell. À utiliser pour tout .sh, .bash, .zsh, .zshrc, .zshenv, Makefile ou script sans extension avec un shebang shell.
---

# Shell

## Toujours

```bash
#!/usr/bin/env bash
set -euo pipefail
```

Et fais tourner `shellcheck` (installé) avant de livrer. Il attrape la
majorité des bugs sans qu'on ait à exécuter le script.

## Les pièges qui mordent vraiment

- **Guillemets partout** : `"$var"`, `"$@"`, `"${arr[@]}"`. Un chemin avec un
  espace casse tout script qui les oublie.
- `set -e` **et** une commande qui peut échouer légitimement : ajoute
  `|| true`. Un `git log` sur un dépôt sans commit sort en 128 et tue le
  script.
- `set -u` **et** un tableau vide : `"${arr[@]}"` explose en bash 3.2 (celui
  de macOS). Écris `"${arr[@]+"${arr[@]}"}"`.
- macOS = **bash 3.2** pour `/bin/bash`. Pas de tableaux associatifs, pas de
  `${var,,}`. Si tu as besoin de bash moderne, `#!/usr/bin/env bash` avec le
  bash de homebrew, et dis-le.
- `sed -i` : macOS exige `sed -i ''`. Écris plutôt un fichier temporaire, ou
  utilise `perl -pi -e`, portable partout.
- Test de fichier avant usage : `[[ -r $f ]]`, pas seulement `[[ -f $f ]]`.

## Style

- `[[ ]]` plutôt que `[ ]` en bash.
- `$(...)` jamais les backticks.
- `local` sur toute variable de fonction.
- Un script qui prend des arguments affiche un usage clair s'ils manquent.

## Configuration zsh

- `~/.zshenv` : PATH et variables d'environnement — lu par **tous** les
  shells, y compris non interactifs (scripts lancés par Emacs, launchd, cron).
- `~/.zshrc` : uniquement l'interactif — alias, prompt, complétions, plugins.
- `typeset -gxU PATH path` : le `-x` est obligatoire, sinon PATH peut ne pas
  être exporté et les sous-processus reçoivent le PATH par défaut du système.
- Jamais de chemin contenant des espaces dans PATH : certains outils
  (`pyenv init`) reconstruisent PATH via un sous-shell et cassent dessus.
