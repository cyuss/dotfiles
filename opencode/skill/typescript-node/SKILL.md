---
name: typescript-node
description: Projets TypeScript ou JavaScript / Node. À utiliser dès qu'un package.json, tsconfig.json, .ts, .tsx, .js ou .jsx est présent. Couvre npm, TypeScript strict, prettier, les tests et les pièges async.
---

# TypeScript / Node

## Outils de cette machine

Node **24** (défaut nvm) · `npm` · `prettier` · `typescript-language-server`.
Pas de bun ni de deno installés — ne propose pas de commandes qui en dépendent.

## Commandes

Lis toujours les `scripts` du `package.json` **avant** d'inventer une commande :
la moitié du temps `npm test` ou `npm run lint` existe déjà.

| Besoin | Commande |
|---|---|
| Installer | `npm ci` si `package-lock.json` existe, sinon `npm install` |
| Types | `npx tsc --noEmit` |
| Format | `npx prettier --write .` |
| Tests | `npm test` (regarde le script réel) |

## TypeScript

- `strict: true` dans `tsconfig.json`. Si le projet ne l'a pas, signale-le
  mais ne l'active pas sans qu'on te le demande — ça casse la compilation.
- Pas d'`any`. Si un type est vraiment inconnu, `unknown` puis narrowing.
- Pas de `!` (non-null assertion) pour faire taire le compilateur : c'est un
  crash qui attend. Gère le cas nul.
- Types de retour explicites sur les fonctions exportées.
- `type` pour les unions et les formes, `interface` quand il faut étendre.

## Async — les pièges qui coûtent cher

- **`await` dans une boucle** quand les itérations sont indépendantes :
  utilise `Promise.all`. Mais garde la boucle si l'ordre compte ou si tu
  risques de saturer une API.
- **Promesse non attendue** : tout appel async sans `await` ni `.catch()` est
  une erreur silencieuse. Vérifie systématiquement.
- `Promise.all` échoue au premier rejet — `Promise.allSettled` si tu veux
  tous les résultats.
- Pas d'`async` dans un callback de `forEach` : il n'attend rien. Utilise
  `for...of`.

## Règles

- Aucune dépendance ajoutée sans justification. Node 24 a `fetch`,
  `structuredClone`, `node:test`, `AbortController` — pas besoin d'axios,
  lodash ou uuid pour ça.
- ESM (`import`) sauf si le projet est en CommonJS.
- Erreurs : `throw new Error("message actionnable")`, jamais `throw "string"`.
- Pas de `console.log` laissé dans le code livré.
