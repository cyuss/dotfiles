---
name: containers
description: Docker, Docker Compose et Kubernetes. À utiliser dès qu'un Dockerfile, docker-compose.yml, compose.yaml, Chart.yaml, kustomization.yaml ou un manifeste k8s est présent.
---

# Conteneurs & Kubernetes

## Outils de cette machine

`docker` · `docker compose` (v2, sans tiret) · `kubectl` · `helm` ·
`kustomize` · `k3d` · `lazydocker`.

Écris toujours `docker compose`, jamais `docker-compose` (v1 est morte).

## Dockerfile

- **Build multi-étages** dès qu'il y a une compilation. L'image finale ne doit
  pas contenir le toolchain.
- Ordonne les couches du moins au plus changeant : dépendances d'abord, code
  ensuite. Un `COPY . .` en haut du fichier invalide le cache à chaque commit.
- Tag précis, jamais `:latest` — ni en base, ni en déploiement.
- `USER` non-root avant le `CMD`.
- `.dockerignore` obligatoire : `.git`, `node_modules`, `.venv`, `__pycache__`.
  Sans lui, le contexte de build fait des centaines de Mo.
- Un `HEALTHCHECK` si le service écoute sur un port.

## Compose

- Pas de secret en clair. `env_file` ou les secrets Docker.
- Ports : `"127.0.0.1:8080:8080"` en dev, pour ne pas exposer sur le réseau.
- `depends_on` seul n'attend pas que le service soit *prêt* — ajoute
  `condition: service_healthy` avec un healthcheck.
- Volumes nommés pour les données, bind mounts pour le code en dev.

## Kubernetes

- **Toujours** `requests` et `limits`. Sans requests, le scheduler place à
  l'aveugle ; sans limits, un pod peut affamer le nœud.
- Liveness **et** readiness probes. Confondre les deux redémarre en boucle un
  pod qui démarre juste lentement.
- Pas de `:latest` — l'image doit être immuable pour que le rollback marche.
- Secrets via `Secret` monté, pas dans les env d'un manifeste versionné.
- `kubectl apply --dry-run=server -f` avant tout apply réel.

## Avant de proposer une commande destructive

`kubectl delete`, `docker system prune`, `helm uninstall` : montre d'abord ce
qui serait supprimé (`--dry-run`, `kubectl get`), puis demande.
