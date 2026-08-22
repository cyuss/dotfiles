# Workflow Tools Cheat Sheet

## 1) AeroSpace (`aerospacectl`)

### Commandes
```bash
aerospacectl status
aerospacectl start
aerospacectl stop
aerospacectl restart
aerospacectl reload
```

### Logique interne
- `status`: vérifie si le serveur AeroSpace répond via `aerospace list-workspaces --all`.
- `start`:
  1. Si déjà actif -> ne fait rien.
  2. Tente `open -a /Applications/AeroSpace.app`.
  3. Si échec, tente le binaire direct `/Applications/AeroSpace.app/Contents/MacOS/AeroSpace` en background.
  4. Re-vérifie que le serveur répond.
- `stop`: tue le process `AeroSpace` puis vérifie qu'il n'est plus actif.
- `restart`: `stop` puis `start`.
- `reload`: recharge uniquement la config (`aerospace reload-config`) si AeroSpace est actif.

## 2) SketchyBar (`sketchybarctl`)

### Commandes
```bash
sketchybarctl status
sketchybarctl start
sketchybarctl stop
sketchybarctl restart
sketchybarctl reload
```

### Logique interne
- `status`: vérifie que SketchyBar répond avec `sketchybar --query bar`.
- `start`:
  1. Si déjà actif -> ne fait rien.
  2. Tente de démarrer via `launchctl` (service Homebrew: `homebrew.mxcl.sketchybar`).
  3. Si échec, lance `sketchybar` directement en background.
  4. Re-vérifie qu'il répond.
- `stop`: stoppe le service `launchctl` + tue le process `sketchybar`.
- `restart`: `stop` puis `start`.
- `reload`: recharge la config en cours (`sketchybar --reload`) si actif.

## 3) Profils de marge (`bar-margin-profile`)

### Commandes
```bash
bar-margin-profile show
bar-margin-profile set mac 8
bar-margin-profile set external 12
bar-margin-profile apply mac
bar-margin-profile apply external
bar-margin-profile apply auto
```

### Logique interne
- `show`: affiche les valeurs sauvegardées (`mac`, `external`).
- `set mac|external <n>`: sauvegarde les marges dans `~/.config/workflow-tools/.bar_margin_profiles`.
- `apply mac|external`:
  1. Modifie `margin=` dans `~/.config/sketchybar/sketchybarrc`.
  2. Lance `~/.config/aerospace/scripts/sync_top_gap.sh` pour ajuster `outer.top`.
  3. Recharge SketchyBar (`sketchybarctl reload` si dispo, sinon `sketchybar --reload`).
- `apply auto`:
  1. Lit le nombre d'écrans via `aerospace list-monitors --count`.
  2. Si `>= 2` -> applique profil `external`.
  3. Sinon -> applique profil `mac`.

## 4) Flux recommandé

### Après modifs AeroSpace
```bash
aerospacectl reload
aerospacectl status
```

### Après modifs SketchyBar
```bash
sketchybarctl reload
sketchybarctl status
```

### Changer de setup écran rapidement
```bash
bar-margin-profile apply auto
```

## 5) Dépannage rapide

### AeroSpace ne répond pas
```bash
aerospacectl restart
aerospace list-workspaces --all
```

### SketchyBar ne répond pas
```bash
sketchybarctl restart
sketchybar --query bar
```

### Marge incorrecte
```bash
bar-margin-profile show
bar-margin-profile apply auto
```

## 6) Alternative à Cmd+Tab (`appswitcher`)

### Commandes directes
```bash
appswitcher next
appswitcher prev
```

### Raccourcis AeroSpace (Hyper)
- `Hyper + u` -> application suivante
- `Hyper + y` -> application précédente

### Logique
- Lit la liste des apps visibles (non background) via `System Events`.
- Trouve l'app frontmost actuelle.
- Calcule l'index suivant/précédent (avec boucle).
- Active l'app cible avec `tell application ... to activate`.

Cette méthode contourne le bug de `Cmd+Tab` dans certains scénarios de fullscreen natif macOS.

## 7) Basculer tout le workflow

### Commandes
```bash
workflow-on
workflow-off
```

### Logique `workflow-off`
- Stoppe AeroSpace + SketchyBar.
- Tue `skhd`, `yabai`, `AltTab` si présents.
- Supprime `AppleSpacesSwitchOnActivate` pour revenir au défaut macOS.
- Relance `Dock`.

### Logique `workflow-on`
- Tue d'abord `skhd`, `yabai`.
- Force `AppleSpacesSwitchOnActivate=true`.
- Relance `Dock`.
- Démarre AeroSpace + SketchyBar + AltTab.
- Applique la marge SketchyBar en mode auto (`mac`/`external`).
- Recharge les configs.

## 8) Switch fiable des apps fullscreen macOS (`spaceswitcher`)

### Commandes
```bash
spaceswitcher next
spaceswitcher prev
```

### Raccourcis AeroSpace (Hyper)
- `Hyper + o` -> Space suivant (Ctrl+Right)
- `Hyper + p` -> Space précédent (Ctrl+Left)

### Pourquoi
Quand une app est en fullscreen natif macOS, c'est un Space dédié.
Le switch par Space est plus fiable que le switch par app dans ce cas.

## 9) Palette d'apps (liste + choix) `apppicker`

### Commande
```bash
apppicker
```

### Raccourcis AeroSpace
- `Hyper + Tab` -> ouvre la liste d'apps
- `Cmd + Alt + Tab` -> fallback

### Logique
- Récupère les apps ouvertes (non background).
- Affiche une liste macOS native (`choose from list`).
- Active l'app sélectionnée.

C'est le mode le plus proche de "voir une liste puis choisir", sans dépendre de Cmd+Tab.

## 10) AltTab (interface proche Cmd+Tab)

### Commandes
```bash
alttabctl start
alttabctl stop
alttabctl restart
alttabctl status
```

### Note importante
- L'interface **exacte** Apple `Cmd+Tab` n'est fournie que par macOS.
- AltTab donne une interface très proche (liste/preview), et gère mieux certains cas fullscreen.

## 11) Raccourcis AeroSpace clés (actuels)

- Fullscreen AeroSpace: `Hyper + t`
- Fullscreen natif macOS: `Hyper + g`
- Resize mode: `Hyper + r`, puis `i/j/k/l`
- App suivante/précédente: `Hyper + u` / `Hyper + y`
- Space suivant/précédent: `Hyper + o` / `Hyper + p`
- App picker (liste): `Hyper + Tab`

Fallbacks utiles:
- App picker: `Cmd + Alt + Tab`
- Space next/prev: `Cmd + Alt + O/P`
