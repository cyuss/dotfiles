# Cheat Sheet Complète - Workflow / AeroSpace / SketchyBar

## 1) Commandes Workflow (global)

```bash
workflow-on
workflow-off
```

- `workflow-on`:
  - active le workflow complet
  - démarre AeroSpace + SketchyBar + AltTab
  - applique la marge SketchyBar auto (mac/external)
- `workflow-off`:
  - stoppe AeroSpace + SketchyBar
  - coupe les helpers (skhd/yabai/AltTab)
  - remet comportement macOS par défaut

## 2) Contrôle AeroSpace

```bash
aerospacectl status
aerospacectl start
aerospacectl stop
aerospacectl restart
aerospacectl reload
```

## 3) Contrôle SketchyBar

```bash
sketchybarctl status
sketchybarctl start
sketchybarctl stop
sketchybarctl restart
sketchybarctl reload
```

## 4) Profils de marge SketchyBar

```bash
bar-margin-profile show
bar-margin-profile set mac 8
bar-margin-profile set external 12
bar-margin-profile apply mac
bar-margin-profile apply external
bar-margin-profile apply auto
```

- `apply auto`: choisit `external` si >= 2 écrans, sinon `mac`.

## 5) App switchers créés

### Switch app suivante/précédente
```bash
appswitcher next
appswitcher prev
```

### Switch Space suivant/précédent (utile fullscreen natif macOS)
```bash
spaceswitcher next
spaceswitcher prev
```

### Palette d'apps (liste + choix)
```bash
apppicker
```

### AltTab control
```bash
alttabctl status
alttabctl start
alttabctl stop
alttabctl restart
```

## 6) Raccourcis AeroSpace (actuels)

### Focus fenêtres
- `Hyper + h` -> focus left
- `Hyper + j` -> focus left
- `Hyper + k` -> focus down
- `Hyper + l` -> focus right
- `Hyper + i` -> focus up

Fallbacks focus:
- `Cmd + Alt + Ctrl + i/j/k/l`
- `Cmd + Alt + i/j/k/l`

### Déplacement fenêtres (SEDF)
- `Hyper + s` -> move left
- `Hyper + e` -> move up
- `Hyper + d` -> move down
- `Hyper + f` -> move right

### Layout / Fullscreen
- `Hyper + Space` -> toggle `tiles/accordion`
- `Hyper + Enter` -> toggle `floating/tiling`
- `Hyper + t` -> fullscreen AeroSpace
- `Hyper + g` -> fullscreen natif macOS

### Resize mode
- `Hyper + r` -> entrer en mode resize
- en mode resize:
  - `i` -> `resize height -50`
  - `j` -> `resize width -50`
  - `k` -> `resize height +50`
  - `l` -> `resize width +50`
  - `1` -> one-third
  - `2` -> two-thirds
  - `b` -> balance
  - `Esc` / `Enter` -> sortir du mode

### Workspaces
- `Hyper + 1..9` -> workspace 1..9
- `Hyper + 0` -> workspace 1
- `Hyper + n` -> mode send-to-workspace
- `Hyper + b` -> workspace back-and-forth
- `Hyper + m` -> move workspace to next monitor

### App / Space switching custom
- `Hyper + u` -> app suivante (`appswitcher next`)
- `Hyper + y` -> app précédente (`appswitcher prev`)
- `Hyper + o` -> space suivant (`spaceswitcher next`)
- `Hyper + p` -> space précédent (`spaceswitcher prev`)
- `Hyper + Tab` -> app picker (liste)

Fallbacks:
- `Cmd + Alt + Tab` -> app picker
- `Cmd + Alt + O/P` -> next/prev space
- `Cmd + Alt + Ctrl + O/P` -> next/prev space

### Service mode
- `Hyper + ;` ou `Hyper + x` -> mode service

## 7) SketchyBar: actions rapides

```bash
sketchybarctl reload
sketchybarctl restart
```

- La marge est pilotée via `bar-margin-profile`.
- Le top gap AeroSpace est resynchronisé automatiquement.

## 8) Où sont les scripts

Tous dans:
- `~/.config/workflow-tools`

Fichiers principaux:
- `aerospacectl`
- `sketchybarctl`
- `bar-margin-profile`
- `workflow-on`
- `workflow-off`
- `appswitcher`
- `spaceswitcher`
- `apppicker`
- `alttabctl`
