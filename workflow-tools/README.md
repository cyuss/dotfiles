# workflow-tools

Scripts utilitaires pour AeroSpace et SketchyBar.

## AeroSpace
- `~/.config/workflow-tools/aerospacectl start`
- `~/.config/workflow-tools/aerospacectl stop`
- `~/.config/workflow-tools/aerospacectl restart`
- `~/.config/workflow-tools/aerospacectl reload`
- `~/.config/workflow-tools/aerospacectl status`

## SketchyBar
- `~/.config/workflow-tools/sketchybarctl start`
- `~/.config/workflow-tools/sketchybarctl stop`
- `~/.config/workflow-tools/sketchybarctl restart`
- `~/.config/workflow-tools/sketchybarctl reload`
- `~/.config/workflow-tools/sketchybarctl status`

## Profils de marge SketchyBar (mac/external)
- Afficher les valeurs: `~/.config/workflow-tools/bar-margin-profile show`
- Définir marge laptop: `~/.config/workflow-tools/bar-margin-profile set mac 8`
- Définir marge écran externe: `~/.config/workflow-tools/bar-margin-profile set external 12`
- Appliquer laptop: `~/.config/workflow-tools/bar-margin-profile apply mac`
- Appliquer externe: `~/.config/workflow-tools/bar-margin-profile apply external`
- Appliquer auto selon nb d'écrans: `~/.config/workflow-tools/bar-margin-profile apply auto`

Les profils sont stockés dans `~/.config/workflow-tools/.bar_margin_profiles`.

## Basculer tout le workflow
- Activer tout: `~/.config/workflow-tools/workflow-on`
- Désactiver tout (retour macOS): `~/.config/workflow-tools/workflow-off`

## Switch Spaces (utile pour fullscreen macOS)
- `~/.config/workflow-tools/spaceswitcher next`
- `~/.config/workflow-tools/spaceswitcher prev`

## Palette applications (liste + choix)
- `~/.config/workflow-tools/apppicker`
- Raccourcis: `Hyper+Tab` (et fallback `Cmd+Alt+Tab`)

## AltTab (switcher style Cmd+Tab)
- `~/.config/workflow-tools/alttabctl start`
- `~/.config/workflow-tools/alttabctl stop`
- `~/.config/workflow-tools/alttabctl restart`
- `~/.config/workflow-tools/alttabctl status`
