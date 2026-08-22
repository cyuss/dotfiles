# -*- coding: utf-8 -*-
"""AeroSpace — reference d'usage approfondi."""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from lib import *

VER = os.environ.get("AS_VER", "AeroSpace 0.21.3-Beta")
DATE = os.environ.get("DOC_DATE", "2026-08-21")

TOC = [
    ("01", "Le modèle — un arbre"),
    ("02", "Hyper, le modificateur"),
    ("03", "Focus & déplacement"),
    ("04", "Les neuf workspaces"),
    ("05", "Les modes"),
    ("06", "Pourquoi ça se range seul"),
    ("07", "Règles à l'ouverture"),
    ("08", "Multi-écran"),
    ("09", "La ligne de commande"),
    ("10", "Dépannage"),
    ("11", "Index des touches"),
]

S = []

# ══════════════════════════════════════════════════════════════════ 01
S.append(section("01", "Le modèle — un arbre",
  lede="AeroSpace ne place pas des fenêtres sur une grille : il maintient un arbre de conteneurs. Comprendre cet arbre explique tout le reste — pourquoi une fenêtre atterrit là, pourquoi <kbd>Hyper+Espace</kbd> change ce qu'il change, et pourquoi il faut parfois remettre l'arbre à plat.",
  body=""
  + flow([
    ("A", "monitor", "Un écran physique. Chaque workspace y est ancré, ou libre."),
    ("B", "workspace", "Neuf ici, nommés. Chacun porte un arbre indépendant."),
    ("C", "container", "Un nœud : <em>tiles</em> ou <em>accordion</em>, horizontal ou vertical."),
    ("D", "window", "Une feuille de l'arbre. Ou <em>floating</em>, hors de l'arbre."),
  ])
  + "<h3>Les deux dispositions</h3>"
  + table(("disposition", "comportement", "quand"), [
      ("<b>tiles</b>", "les fenêtres se partagent l'espace, sans recouvrement", "le mode par défaut ; deux à quatre fenêtres visibles ensemble"),
      ("<b>accordion</b>", "les fenêtres s'empilent, une seule dépliée, les autres réduites à une tranche de <code>30</code>&nbsp;px", "cinq fenêtres et plus dans un même workspace : le pavage devient illisible, l'accordéon reste navigable"),
      ("<b>floating</b>", "hors de l'arbre, libre, au-dessus", "utilitaires, boîtes de dialogue, lanceurs"),
    ], widths=(16, 34, 50), classes=["", "d", "d"])
  + "<h3>Orientation</h3>"
  + "<p>Un conteneur est <b>horizontal</b> (les enfants côte à côte) ou <b>vertical</b> (les uns sous les autres). <code>default-root-container-orientation = 'auto'</code> : la racine choisit selon la forme de l'écran — horizontal sur un ultrawide, vertical sur un écran en portrait. Un conteneur imbriqué prend l'orientation <em>inverse</em> de son parent, ce qui produit un vrai pavage au lieu d'un alignement dans un seul sens.</p>"
  + note("key", "les marges",
    "<code>inner</code> 12 px entre les fenêtres, <code>outer</code> 12 px sur les quatre bords. <code>outer.top</code> vaut 12 comme les autres : macOS réserve déjà la barre de menus, et AeroSpace pose les fenêtres <em>sous</em> elle — cette valeur ne s'ajoute qu'à l'espace visible. Elle était auparavant calculée depuis la hauteur de SketchyBar ; SketchyBar étant désactivé, il n'y a plus rien à synchroniser.")
))

# ══════════════════════════════════════════════════════════════════ 02
S.append(section("02", "Hyper, le modificateur",
  lede="<b>Hyper = ⌘ + ⌥ + ⌃ + ⇧.</b> Quatre modificateurs à la fois : aucune application ne revendique cette combinaison, donc aucun raccourci n'entre jamais en conflit avec le gestionnaire de fenêtres.",
  body=""
  + note("key", "d'où vient Hyper sur ton clavier",
    "La colonne extérieure du Corne, rangée du milieu, des deux côtés : <code>&amp;het LG(LA(LS(LCTRL))) ESCAPE</code>. <b>Appui bref</b> = Échap, <b>appui maintenu</b> = Hyper. C'est ce qui rend l'accord confortable : une seule touche, sous l'auriculaire, au lieu de quatre doigts tordus.")
  + "<h3>Deux clusters, un seul geste</h3>"
  + table(("cluster", "touches", "sens", "note"), [
      ("<b>IJKL</b>", kbd("Hyper+i")+" "+kbd("j")+" "+kbd("k")+" "+kbd("l"), "i&nbsp;haut · j&nbsp;gauche · k&nbsp;bas · l&nbsp;droite", "<b>le geste principal.</b> Le même cluster que <kbd>Ctrl+b i j k l</kbd> dans herdr : un seul réflexe pour les fenêtres macOS et les panes du terminal"),
      ("<b>Sans Shift</b>", kbd("⌘⌥⌃+i")+" "+kbd("j")+" "+kbd("k")+" "+kbd("l"), "identique", "trois modificateurs au lieu de quatre, pour les moments où Hyper est déjà pris"),
      ("<b>Flèches</b>", kbd("Hyper+←")+" "+kbd("↓")+" "+kbd("↑")+" "+kbd("→"), "identique", "repli, sur un clavier sans Corne"),
      ("<b>SEDF</b>", kbd("Hyper+s")+" "+kbd("e")+" "+kbd("d")+" "+kbd("f"), "s&nbsp;gauche · e&nbsp;haut · d&nbsp;bas · f&nbsp;droite", "<b>déplacer</b> la fenêtre, et non déplacer le focus. Main gauche : les deux gestes ne se confondent jamais"),
    ], widths=(13, 26, 26, 35), classes=["", "k", "d", "d"])
  + note("note", "pourquoi IJKL et pas HJKL",
    "Sur un Corne, la rangée de repos porte les <em>home row mods</em> : <kbd>H</kbd> est libre mais <kbd>J K L</kbd> portent Shift, Ctrl et Alt en maintien. Le cluster IJKL évite tout chevauchement, et il forme une croix physique — <kbd>i</kbd> est bien <em>au-dessus</em> de <kbd>k</kbd>, ce que <kbd>hjkl</kbd> ne fait pas.")
))

# ══════════════════════════════════════════════════════════════════ 03
S.append(section("03", "Focus & déplacement",
  keys_pair("Déplacer le focus", [
      ("Hyper+i", "vers le haut"),
      ("Hyper+j", "vers la gauche"),
      ("Hyper+k", "vers le bas"),
      ("Hyper+l", "vers la droite"),
      ("Hyper+←↓↑→", "les mêmes, aux flèches"),
      ("Hyper+b", "revenir au workspace précédent"),
    ], "Déplacer la fenêtre", [
      ("Hyper+s", "vers la gauche"),
      ("Hyper+e", "vers le haut"),
      ("Hyper+d", "vers le bas"),
      ("Hyper+f", "vers la droite"),
      ("Hyper+n", "mode <em>envoyer vers…</em>"),
      ("Hyper+m", "envoyer le workspace sur l'autre écran"),
    ])
  + "<h3>Disposition & taille</h3>"
  + keys_pair("Disposition", [
      ("Hyper+Espace", "bascule tuiles ↔ accordéon"),
      ("Hyper+Entrée", "bascule flottant ↔ tuilé"),
      ("Hyper+t", "plein écran (dans le workspace)"),
      ("Hyper+g", "plein écran natif macOS"),
      ("Hyper+a", "<b>mode disposition</b> — une lettre suffit ensuite"),
    ], "Taille", [
      ("Hyper+y", "rééquilibrer toutes les fenêtres"),
      ("Hyper+u", "la fenêtre courante au <b>tiers</b>"),
      ("Hyper+o", "la fenêtre courante aux <b>deux tiers</b>"),
      ("Hyper+r", "<b>mode redimensionnement</b>"),
    ])
  + note("tip", "le tiers / deux tiers",
    "<kbd>Hyper+u</kbd> et <kbd>Hyper+o</kbd> rééquilibrent d'abord, puis appliquent le ratio — c'est ce qui les rend reproductibles quel que soit l'état de départ. Sur un ultrawide, deux tiers pour l'éditeur et un tiers pour le terminal est la disposition la plus rentable : le code garde une largeur de lecture correcte, et le terminal reste lisible.")
  + "<h3>Changer de fenêtre entre applications</h3>"
  + cmds([
      ("⌘+Tab", "appswitcher maison — fonctionne aussi à travers les espaces plein écran natifs, contrairement au commutateur macOS"),
      ("⌘+⇧+Tab", "le même, en sens inverse"),
      ("⌘+⌥+Tab", "alias du précédent"),
    ], widths=(20, 80))
))

# ══════════════════════════════════════════════════════════════════ 04
S.append(section("04", "Les neuf workspaces",
  lede="Nommés, déclarés explicitement (<code>persistent-workspaces</code>) et thématiques. Une application « destination » y est routée automatiquement à l'ouverture ; le terminal et l'éditeur, eux, restent là où tu travailles.",
  body=""
  + table(("n°", "workspace", "écran", "ce qu'on y met (routage automatique)"), [
      ("1", "<b>01_Main</b>", "principal", "libre — le workspace de départ"),
      ("2", "<b>02_Coding</b>", "principal", "libre — <em>volontairement pas de routage</em> : Alacritty et Emacs s'ouvrent là où tu es"),
      ("3", "<b>03_Work</b>", "secondaire", "Mimestream, Telegram, Discord"),
      ("4", "<b>04_Management</b>", "principal", "Notion, Obsidian, ClickUp"),
      ("5", "<b>05_I.A</b>", "principal", "Claude, ChatGPT, Perplexity"),
      ("6", "<b>06_Web</b>", "principal", "Arc, Chrome, Firefox, Safari"),
      ("7", "<b>07_System</b>", "secondaire", "libre — surveillance"),
      ("8", "<b>08_Ops</b>", "principal", "Docker, Postman, DBeaver"),
      ("9", "<b>09_Divers</b>", "secondaire", "Spotify, IINA, VLC"),
    ], widths=(6, 20, 16, 58), classes=["num", "", "d", "d"])
  + "<h3>Les touches</h3>"
  + keys_pair("Aller au workspace", [
      ("Hyper+1..9", "aller au workspace n°N"),
      ("Hyper+0", "retour à 01_Main"),
      ("Hyper+b", "aller-retour entre les deux derniers"),
      ("Hyper+m", "déplacer le workspace sur l'autre écran"),
    ], "Y envoyer une fenêtre", [
      ("Hyper+n puis 1..9", "envoyer la fenêtre <b>et suivre</b>"),
      ("Hyper+n puis Échap", "annuler"),
    ])
  + note("key", "pourquoi le terminal et l'éditeur ne sont pas routés",
    "C'est un choix de conception, pas un oubli. Une fenêtre Alacritty ou Emacs doit s'ouvrir <b>là où tu travailles</b>, pas sauter ailleurs. Seules les applications « destination » — navigateur, messagerie, média, ops — sont routées, parce que là le gain est sans ambiguïté : on sait toujours où les retrouver.")
  + note("warn", "declaration explicite en config v2",
    "En <code>config-version = 2</code>, les workspaces persistants ne sont plus déduits des raccourcis clavier (l'heuristique a été jugée fragile en amont). Il faut les déclarer dans <code>persistent-workspaces</code> — sinon un workspace vide disparaît, et son raccourci ne mène nulle part.")
))

# ══════════════════════════════════════════════════════════════════ 05
S.append(section("05", "Les modes",
  lede="Un mode capture le clavier : on y entre par un accord Hyper, puis chaque action tient en <b>une lettre</b>, sans modificateur. <kbd>Échap</kbd> ou <kbd>Entrée</kbd> ramène toujours au mode principal.",
  body=""
  + grid([
      card("Mode disposition", "Hyper+a",
        "Remplace trois accords Hyper distincts par un mode où une seule lettre suffit.",
        [("t", "tuiles"), ("a", "accordéon"), ("f", "bascule flottant"),
         ("h", "forcer horizontal"), ("v", "forcer vertical"),
         ("s", "plein écran"), ("g", "plein écran natif macOS"),
         ("b", "rééquilibrer"), ("r", "<b>remettre l'arbre à plat</b>"),
         ("Échap", "revenir")], widths=(24, 76)),
      card("Mode redimensionnement", "Hyper+r",
        "Ajustement fin, en restant dans le mode tant qu'on n'a pas fini.",
        [("i", "hauteur −50"), ("k", "hauteur +50"),
         ("j", "largeur −50"), ("l", "largeur +50"),
         ("1", "au tiers"), ("2", "aux deux tiers"),
         ("b", "rééquilibrer"), ("Échap", "revenir"), ("", "")], widths=(24, 76)),
    ])
  + grid([
      card("Mode envoi", "Hyper+n",
        "Envoyer la fenêtre courante dans un workspace <b>et l'y suivre</b>.",
        [("1..9", "envoyer vers le workspace n°N et y aller"),
         ("0", "envoyer vers 01_Main"),
         ("Échap", "annuler")], widths=(24, 76)),
      card("Mode service", "Hyper+; · Hyper+x",
        "Les gestes rares, tenus à l'écart des touches quotidiennes.",
        [("Échap", "<b>recharger la configuration</b>"),
         ("r", "remettre l'arbre du workspace à plat"),
         ("f", "bascule flottant / tuilé"),
         ("Retour arrière", "fermer toutes les fenêtres sauf celle-ci"),
         ("q", "fermer la fenêtre")], widths=(24, 76)),
    ])
  + note("tip", "le geste qui répare tout",
    "Quand la disposition d'un workspace devient incompréhensible — des fenêtres qui ne bougent plus, des conteneurs imbriqués les uns dans les autres — <b>"+kbd("Hyper+a")+" puis "+kbd("r")+"</b> remet l'arbre à plat. Toutes les fenêtres redeviennent enfants directs de la racine, et le pavage repart de zéro.")
))

# ══════════════════════════════════════════════════════════════════ 06
S.append(section("06", "Pourquoi ça se range tout seul",
  lede="Deux options portent tout le comportement automatique. Elles étaient à <code>false</code> — les défauts AeroSpace sont <code>true</code> — et c'est exactement pour cela que plus rien ne se plaçait seul.",
  body=""
  + table(("option", "ce qu'elle fait", "ce qui se passait sans elle"), [
      ("<code>enable-normalization-<br>flatten-containers</code>",
       "supprime les conteneurs qui n'ont qu'un seul enfant",
       "ces conteneurs s'empilaient au fil des ouvertures et fermetures. Chaque nouvelle fenêtre s'ajoutait dans un nœud enfoui, et la disposition se figeait"),
      ("<code>enable-normalization-<br>opposite-orientation-<br>for-nested-containers</code>",
       "un conteneur imbriqué prend l'orientation <em>inverse</em> de son parent",
       "tout s'alignait dans le même sens. Au lieu d'un pavage, on obtenait une longue rangée de fenêtres de plus en plus étroites"),
    ], widths=(24, 26, 50), classes=["c", "d", "d"])
  + note("key", "le diagnostic, en une phrase",
    "Un arbre qui dégénère ne produit pas d'erreur : il produit des fenêtres qui « ne bougent pas ». Si <kbd>Hyper+Espace</kbd> semble sans effet sur un workspace précis, c'est presque toujours l'arbre — <kbd>Hyper+a</kbd> <kbd>r</kbd> le confirme immédiatement.")
  + "<h3>Les autres réglages de comportement</h3>"
  + table(("réglage", "valeur", "effet"), [
      ("<code>accordion-padding</code>", "30", "largeur de la tranche visible des fenêtres repliées"),
      ("<code>on-focus-changed</code>", "<code>move-mouse window-lazy-center</code>", "le curseur suit la fenêtre focalisée. <em>lazy</em> : il ne bouge que s'il n'est pas déjà dedans — pas de saut si tu es à la souris"),
      ("<code>on-focused-monitor-changed</code>", "<code>move-mouse monitor-lazy-center</code>", "idem au changement d'écran"),
      ("<code>automatically-unhide-<br>macos-hidden-apps</code>", "true", "une app masquée par ⌘H réapparaît quand on la focalise"),
      ("<code>start-at-login</code>", "true", "AeroSpace démarre avec la session"),
      ("<code>key-mapping.preset</code>", "qwerty", "l'interprétation des touches physiques"),
    ], widths=(24, 22, 54), classes=["c", "c", "d"])
))

# ══════════════════════════════════════════════════════════════════ 07
S.append(section("07", "Règles à l'ouverture",
  lede="<code>on-window-detected</code> agit une seule fois, au moment où la fenêtre apparaît. Deux familles de règles seulement — la retenue est délibérée.",
  body=""
  + "<h3>1 · Utilitaires : flottants, jamais tuilés</h3>"
  + "<p>Les tuiler n'a aucun sens et casse la mise en page à chaque apparition.</p>"
  + cmds2([
      ("Raycast", "lanceur"), ("Maccy", "presse-papiers"),
      ("Shottr", "capture d'écran"), ("Numi", "calculatrice"),
      ("Réglages Système", "panneau macOS"), ("Karabiner-Elements", "réglages clavier"),
      ("BetterDisplay", "gestion des écrans"),
    ])
  + note("key", "Finder n'y est plus, et c'est un choix",
    "Il figurait dans cette liste par erreur de classement. Les entrées ci-dessus sont des fenêtres <b>passagères</b> — lanceur, presse-papiers, capture, HUD — qu'on fait apparaître puis disparaître ; les tuiler casse la mise en page à chaque apparition. "
    "Finder est une fenêtre dans laquelle on <b>travaille</b> : la tuiler a exactement le même sens que tuiler un terminal. "
    "Vérifié avant de changer : <code>aerospace list-windows --all</code> ne rapporte qu'une seule fenêtre Finder, celle du dossier ouvert — le bureau n'est pas compté comme une fenêtre, il n'y avait donc rien à protéger. "
    "Ses boîtes Ouvrir/Enregistrer restent flottantes, couvertes par la règle de titre ci-dessous.")
  + "<h3>Finder : un seul emplacement</h3>"
  + table(("point", "ce qu'il en est"), [
      ("<b>Les onglets fonctionnent</b>", "sur une fenêtre <b>tuilée</b>, <kbd>⌘T</kbd> ajoute un onglet — le nombre de fenêtres ne bouge pas, donc l'emplacement reste unique. Vérifié en A/B : tuilé <code>1→1→1→1</code>, flottant <code>1→2→3→3</code>. C'est flottant que le comportement se dégrade, pas l'inverse"),
      ("<b>⌘N remappé</b>", "<code>⌘N</code> ouvre désormais un <b>onglet</b>, <code>⌥⌘N</code> une fenêtre. Sans ça, le réflexe <code>⌘N</code> créait une fenêtre, donc un emplacement de plus à chaque fois"),
      ("<b>Desktop &amp; Copy</b>", "le bureau et la fenêtre de progression de copie sont des fenêtres Finder à part entière. Elles flottent par règle, sinon elles prenaient un emplacement pour rien"),
      ("<b>Ce qui ne marche pas</b>", "<code>AppleWindowTabbingMode = always</code> et <code>FinderSpawnTab = true</code> ne changent <b>rien</b> au comportement de Finder — testés avec redémarrage entre chaque essai, puis retirés. <em>Merge All Windows</em> reste grisé tant qu'AeroSpace gère les fenêtres"),
    ], widths=(22, 78), classes=["", "d"])
  + note("warn", "une règle ne s'applique qu'aux fenêtres à venir",
    "<code>on-window-detected</code> se déclenche à la <b>création</b> de la fenêtre. Modifier ces règles ne réorganise donc pas ce qui est déjà ouvert : il faut fermer et rouvrir la fenêtre, ou la basculer à la main avec <b>"+kbd("Hyper+Entrée")+"</b>. En ligne de commande : <code>aerospace layout tiling --window-id &lt;id&gt;</code>.")
  + "<p class=small>Plus une règle par <b>titre</b> : toute fenêtre dont le titre commence par <code>Open</code>, <code>Save</code>, <code>Enregistrer</code>, <code>Ouvrir</code>, <code>Preferences</code> ou <code>Réglages</code> flotte. <code>check-further-callbacks = true</code> : les règles suivantes continuent de s'appliquer.</p>"
  + "<h3>2 · Applications « destination » : routées</h3>"
  + table(("workspace", "applications"), [
      ("<b>06_Web</b>", "Arc · Chrome · Firefox · Safari"),
      ("<b>05_I.A</b>", "Claude · ChatGPT · Perplexity"),
      ("<b>03_Work</b>", "Mimestream · Telegram · Discord"),
      ("<b>04_Management</b>", "Notion · Obsidian · ClickUp"),
      ("<b>08_Ops</b>", "Docker · Postman · DBeaver"),
      ("<b>09_Divers</b>", "Spotify · IINA · VLC"),
    ], widths=(22, 78), classes=["", "d"])
  + note("note", "trouver l'identifiant d'une application",
    "<code>aerospace list-apps</code> donne l'<code>app-id</code> exact de tout ce qui tourne. C'est cet identifiant qu'il faut mettre dans <code>if.app-id</code> — le nom affiché ne suffit pas, et diffère parfois de l'identifiant de bundle.")
))

# ══════════════════════════════════════════════════════════════════ 08
S.append(section("08", "Multi-écran",
  lede="Les ancrages ne s'activent qu'au branchement du second écran. Sur un seul écran, tout retombe dessus — les règles sont inertes, il n'y a rien à désactiver en déplacement.",
  body=""
  + table(("écran", "workspaces", "logique"), [
      ("<b>main</b> (ultrawide)", "01 · 02 · 04 · 05 · 06 · 08", "le travail lourd : code, terminal, navigateur, ops, notes"),
      ("<b>secondary / built-in</b>", "03 · 07 · 09", "la surveillance et le loisir : communication, système, média"),
    ], widths=(22, 24, 54), classes=["", "d", "d"])
  + keys([
      ("Hyper+m", "envoyer le workspace courant sur l'écran suivant (avec bouclage)"),
      ("Hyper+b", "aller-retour entre les deux derniers workspaces — traverse les écrans"),
    ], widths=(20, 80))
  + note("tip", "l'ordre de priorité",
    "<code>'03_Work' = ['secondary', 'built-in']</code> se lit comme une liste de repli : AeroSpace prend le premier écran disponible dans l'ordre. Sur un poste à deux écrans externes, <code>secondary</code> gagne ; en déplacement, <code>built-in</code> prend le relais ; sur l'ultrawide seul, tout retombe sur <code>main</code>.")
))

# ══════════════════════════════════════════════════════════════════ 09
S.append(section("09", "La ligne de commande",
  "<p>Toute action liée à une touche existe aussi en commande — utile pour scripter, ou pour tester un comportement avant de le lier.</p>"
  + cmds([
      ("aerospace reload-config", "recharger — alias <code>ar</code>"),
      ("aerospace list-apps", "les applications en cours, avec leur <code>app-id</code>"),
      ("aerospace list-windows --all", "toutes les fenêtres, avec leur identifiant"),
      ("aerospace list-workspaces --all", "les workspaces déclarés"),
      ("aerospace list-monitors", "les écrans détectés et leur nom"),
      ("aerospace workspace 02_Coding", "aller à un workspace"),
      ("aerospace move-node-to-workspace 08_Ops", "y envoyer la fenêtre focalisée"),
      ("aerospace flatten-workspace-tree", "remettre l'arbre à plat"),
      ("aerospace balance-sizes", "rééquilibrer"),
      ("aerospace layout tiles accordion", "basculer la disposition"),
      ("aerospace debug-windows", "diagnostiquer une fenêtre qui refuse d'être tuilée"),
    ], widths=(44, 56))
  + "<h3>Les scripts maison</h3>"
  + cmds([
      ("aerospacectl", "pilotage global depuis un script"),
      ("appswitcher next · prev", "le commutateur ⌘Tab, qui traverse les espaces plein écran"),
      ("apppicker", "choisir une application au clavier"),
      ("spaceswitcher", "changer de workspace depuis l'extérieur"),
      ("gap-mode", "basculer entre plusieurs jeux de marges"),
      ("resize_thirds.sh one-third · two-thirds", "les ratios utilisés par <kbd>Hyper+u</kbd> et <kbd>Hyper+o</kbd>"),
    ], widths=(38, 62))
))

# ══════════════════════════════════════════════════════════════════ 10
S.append(section("10", "Dépannage",
  table(("symptôme", "cause", "geste"), [
      ("<b>Hyper+Espace ne fait rien sur un workspace</b>",
       "l'arbre a dégénéré — conteneurs à un seul enfant empilés",
       kbd("Hyper+a")+" puis "+kbd("r")+" (remettre à plat). Vérifier que les deux options <code>enable-normalization-*</code> sont à <code>true</code>"),
      ("<b>Les fenêtres ne se rangent plus toutes seules</b>",
       "même cause",
       "idem, puis <code>ar</code> pour recharger"),
      ("<b>Un raccourci de workspace ne mène nulle part</b>",
       "le workspace n'est pas déclaré et a disparu une fois vide",
       "l'ajouter à <code>persistent-workspaces</code> — obligatoire en <code>config-version = 2</code>"),
      ("<b>Une application refuse d'être tuilée</b>",
       "elle ne se déclare pas comme fenêtre standard",
       "<code>aerospace debug-windows</code> puis une règle <code>layout floating</code> si c'est irréductible"),
      ("<b>Une fenêtre part sur le mauvais écran</b>",
       "ancrage <code>workspace-to-monitor-force-assignment</code>",
       "<code>aerospace list-monitors</code> pour vérifier les noms réels"),
      ("<b>Une nouvelle fenêtre saute ailleurs</b>",
       "une règle <code>on-window-detected</code> la route",
       "seules les apps « destination » doivent être routées ; retirer la règle pour un terminal ou un éditeur"),
      ("<b>Le curseur ne suit plus</b>",
       "<code>on-focus-changed</code> retiré",
       "<code>move-mouse window-lazy-center</code>"),
      ("<b>Une bande vide en haut de l'écran</b>",
       "<code>outer.top</code> compense une barre qui n'existe plus",
       "la remettre à 12, comme les autres marges"),
    ], widths=(24, 30, 46), classes=["", "d", "d"])
))

# ══════════════════════════════════════════════════════════════════ 11
S.append(section("11", "Index des touches",
  "<p class=small>Hyper = <kbd>⌘</kbd> + <kbd>⌥</kbd> + <kbd>⌃</kbd> + <kbd>⇧</kbd>, omis dans cet index.</p>"
  + idx([
    ("Focus", [
        ("i", "haut"), ("j", "gauche"), ("k", "bas"), ("l", "droite"),
        ("←↓↑→", "les mêmes, aux flèches"),
        ("⌘⌥⌃+ijkl", "les mêmes, sans Shift"),
        ("b", "workspace précédent"),
        ("⌘+Tab", "application suivante"),
        ("⌘+⇧+Tab", "application précédente"),
        ("`", "—"),
    ]),
    ("Déplacer la fenêtre", [
        ("s", "gauche"), ("e", "haut"), ("d", "bas"), ("f", "droite"),
        ("n", "mode envoi"), ("n puis 1..9", "envoyer et suivre"),
        ("m", "workspace → autre écran"),
        ("x  ·  ;", "mode service"),
        ("service + ⌫", "fermer les autres fenêtres"),
        ("service + q", "fermer la fenêtre"),
    ]),
    ("Disposition & taille", [
        ("Espace", "tuiles ↔ accordéon"),
        ("Entrée", "flottant ↔ tuilé"),
        ("t", "plein écran"), ("g", "plein écran natif"),
        ("y", "rééquilibrer"), ("u", "au tiers"), ("o", "aux deux tiers"),
        ("a", "mode disposition"), ("r", "mode redimensionnement"),
        ("a puis r", "remettre l'arbre à plat"),
    ]),
    ("Workspaces", [
        ("1", "01_Main"), ("2", "02_Coding"), ("3", "03_Work"),
        ("4", "04_Management"), ("5", "05_I.A"), ("6", "06_Web"),
        ("7", "07_System"), ("8", "08_Ops"), ("9", "09_Divers"),
        ("0", "01_Main"),
    ]),
    ("Mode disposition (a)", [
        ("t", "tuiles"), ("a", "accordéon"), ("f", "flottant"),
        ("h", "horizontal"), ("v", "vertical"), ("s", "plein écran"),
        ("g", "plein écran natif"), ("b", "rééquilibrer"),
        ("r", "remettre à plat"), ("Échap", "revenir"),
    ]),
    ("Mode redimensionnement (r)", [
        ("i", "hauteur −50"), ("k", "hauteur +50"),
        ("j", "largeur −50"), ("l", "largeur +50"),
        ("1", "au tiers"), ("2", "aux deux tiers"),
        ("b", "rééquilibrer"), ("Échap", "revenir"),
        ("service + Échap", "recharger la config"),
        ("ar", "recharger, au terminal"),
    ]),
  ])
))

HERE = os.path.dirname(os.path.abspath(__file__))
cv = cover(
    "référence · gestionnaire de fenêtres",
    "AeroSpace",
    subtitle="usage approfondi",
    sub="Un arbre de tuiles piloté au clavier, sans jamais toucher la souris. "
        "Neuf workspaces nommés, quatre modes, et deux options de normalisation "
        "qui portent tout le comportement automatique.",
    toc=TOC,
    stats=[("9", "workspaces"), ("4", "modes"), ("12 px", "marges"), ("⌘⌥⌃⇧", "hyper")],
    meta_left=VER, meta_right=DATE,
)
open(os.path.join(HERE, "aerospace-cover.html"), "w").write(page("AeroSpace — couverture", cv, full_bleed=True))
open(os.path.join(HERE, "aerospace-body.html"), "w").write(page("AeroSpace — référence", "".join(S)))
print("aerospace-cover.html + aerospace-body.html")
