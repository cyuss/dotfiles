# -*- coding: utf-8 -*-
"""Doom Emacs — reference d'usage approfondi. Genere doom-cover/body.html."""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from lib import *

VER = os.environ.get("DOOM_VER", "Doom Emacs 2.2.3 · Emacs 30.2")
DATE = os.environ.get("DOC_DATE", "2026-08-21")

TOC = [
    ("01", "Le poste de travail"),
    ("02", "Evil — le socle modal"),
    ("03", "Se déplacer dans le code"),
    ("04", "SPC — la carte du leader"),
    ("05", "Fenêtres, buffers, workspaces"),
    ("06", "Projets & recherche"),
    ("07", "Python — la chaîne complète"),
    ("08", "Le débogueur (dape)"),
    ("09", "Complétion & Copilot"),
    ("10", "Édition structurée"),
    ("11", "Git"),
    ("12", "LeetCode"),
    ("13", "Performance — les réglages"),
    ("14", "Dépannage"),
    ("15", "Index des touches"),
]

S = []

# ══════════════════════════════════════════════════════════════════ 01
S.append(section("01", "Le poste de travail",
  flow([
    ("1", "launchd", "Le LaunchAgent <code>com.youcef.doom-emacs</code> démarre <code>emacs --fg-daemon</code> à la session."),
    ("2", "daemon", "Un seul processus Emacs vit en permanence. Il porte les paquets, les serveurs LSP et l'historique."),
    ("3", "emacsclient", "Chaque fenêtre est un <em>frame</em> client. Ouverture instantanée : rien n'est rechargé."),
    ("4", "frame", "Fermer une fenêtre ne tue rien. Le daemon garde buffers, undo et sessions LSP."),
  ])
  + "<h3>Ouvrir Emacs</h3>"
  + cmds([
      ("ec", "nouvelle fenêtre graphique rattachée au daemon — <b>le geste par défaut</b>"),
      ("e / enw", "Emacs dans le terminal courant, même daemon"),
      ("edaemon", "relancer le daemon à la main s'il est tombé"),
      ("doom sync", "après toute modification de <code>init.el</code> ou <code>packages.el</code>"),
      ("doom doctor", "diagnostic complet de l'installation"),
      ("doom build", "recompiler les paquets (après mise à jour d'Emacs)"),
    ], widths=W_CMD)
  + note("warn", "ne jamais faire", "<code>SPC q q</code> tue le <b>daemon</b>, pas la fenêtre. Pour fermer une fenêtre et laisser le daemon vivre : <b>"+kbd("SPC q f")+"</b> (<code>delete-frame</code>). C'est la confusion la plus coûteuse du mode daemon.")
  + "<h3>Ce qui est activé</h3>"
  + grid([
      "<h4>Complétion & UI</h4>" + table(("module", "rôle"), [
          (mono("corfu +orderless +icons"), "complétion en ligne, popup"),
          (mono("vertico +icons"), "minibuffer, sélection floue"),
          (mono("doom · modeline · dashboard"), "l'apparence Doom"),
          (mono("ligatures"), "→ ⇒ ≠ ≥ rendues par la police"),
          (mono("indent-guides"), "colonnes d'indentation"),
          (mono("workspaces"), "espaces de travail persistants"),
          (mono("zen"), "mode sans distraction"),
          (mono("treemacs"), "tiroir de projet"),
      ], widths=(52, 48), classes=["c", "d"]),
      "<h4>Édition & outils</h4>" + table(("module", "rôle"), [
          (mono("evil +everywhere"), "modal partout, y compris magit"),
          (mono("lsp +eglot"), "eglot, pas lsp-mode"),
          (mono("tree-sitter"), "analyse syntaxique réelle"),
          (mono("lookup"), "<code>gd</code>, <code>gD</code>, <code>K</code>, docsets"),
          (mono("magit"), "sans <code>+forge</code> — 1,8 s au démarrage"),
          (mono("undo"), "undo-fu, sans <code>+tree</code>"),
          (mono("multiple-cursors"), "curseurs multiples"),
          (mono("snippets · file-templates"), "yasnippet"),
      ], widths=(52, 48), classes=["c", "d"]),
    ])
  + "<h3>Volontairement absents</h3>"
  + table(("module", "pourquoi", "remplacé par"), [
      (mono(":checkers syntax"), "aucun diagnostic à l'écran — décision assumée", "flymake sur demande, "+kbd("SPC t f")),
      (mono(":tools debugger"), "tire dap-mode <em>et</em> realgud pour le même service", "<code>dape</code>, natif DAP"),
      (mono(":editor format"), "reformatage automatique non désiré", "<code>ruffc</code> au terminal"),
      (mono("magit +forge"), "chargeait magit au démarrage", "<code>lazygit</code> et <code>gh</code>"),
      (mono(":emacs undo +tree"), "undo-tree rame sur gros tampons", "<code>vundo</code>, à la demande"),
      (mono(":term vterm"), "le terminal, c'est herdr", "—"),
    ], widths=(24, 46, 30), classes=["c", "d", "d"])
))

# ══════════════════════════════════════════════════════════════════ 02
S.append(section("02", "Evil — le socle modal",
  "<h3>Les états</h3>"
  + table(("état", "curseur", "on y entre par", "on en sort par"), [
      ("<b>Normal</b>", "bloc <span style='color:#6f5590'>orchidée</span>", kbd("Esc")+" · "+kbd("jk"), "—"),
      ("<b>Insert</b>", "barre <span style='color:#47703f'>verte</span>", kbd("i")+" "+kbd("a")+" "+kbd("o")+" "+kbd("c"), kbd("jk")+" · "+kbd("Esc")),
      ("<b>Visual</b>", "creux <span style='color:#9d6a22'>orange</span>", kbd("v")+" "+kbd("V")+" "+kbd("Ctrl+v"), kbd("Esc")),
      ("<b>Operator</b>", "—", "après "+kbd("d")+" "+kbd("c")+" "+kbd("y"), "le mouvement le termine"),
    ], widths=(16, 22, 32, 30), classes=["", "d", "k", "k"])
  + note("key", "réglages maison", "<code>evil-escape</code> : taper <b>"+kbd("j k")+"</b> en Insert revient en Normal — la touche Échap physique devient inutile. Délai 0,25 s. · <code>evil-want-fine-undo</code> : chaque mot tapé est un pas d'annulation, au lieu de la session Insert entière. · <code>C-u</code> défile d'une demi-page (comportement Vim), pas <code>universal-argument</code>. · Après chaque sauvegarde, retour automatique en Normal.")
  + "<h3>La grammaire : opérateur + mouvement</h3>"
  + "<p>Tout se compose. <code>d</code> (delete) + <code>i</code> (inner) + <code>(</code> = supprimer l'intérieur des parenthèses. Apprendre les trois colonnes ci-dessous, c'est apprendre des centaines de commandes.</p>"
  + mixed_pair("Opérateurs", [
          ("d", "supprimer"), ("c", "changer (supprime + Insert)"),
          ("y", "copier"), ("p · P", "coller après · avant"),
          ("> · <", "indenter · désindenter"),
          ("g c", "commenter"), ("g u · g U", "minuscules · majuscules"),
          ("=", "réindenter"), ("g q", "reformater le paragraphe"),
        ], "Objets texte", [
          ("i w · a w", "mot · mot + espace"),
          ("i ( · i [ · i {", "intérieur du délimiteur"),
          ("a ( · a [ · a {", "délimiteur compris"),
          ("i \" · i '", "intérieur de la chaîne"),
          ("i t · a t", "balise HTML/JSX"),
          ("i p · a p", "paragraphe"),
          ("i i", "bloc indenté — <b>Python</b>"),
          ("i f · a f", "fonction (tree-sitter)"),
          ("g g · G", "début · fin du tampon"),
        ])
  + "<h3>Mouvements qui font gagner du temps</h3>"
  + keys_pair("", [
          ("w · b", "mot suivant · précédent"),
          ("e · g e", "fin de mot suivant · précédent"),
          ("0 · ^ · $", "colonne 0 · 1er caractère · fin de ligne"),
          ("f x · F x", "sauter au prochain <code>x</code> · précédent"),
          ("t x · T x", "juste avant le prochain <code>x</code>"),
          (";  ·  ,", "répéter le <code>f/t</code> · à l'envers"),
          ("{ · }", "paragraphe précédent · suivant"),
          ("% ", "au délimiteur correspondant"),
        ], "", [
          ("Ctrl+d · Ctrl+u", "demi-page bas · haut <em>(animé)</em>"),
          ("Ctrl+o · Ctrl+i", "reculer · avancer dans les sauts"),
          ("z z · z t · z b", "recentrer · en haut · en bas"),
          ("H · M · L", "haut · milieu · bas de l'écran"),
          ("* · #", "chercher le mot sous le curseur"),
          ("n · N", "occurrence suivante · précédente"),
          ("'' ", "revenir où l'on était avant le saut"),
          ("g ;", "dernier endroit modifié"),
        ])
  + note("tip", "les répétitions", "<b>"+kbd(".")+"</b> rejoue la dernière modification : c'est la commande la plus rentable de Vim. Combinée à <code>*</code> puis <code>cgn</code> (changer la prochaine occurrence), elle remplace un chercher-remplacer interactif : <code>*</code> <code>cgn</code> <em>nouveau texte</em> <code>Esc</code> puis <code>.</code> <code>.</code> <code>.</code>")
))

# ══════════════════════════════════════════════════════════════════ 03
S.append(section("03", "Se déplacer dans le code",
  lede="Quatre échelles de déplacement, de la plus locale à la plus large. Choisir la bonne échelle est ce qui distingue un déplacement fluide d'une chasse au curseur.",
  body=""
  + "<h3>1 · À l'écran — avy</h3>"
  + "<p>avy affiche une lettre sur chaque cible visible ; on tape la lettre, on y est. Configuré ici sur <b>toutes les fenêtres</b> (<code>avy-all-windows t</code>) : le saut traverse les splits et déplace le focus avec lui.</p>"
  + keys([
      ("g a", "<b>le geste principal.</b> Taper autant de caractères qu'on veut, puis choisir l'étiquette (<code>avy-goto-char-timer</code>, seuil 0,35 s)"),
      ("SPC j j", "identique, via le leader"),
      ("SPC j c", "sauter par 2 caractères exactement"),
      ("SPC j l", "sauter à une ligne"),
      ("SPC j w", "sauter à un mot commençant par…"),
      ("SPC j r", "rejouer le dernier saut avy"),
      ("SPC j b", "revenir en arrière (<code>avy-pop-mark</code>)"),
      ("g s j · g s k", "motions Evil natives : ligne suivante / précédente étiquetée"),
    ], widths=(22, 78))
  + note("key", "avy est un opérateur, pas seulement un saut",
    "Pendant que les étiquettes sont affichées, une <em>action</em> peut être tapée à la place d'une étiquette, puis la cible choisie. C'est la moitié cachée du paquet :"
    + table(None, [
        (mono("x"), "tuer la ligne cible"), (mono("X"), "tuer la région cible"),
        (mono("t"), "téléporter la ligne ici"), (mono("m"), "déplacer la ligne ici"),
        (mono("y"), "copier la ligne"), (mono("Y"), "copier la région"),
        (mono("z"), "zapper jusqu'à la cible"), (mono("i"), "annuler"),
      ], widths=(8, 42, 8, 42), classes=["c", "d", "c", "d"]))
  + "<h3>2 · Dans le fichier — symboles</h3>"
  + keys([
      ("SPC s i", "<b>imenu</b> — liste des fonctions et classes du fichier, filtrable"),
      ("SPC s j", "imenu sur tous les tampons ouverts"),
      ("[ f · ] f", "fonction précédente · suivante"),
      ("[ m · ] m", "début de méthode précédente · suivante"),
      ("SPC s s", "chercher dans le tampon (<code>consult-line</code>)"),
      ("SPC s S", "chercher le symbole sous le curseur"),
      ("C-c o", "<b>combobulate</b> — navigation par arbre syntaxique (préfixe)"),
    ], widths=(22, 78))
  + note("note", "breadcrumb", "Le fil <em>fichier › classe › méthode</em> en haut de la fenêtre vient de <code>breadcrumb</code>, écrit par l'auteur d'eglot. Il lit imenu <em>et</em> les symboles LSP — il suit donc le code réel, pas une heuristique. Actif en <code>prog-mode</code> seulement.")
  + "<h3>3 · Vers la définition — LSP</h3>"
  + keys([
      ("g d", "aller à la définition"),
      ("g D", "toutes les références"),
      ("g I", "implémentations (<code>eglot-find-implementation</code>)"),
      ("g y", "définition du type (<code>eglot-find-typeDefinition</code>)"),
      ("K", "documentation en surimpression"),
      ("Ctrl+o", "<b>revenir</b> — après n'importe quel <code>gd</code>"),
      ("SPC c r", "renommer le symbole partout"),
      ("SPC c a", "actions de code (imports manquants, refactor)"),
      ("SPC c d", "aller à la définition · <kbd>SPC c D</kbd> références"),
      ("SPC c i", "hiérarchie d'implémentation"),
      ("SPC c j · SPC c J", "symbole du tampon · du projet"),
    ], widths=(22, 78))
  + "<h3>4 · Entre fichiers</h3>"
  + keys([
      ("SPC .", "trouver un fichier depuis le dossier courant"),
      ("SPC SPC", "fichier dans le projet — <b>le raccourci le plus utilisé</b>"),
      ("SPC f r", "fichiers récents"),
      ("SPC f f", "trouver un fichier n'importe où"),
      ("SPC ,", "changer de tampon (projet)"),
      ("SPC <", "changer de tampon (tous)"),
      ("SPC `", "revenir au tampon précédent"),
    ], widths=(22, 78))
))

# ══════════════════════════════════════════════════════════════════ 04
S.append(section("04", "SPC — la carte du leader",
  lede="Doom pose toutes ses commandes derrière <kbd>SPC</kbd>. La règle : la première lettre nomme la famille. Hésiter une demi-seconde après <kbd>SPC</kbd> fait apparaître which-key, qui liste la suite — c'est la documentation intégrée du clavier.",
  body=""
  + table(("préfixe", "famille", "ce qu'on y trouve"), [
      (kbd("SPC f"), "<b>file</b>", "ouvrir, enregistrer, renommer, supprimer, récents, copier le chemin"),
      (kbd("SPC b"), "<b>buffer</b>", "changer, tuer, sauver tout, revenir, tampon vierge"),
      (kbd("SPC p"), "<b>project</b>", "ouvrir un projet, chercher dedans, compiler, exécuter"),
      (kbd("SPC s"), "<b>search</b>", "dans le tampon, le projet, le dossier ; imenu ; iedit ; curseurs"),
      (kbd("SPC c"), "<b>code</b>", "LSP : renommer, actions, définition, erreurs, compiler, <code>make</code>"),
      (kbd("SPC g"), "<b>git</b>", "magit : statut, commit, branches, blame, historique du fichier"),
      (kbd("SPC w"), "<b>window</b>", "splits, focus, fermer, ace-window, zoom"),
      (kbd("SPC o"), "<b>open</b>", "treemacs, dired, terminal, vundo, agenda"),
      (kbd("SPC t"), "<b>toggle</b>", "zen, transparence, diagnostics, numéros, retour à la ligne"),
      (kbd("SPC h"), "<b>help</b>", "aide Emacs complète : fonctions, variables, touches, modules Doom"),
      (kbd("SPC j"), "<b>jump</b>", "avy — <em>ajouté ici</em>"),
      (kbd("SPC k"), "<b>kill/sexp</b>", "puni — édition structurée — <em>ajouté ici</em>"),
      (kbd("SPC d"), "<b>debugger</b>", "dape — <em>ajouté ici</em>"),
      (kbd("SPC l"), "<b>leetcode</b>", "<em>ajouté ici</em>"),
      (kbd("SPC P"), "<b>profiler</b>", "<em>ajouté ici</em> — majuscule, <code>SPC p</code> est pris"),
      (kbd("SPC TAB"), "<b>workspace</b>", "espaces de travail Doom"),
      (kbd("SPC q"), "<b>quit</b>", "fermer une fenêtre, sauver la session, <em>tuer Emacs</em>"),
    ], widths=(15, 17, 68), classes=["k", "", "d"])
  + "<h3>Les vingt raccourcis qui portent la journée</h3>"
  + keys_pair("", [
          ("SPC SPC", "fichier dans le projet"),
          ("SPC ,", "changer de tampon"),
          ("SPC .", "fichier dans le dossier"),
          ("SPC s p", "grep dans tout le projet"),
          ("SPC s s", "chercher dans le tampon"),
          ("SPC f s", "enregistrer"),
          ("SPC b k", "tuer le tampon"),
          ("SPC g g", "magit — statut"),
          ("SPC p p", "changer de projet"),
          ("SPC h r r", "recharger la config"),
        ], "", [
          ("SPC c r", "renommer le symbole"),
          ("SPC c a", "actions de code"),
          ("SPC c c", "compiler"),
          ("SPC w v · SPC w s", "split vertical · horizontal"),
          ("SPC w c", "fermer la fenêtre"),
          ("SPC w a", "ace-window — choisir au clavier"),
          ("SPC w z", "zoom sur la fenêtre courante"),
          ("SPC o p", "treemacs (tiroir de projet)"),
          ("SPC o u", "vundo — l'arbre d'annulation"),
          ("SPC t z", "mode zen"),
        ])
  + "<h3>Ajouts personnels — le détail</h3>"
  + keys_pair("SPC k — puni (structurel)", [
          ("SPC k f · b", "sexp suivant · précédent"),
          ("SPC k u · d", "remonter · descendre"),
          ("SPC k s · S", "avaler à droite · à gauche"),
          ("SPC k r · R", "recracher à droite · à gauche"),
          ("SPC k x", "supprimer les délimiteurs"),
          ("SPC k e", "remonter le sexp d'un cran"),
          ("SPC k k", "tuer jusqu'à la fin du sexp"),
          ("SPC k i", "vider l'intérieur du délimiteur"),
        ], "SPC s — recherche & curseurs", [
          ("SPC s e", "iedit — éditer toutes les occurrences"),
          ("SPC s l", "curseur sur chaque ligne de la région"),
          ("SPC s n · p", "marquer l'occurrence suivante · précédente"),
          ("SPC s a", "marquer toutes les occurrences"),
          ("SPC c ;", "comment-dwim-2"),
          ("SPC t f", "afficher / masquer les diagnostics"),
          ("SPC t t", "transparence de la fenêtre"),
          ("g h", "discover-my-major — touches du mode courant"),
        ])
))

# ══════════════════════════════════════════════════════════════════ 05
S.append(section("05", "Fenêtres, buffers, workspaces",
  "<h3>La hiérarchie</h3>"
  + flow([
    ("A", "buffer", "Un contenu en mémoire. Un fichier ouvert, un résultat de grep, magit."),
    ("B", "window", "Une vue sur un buffer. Plusieurs fenêtres peuvent montrer le même buffer."),
    ("C", "frame", "Une fenêtre du système. Chaque <code>ec</code> en crée une, sur le même daemon."),
    ("D", "workspace", "Un jeu de fenêtres nommé et persistant. <kbd>SPC TAB</kbd>."),
  ])
  + "<h3>Splits</h3>"
  + keys_pair("", [
          ("SPC w v", "split vertical — <em>côte à côte</em>"),
          ("SPC w s", "split horizontal — <em>l'un sous l'autre</em>"),
          ("SPC w V · SPC w S", "idem, en y déplaçant le focus"),
          ("SPC w c", "fermer la fenêtre courante"),
          ("SPC w o", "fermer <b>toutes les autres</b>"),
          ("SPC w u", "annuler la dernière disposition"),
        ], "", [
          ("Ctrl+w h j k l", "focus gauche / bas / haut / droite"),
          ("SPC w a", "ace-window — une lettre par fenêtre"),
          ("SPC w z", "zoom : la fenêtre prend tout, on la rend ensuite"),
          ("Ctrl+w H J K L", "<b>déplacer</b> la fenêtre dans cette direction"),
          ("Ctrl+w =", "rééquilibrer les tailles"),
          ("Ctrl+w +/-", "agrandir / rétrécir"),
        ])
  + note("tip", "quelle disposition, quand",
    "<b>Vertical</b> (<code>SPC w v</code>) pour comparer deux fichiers, ou code + tests : le code se lit en colonnes étroites sans gêne. "
    "<b>Horizontal</b> (<code>SPC w s</code>) pour un fichier + un terminal / un résultat de compilation : le second panneau n'a pas besoin de hauteur. "
    "<b>Trois fenêtres et plus</b> : passer par <code>SPC w a</code> plutôt que par <code>C-w</code> répétés — une lettre suffit à atteindre n'importe laquelle.")
  + "<h3>Workspaces</h3>"
  + keys([
      ("SPC TAB n", "nouveau workspace"),
      ("SPC TAB d", "supprimer le workspace"),
      ("SPC TAB r", "renommer"),
      ("SPC TAB 1..9", "aller au n°N"),
      ("SPC TAB TAB", "liste et sélection"),
      ("SPC TAB ] · [", "suivant · précédent"),
      ("SPC TAB s · l", "sauvegarder · charger une session"),
    ], widths=(22, 78))
  + note("note", "workspace Doom ou space herdr ?",
    "Les deux existent et ne se recouvrent pas. Le <b>space herdr</b> porte le contexte complet d'un projet : terminaux, agents, serveurs, git. Le <b>workspace Doom</b> porte une disposition de fenêtres <em>à l'intérieur</em> d'Emacs. En pratique : un space herdr par projet, et un workspace Doom quand on veut deux dispositions distinctes dans le même projet (par ex. « code » et « tests »).")
))

# ══════════════════════════════════════════════════════════════════ 06
S.append(section("06", "Projets & recherche",
  "<h3>Chercher — quatre portées</h3>"
  + table(("touche", "portée", "moteur", "quand"), [
      (kbd("SPC s s"), "tampon courant", "consult-line", "on sait que c'est dans ce fichier"),
      (kbd("SPC s p"), "<b>projet entier</b>", "ripgrep", "le geste de recherche par défaut"),
      (kbd("SPC s d"), "dossier courant", "ripgrep", "sous-arbre précis"),
      (kbd("SPC s b"), "tous les tampons", "consult", "on l'a vu tout à l'heure"),
      (kbd("SPC s i"), "symboles du fichier", "imenu", "aller à une fonction par son nom"),
      (kbd("SPC c j"), "symboles LSP", "eglot", "le serveur connaît le vrai symbole"),
    ], widths=(15, 20, 18, 47), classes=["k", "t", "c", "d"])
  + note("key", "affiner sans relancer",
    "Dans le minibuffer de <code>SPC s p</code>, <b>"+kbd("C-c C-e")+"</b> (ou <code>embark-export</code>) verse les résultats dans un tampon <em>grep</em> éditable : on modifie les lignes, <code>C-c C-c</code> écrit dans tous les fichiers. C'est le chercher-remplacer multi-fichiers d'Emacs — plus sûr qu'un <code>sed</code> parce qu'on relit avant d'écrire.")
  + "<h3>Projets</h3>"
  + keys([
      ("SPC p p", "changer de projet"),
      ("SPC p f", "fichier dans le projet"),
      ("SPC p a", "ajouter un projet connu"),
      ("SPC p d", "retirer un projet"),
      ("SPC p c", "compiler le projet"),
      ("SPC p r", "exécuter (<code>run</code>)"),
      ("SPC p t", "lancer les tests"),
      ("SPC p i", "invalider le cache — <em>après un déplacement massif de fichiers</em>"),
      ("SPC c m", "<code>+make/run</code> — choisir une cible du Makefile"),
    ], widths=(22, 78))
  + "<h3>Les fichiers, sans souris</h3>"
  + keys_pair("", [
          ("SPC o p", "treemacs — tiroir latéral"),
          ("SPC o P", "treemacs sur le fichier courant"),
          ("SPC o -", "dired dans le dossier courant"),
          ("SPC f f", "ouvrir n'importe où"),
          ("SPC f r", "récents"),
          ("SPC f p", "ouvrir un fichier de configuration Doom"),
        ], "", [
          ("SPC f y", "copier le chemin du fichier courant"),
          ("SPC f R", "renommer le fichier courant"),
          ("SPC f D", "supprimer le fichier courant"),
          ("SPC f u", "remonter d'un dossier"),
          ("SPC f e", "aller dans <code>~/.config/doom</code>"),
          ("SPC f P", "aller dans le dossier privé de Doom"),
        ])
))

# ══════════════════════════════════════════════════════════════════ 07
S.append(section("07", "Python — la chaîne complète",
  lede="C'est la partie la plus travaillée de cette configuration. Chaque maillon a été choisi pour une raison précise, et le venv du projet est détecté sans rien déclarer.",
  body=""
  + flow([
    ("1", "ouverture", "<code>python-mode-hook</code> déclenche <code>my/python-eglot-ensure</code>."),
    ("2", "venv", "<code>.venv</code> du projet détecté ; <code>exec-path</code> et <code>PATH</code> posés <b>localement au tampon</b>."),
    ("3", "serveur", "<code>basedpyright-langserver</code> démarre, via <code>emacs-lsp-booster</code>."),
    ("4", "édition", "Complétion, <code>gd</code>, <code>K</code>, renommage. Aucun diagnostic affiché."),
  ])
  + "<h3>Pourquoi ces choix</h3>"
  + table(("maillon", "décision", "raison"), [
      ("<b>basedpyright</b>", "remplace pyright", "fork maintenu, inférence plus stricte, installé par <code>uv tool</code> dans <code>~/.local/bin</code> — plus de shim pyenv à traverser à chaque démarrage de serveur"),
      ("<b>eglot-booster</b>", "activé", "le binaire convertit le JSON du serveur en bytecode elisp : Emacs n'analyse plus de JSON dans la boucle de complétion. Compilé en arm64 natif — les <em>releases</em> GitHub ne fournissent que x86_64"),
      ("<b>venv local</b>", "<code>setq-local</code>", "un <code>setenv</code> global empilait le venv de chaque projet ouvert sur le PATH du processus Emacs, et le mauvais python finissait par gagner partout"),
      ("<b>flymake</b>", "coupé à la source", "<code>eglot-stay-out-of</code> empêche eglot de le rallumer dans chaque nouveau tampon"),
      ("<b>tree-sitter</b>", "<code>python-ts-mode</code>", "arbre syntaxique réel : objets texte <code>i f</code>, indent-bars fidèles, combobulate"),
    ], widths=(16, 20, 64), classes=["", "c", "d"])
  + "<h3>Au quotidien</h3>"
  + mixed_pair("Dans Emacs", [
          ("g d · K", "définition · documentation"),
          ("SPC c r", "renommer dans tout le projet"),
          ("SPC c a", "action de code — import manquant"),
          ("SPC c f", "formater la région / le tampon"),
          ("SPC t f", "montrer les erreurs, le temps d'une relecture"),
          ("SPC p t", "lancer les tests du projet"),
          ("C-c o", "combobulate — préfixe structurel"),
        ], "Au terminal", [
          ("uvs", "<code>uv sync</code> — aligner l'environnement"),
          ("uva / uvad", "ajouter une dépendance / de dev"),
          ("uvr", "<code>uv run</code> — exécuter dans le venv"),
          ("py", "<code>uv run python</code>"),
          ("pt", "<code>uv run pytest -q</code>"),
          ("ruffc", "<code>ruff check --fix</code> puis <code>ruff format</code>"),
        ])
  + note("warn", "un venv non détecté",
    "Le venv doit s'appeler <code>.venv</code> et se trouver à la racine du projet — c'est ce que <code>my/python-venv-dir</code> cherche. S'il porte un autre nom, rien n'est appliqué et le serveur voit le python système. Vérifier avec <b>"+kbd("SPC h v")+"</b> puis <code>python-shell-interpreter</code> : le chemin affiché doit pointer dans le <code>.venv</code> du projet.")
))

# ══════════════════════════════════════════════════════════════════ 08
S.append(section("08", "Le débogueur (dape)",
  lede="dape parle DAP directement, sans dépendre de lsp-mode. Le module Doom <code>:tools debugger</code> reste désactivé parce qu'il tire dap-mode <em>et</em> realgud pour rendre le même service.",
  body=""
  + note("key", "prérequis, une seule fois par projet",
    "<code>uv add --dev debugpy</code>. L'adaptateur lance <code>python -m debugpy.adapter</code> ; comme <code>my/python-apply-local-venv</code> a déjà posé <code>exec-path</code> et <code>PATH</code> localement au tampon, <code>python</code> désigne le bon interpréteur sans rien déclarer. Si le paquet manque, dape le dit franchement : <em>module debugpy is not installed</em>.")
  + "<h3>La séquence</h3>"
  + steps([
      "Poser un point d'arrêt sur la ligne voulue : <b>"+kbd("SPC d b")+"</b>",
      "Démarrer : <b>"+kbd("SPC d d")+"</b> — dape demande la configuration (<code>debugpy</code>), puis le fichier ou le module",
      "L'exécution s'arrête. Les panneaux (pile, variables, points d'arrêt) s'ouvrent <b>à droite</b> ; le code reste à gauche",
      "Avancer : <b>"+kbd("SPC d n")+"</b> pas à pas, <b>"+kbd("SPC d i")+"</b> entrer dans l'appel, <b>"+kbd("SPC d o")+"</b> en sortir",
      "Inspecter une expression : <b>"+kbd("SPC d E")+"</b> — ou lire les valeurs affichées <em>en ligne</em> dans le code (<code>dape-inlay-hints</code>)",
      "Terminer : <b>"+kbd("SPC d q")+"</b>. Les points d'arrêt sont sauvegardés et rechargés au démarrage suivant",
    ])
  + "<h3>La carte SPC d</h3>"
  + keys_pair("", [
          ("SPC d d", "démarrer / continuer"),
          ("SPC d b", "poser / retirer un point d'arrêt"),
          ("SPC d c", "point d'arrêt <b>conditionnel</b>"),
          ("SPC d l", "point d'arrêt <b>traçant</b> (log, sans arrêt)"),
          ("SPC d B", "retirer tous les points d'arrêt"),
          ("SPC d r", "continuer"),
          ("SPC d R", "relancer la session"),
        ], "", [
          ("SPC d n", "pas à pas (par-dessus)"),
          ("SPC d i", "entrer dans l'appel"),
          ("SPC d o", "sortir de l'appel"),
          ("SPC d s", "panneaux pile / variables"),
          ("SPC d e", "REPL de débogage"),
          ("SPC d E", "évaluer une expression"),
          ("SPC d q", "quitter la session"),
        ])
  + note("tip", "le point d'arrêt traçant",
    "<b>"+kbd("SPC d l")+"</b> est sous-utilisé et souvent supérieur au pas-à-pas : il imprime une expression à chaque passage <em>sans interrompre l'exécution</em>. C'est un <code>print()</code> qu'on n'a pas à écrire, ni à retirer avant de committer.")
))

# ══════════════════════════════════════════════════════════════════ 09
S.append(section("09", "Complétion & Copilot",
  "<h3>Trois sources, un seul popup</h3>"
  + table(("source", "d'où ça vient", "réglage"), [
      ("<b>eglot</b>", "le serveur de langage — types réels, signatures", "premier dans <code>completion-at-point-functions</code>"),
      ("<b>cape-file</b>", "chemins de fichiers", "après eglot"),
      ("<b>cape-dabbrev</b>", "mots déjà présents dans les tampons ouverts", "dernier — le filet de sécurité"),
      ("<b>yasnippet</b>", "les snippets, par leur abréviation", "intégré à corfu"),
    ], widths=(16, 48, 36), classes=["", "d", "d"])
  + "<h3>corfu — le popup</h3>"
  + keys_pair("", [
          ("(automatique)", "s'ouvre après 1 caractère, délai 0,1 s"),
          ("TAB · S-TAB", "candidat suivant · précédent"),
          ("RET", "valider"),
          ("C-g", "fermer"),
        ], "", [
          ("C-SPC", "forcer l'ouverture"),
          ("M-d", "documentation du candidat"),
          ("M-l", "où est défini ce candidat"),
          ("C-M-i", "complétion manuelle (<code>completion-at-point</code>)"),
        ])
  + "<h3>Copilot — la suggestion grisée</h3>"
  + "<p>Elle apparaît en surimpression, de façon asynchrone : elle ne bloque jamais la frappe. Le délai est de <b>0,25 s</b> d'inactivité — assez pour ne pas déclencher pendant une frappe continue, assez peu pour être là dès qu'on marque une pause.</p>"
  + keys([
      ("TAB", "accepter toute la suggestion"),
      ("C-e", "accepter <b>une ligne</b> seulement"),
      ("M-f", "accepter <b>un mot</b> seulement"),
      ("C-] · C-[", "suggestion suivante · précédente"),
      ("C-g", "rejeter"),
      ("g /", "<b>demander</b> une suggestion, en mode Normal"),
    ], widths=(22, 78))
  + note("note", "où Copilot ne se déclenche pas",
    "<code>emacs-lisp</code>, <code>lisp-interaction</code> (le tampon <code>*scratch*</code>), <code>org</code>, <code>markdown</code>, <code>vterm</code>, <code>eshell</code>, <code>fundamental</code>. L'exclusion est testée <b>avant</b> le <code>require</code> — c'est ce qui empêche le serveur node de démarrer au lancement du daemon, quand seul <code>*scratch*</code> existe.")
  + "<h3>Snippets</h3>"
  + keys([
      ("(abréviation) TAB", "développer le snippet"),
      ("SPC i s", "insérer un snippet depuis la liste"),
      ("SPC c S", "créer un snippet pour le mode courant"),
      ("TAB · S-TAB", "champ suivant · précédent dans le snippet"),
    ], widths=(24, 76))
))

# ══════════════════════════════════════════════════════════════════ 10
S.append(section("10", "Édition structurée",
  lede="Trois outils qui se recouvrent en apparence, mais dont chacun a un domaine propre.",
  body=""
  + grid([
      card("puni", None, "Raisonne sur les <b>délimiteurs</b>. Excellent en Lisp, JSON, JSX ; presque inutile en Python, où les blocs sont de l'indentation.",
        [("SPC k s", "avaler l'élément suivant dans le sexp"),
         ("SPC k r", "le recracher"),
         ("SPC k e", "remonter le sexp d'un cran"),
         ("SPC k x", "supprimer les délimiteurs"),
         ("SPC k i", "vider l'intérieur")], widths=(32, 68)),
      card("combobulate", "C-c o", "Lit l'<b>arbre tree-sitter</b>. C'est lui qui rend le structurel utilisable en Python, YAML et JSON — là où puni n'a rien à saisir.",
        [("C-c o n · p", "nœud frère suivant · précédent"),
         ("C-c o u", "remonter au parent"),
         ("C-c o d", "descendre dans l'enfant"),
         ("C-c o t", "afficher l'arbre"),
         ("C-c o ↑ ↓", "déplacer le bloc entier")], widths=(32, 68)),
    ])
  + "<h3>Curseurs multiples</h3>"
  + table(("outil", "touche", "quand l'utiliser"), [
      ("<b>evil-multiedit</b>", kbd("M-d"), "occurrences du mot sous le curseur, une par une — le plus fluide en Evil"),
      ("<b>iedit</b>", kbd("SPC s e"), "toutes les occurrences d'un coup, avec restriction possible à la fonction"),
      ("<b>mc/edit-lines</b>", kbd("SPC s l"), "un curseur sur chaque ligne d'une sélection visuelle"),
      ("<b>mc/mark-next</b>", kbd("SPC s n"), "ajouter l'occurrence suivante à la sélection"),
      ("<b>mc/mark-all</b>", kbd("SPC s a"), "toutes les occurrences du tampon"),
      ("<b>gn + .</b>", kbd("* c g n"), "sans curseurs multiples : changer une occurrence, puis <kbd>.</kbd> pour chaque suivante"),
    ], widths=(20, 18, 62), classes=["", "k", "d"])
  + "<h3>Annuler — vundo</h3>"
  + "<p>Le module <code>:emacs undo</code> tourne <b>sans</b> <code>+tree</code> : undo-fu s'appuie sur l'annulation native d'Emacs. undo-tree gardait un arbre en mémoire en permanence, et sa sérialisation se corrompait. vundo, lui, ne <em>dessine</em> l'historique que lorsqu'on l'ouvre.</p>"
  + keys([
      ("SPC o u", "ouvrir l'arbre d'annulation"),
      ("h · l", "reculer · avancer dans le temps"),
      ("j · k", "changer de <b>branche</b> — c'est tout l'intérêt"),
      ("RET", "valider l'état choisi"),
      ("q", "quitter"),
      ("u · C-r", "annuler · refaire, sans ouvrir l'arbre"),
    ], widths=(22, 78))
))

# ══════════════════════════════════════════════════════════════════ 11
S.append(section("11", "Git",
  lede="magit est chargé sans <code>+forge</code> : ce drapeau chargeait magit au démarrage (1,8 s mesurée) et recouvrait ce que lazygit et <code>gh</code> font déjà mieux au terminal.",
  body=""
  + keys_pair("Dans Emacs — magit", [
          ("SPC g g", "<b>statut</b> — le point d'entrée"),
          ("SPC g /", "magit dispatch (toutes les commandes)"),
          ("SPC g b", "blame — qui a écrit cette ligne"),
          ("SPC g f f", "historique du fichier courant"),
          ("SPC g L", "log du dépôt"),
          ("SPC g c c", "commit"),
          ("SPC g B", "changer de branche"),
          ("SPC g t", "time machine sur le fichier"),
        ], "Dans le statut magit", [
          ("TAB", "déplier / replier un diff"),
          ("s · u", "stager · déstager le bloc sous le curseur"),
          ("S · U", "stager · déstager tout"),
          ("c c", "commiter — <code>C-c C-c</code> valide le message"),
          ("P p", "pousser"),
          ("F p", "tirer"),
          ("b b", "changer de branche"),
          ("? ", "l'aide contextuelle complète"),
        ])
  + note("tip", "le geste qui change tout",
    "Dans le statut magit, se placer sur une <b>ligne</b> d'un diff (pas sur le fichier) et taper <b>"+kbd("s")+"</b> : seule cette ligne est stagée. C'est le <em>staging par blocs</em> — il permet de découper un travail en cours en plusieurs commits propres sans rien réécrire. En visuel (<kbd>v</kbd>), on sélectionne exactement les lignes voulues.")
  + "<h3>Au terminal — ce qui est plus rapide dehors</h3>"
  + cmds([
      ("lg", "lazygit — la revue complète, la mise en scène et les rebases interactifs"),
      ("gs", "<code>git status -sb</code> — coup d'œil"),
      ("gd · gds", "diff · diff des fichiers stagés — passent par <code>delta</code>"),
      ("gdt", "<code>git dft</code> — diff syntaxique (difftastic), lit l'AST"),
      ("gab", "<code>git absorb --and-rebase</code> — range les corrections dans les bons commits"),
      ("gundo", "<code>git reset --soft HEAD~1</code> — défait le commit, garde le travail"),
      ("gpf", "<code>push --force-with-lease</code> — jamais <code>--force</code> nu"),
      ("ghpr", "<code>gh pr create --web</code>"),
    ], widths=W_CMD)
))

# ══════════════════════════════════════════════════════════════════ 12
S.append(section("12", "LeetCode",
  lede="Le tampon de code, l'énoncé et les tests côte à côte, dans Emacs. Les solutions sont écrites dans <code>~/Desktop/projects/leetcode-challenges/solutions</code>, donc versionnées et relisibles.",
  body=""
  + keys_pair("Dans Emacs", [
          ("SPC l l", "liste des problèmes"),
          ("SPC l d", "problème du jour"),
          ("SPC l r", "rafraîchir la liste"),
          ("SPC l t", "lancer les tests"),
          ("SPC l s", "soumettre"),
          ("SPC l q", "quitter"),
          ("q · r", "dans la liste : quitter · rafraîchir"),
        ], "Au terminal", [
          ("lc", "ouvrir Emacs directement sur la liste"),
          ("lcd", "aller dans le dépôt"),
          ("lcs", "statistiques — résolus par pattern"),
          ("lcn <pattern> <id> <slug>", "nouveau problème depuis le gabarit"),
        ])
  + note("note", "langage et sauvegarde",
    "<code>python3</code> par défaut (<code>mysql</code> pour le SQL), <code>leetcode-save-solutions</code> actif, le code ouvert en <code>python-ts-mode</code> — donc avec eglot, tree-sitter et indent-bars, exactement comme un fichier de projet.")
))

# ══════════════════════════════════════════════════════════════════ 13
S.append(section("13", "Performance — les réglages",
  lede="Chaque ligne de cette section correspond à une mesure, pas à une intuition. C'est ce qui rend l'ensemble tenable sur de gros fichiers.",
  body=""
  + table(("réglage", "valeur", "ce que ça résout"), [
      ("<code>read-process-output-max</code>", "4 Mo", "le débit LSP : par défaut 4 Ko, soit un aller-retour par fragment"),
      ("<code>process-adaptive-read-buffering</code>", "nil", "supprime la latence ajoutée par la lecture adaptative sur les gros flux"),
      ("<code>eglot-booster</code>", "actif", "le JSON du serveur arrive en bytecode elisp — plus d'analyse JSON dans la boucle"),
      ("<code>NumberOfFiles</code> (plist)", "16384", "<code>launchctl limit maxfiles</code> vaut 256 : les watchers d'eglot épuisaient les descripteurs et le serveur mourait en pleine session"),
      ("<code>redisplay-skip-fontification-on-input</code>", "t", "ne recolore pas pendant qu'on tape vite"),
      ("<code>display-line-numbers-type</code>", "t", "numéros <b>absolus</b> : <code>relative</code> redessine toute la marge à chaque mouvement"),
      ("<code>+big-file-lines</code>", "2000", "au-delà : plus de numéros, plus de ligatures, plus de guides"),
      ("<code>alpha-background</code>", "70", "composé par le serveur de fenêtres macOS — coût côté Emacs nul (0,60 ms/car. mesuré, identique sans)"),
      ("<code>gc-cons-threshold</code>", "(Doom)", "Doom le gère (16 Mo après démarrage). Mesuré en session réelle : 3 collectes, 0,1 s au total — la GC n'est pas la source de lenteur ici"),
    ], widths=(30, 12, 58), classes=["", "c", "d"])
  + "<h3>Mesurer une lenteur pendant qu'elle se produit</h3>"
  + note("key", "le seul protocole fiable",
    "Aucune mesure sans interface n'est crédible pour du rendu interactif : la lenteur doit être capturée <em>pendant</em> qu'elle a lieu, dans une vraie fenêtre.")
  + steps([
      "<b>"+kbd("SPC P s")+"</b> — démarre le profileur (cpu + mémoire)",
      "Reproduire la lenteur pendant 10 à 20 secondes",
      "<b>"+kbd("SPC P r")+"</b> — ouvre le rapport et arrête le profileur. <kbd>TAB</kbd> déplie les branches ; la fonction coupable est en haut de la branche la plus lourde",
      "<b>"+kbd("SPC P x")+"</b> — remettre les compteurs à zéro avant une nouvelle mesure",
    ])
  + "<h3>Défilement animé</h3>"
  + "<p>Sans paquet supplémentaire. Emacs 29+ fournit <code>pixel-scroll-precision-mode</code> et son moteur d'interpolation — le même que celui du trackpad. <code>evil-scroll-down</code> et <code>evil-scroll-up</code> y sont simplement redirigés.</p>"
  + note("note", "pourquoi c'est sans risque",
    "L'interpolation ne tourne que pendant l'exécution d'une <em>commande</em> de défilement. Elle n'ajoute rien au rendu, rien à un déplacement de curseur, rien à la frappe. Le seul coût est la durée de l'animation elle-même : <code>+smooth-scroll-time</code> = <b>0,10 s</b>. Mettre cette variable à 0 désactive tout.")
))

# ══════════════════════════════════════════════════════════════════ 14
S.append(section("14", "Dépannage",
  table(("symptôme", "cause probable", "geste"), [
      ("<b>La complétion s'arrête en pleine session</b>",
       "le serveur LSP est mort — descripteurs de fichiers épuisés",
       "<code>M-x eglot-reconnect</code>. Vérifier <code>launchctl limit maxfiles</code> et le <code>NumberOfFiles</code> du plist"),
      ("<b>« Copilot server started » à chaque lancement</b>",
       "un tampon <code>prog-mode</code> existe au démarrage du daemon",
       "l'exclusion par mode passe avant le <code>require</code> — vérifier <code>+copilot-excluded-modes</code>"),
      ("<b>Le mauvais python est utilisé</b>",
       "pas de <code>.venv</code> à la racine, ou un autre nom",
       kbd("SPC h v")+" <code>python-shell-interpreter</code> pour voir lequel est actif"),
      ("<b>Emacs se ferme entièrement</b>",
       kbd("SPC q q")+" tue le daemon",
       "utiliser "+kbd("SPC q f")+" pour fermer une fenêtre ; relancer avec <code>edaemon</code>"),
      ("<b>Un paquet manque après édition</b>",
       "<code>packages.el</code> modifié sans synchronisation",
       "<code>doom sync</code> puis "+kbd("SPC h r r")),
      ("<b>Des erreurs rouges reviennent</b>",
       "un mode a rallumé flymake",
       kbd("SPC t f")+" bascule ; <code>+no-flymake-h</code> est le filet posé sur <code>prog-mode</code>"),
      ("<b>Police illisible ou absente</b>",
       "changement de taille appliqué à toutes les fenêtres",
       kbd("SPC h r f")+" recharge la police depuis <code>config.el</code>"),
      ("<b>Lenteur sur un fichier précis</b>",
       "gros fichier, ou lignes très longues",
       "<code>+maybe-lighten-buffer-h</code> agit au-delà de 2000 lignes ou 512 Ko ; sinon profiler"),
      ("<b>tree-sitter inactif dans un langage</b>",
       "grammaire non compilée",
       "<code>M-x treesit-install-language-grammar</code>"),
    ], widths=(24, 30, 46), classes=["", "d", "d"])
  + "<h3>Les commandes d'inspection</h3>"
  + keys_pair("", [
          ("SPC h r r", "recharger la configuration"),
          ("SPC h r f", "recharger la police"),
          ("SPC h v", "décrire une variable"),
          ("SPC h f", "décrire une fonction"),
          ("SPC h k", "décrire une touche"),
          ("SPC h m", "décrire le mode courant"),
        ], "", [
          ("g h", "discover-my-major — tout le mode d'un coup"),
          ("SPC h d h", "documentation Doom"),
          ("SPC h p", "décrire un paquet"),
          ("SPC h e", "le tampon <code>*Messages*</code>"),
          ("M-x eglot-events-buffer", "le dialogue brut avec le serveur LSP"),
          ("M-x doom/info", "état complet de l'installation"),
        ])
))

# ══════════════════════════════════════════════════════════════════ 15
S.append(section("15", "Index des touches",
  idx([
    ("Fichiers & tampons", [
        ("SPC SPC", "fichier du projet"), ("SPC .", "fichier du dossier"),
        ("SPC f f", "ouvrir n'importe où"), ("SPC f r", "récents"),
        ("SPC f s", "enregistrer"), ("SPC f y", "copier le chemin"),
        ("SPC f R", "renommer"), ("SPC f D", "supprimer"),
        ("SPC ,", "tampon du projet"), ("SPC <", "tous les tampons"),
        ("SPC `", "tampon précédent"), ("SPC b k", "tuer le tampon"),
    ]),
    ("Recherche", [
        ("SPC s s", "dans le tampon"), ("SPC s p", "dans le projet"),
        ("SPC s d", "dans le dossier"), ("SPC s i", "imenu"),
        ("SPC s e", "iedit"), ("SPC s l", "curseur par ligne"),
        ("SPC s n / p", "occurrence suiv. / préc."), ("SPC s a", "toutes les occurrences"),
        ("* / #", "mot sous le curseur"), ("n / N", "occurrence suiv. / préc."),
    ]),
    ("Sauts", [
        ("g a", "avy (timer)"), ("SPC j j", "avy (timer)"),
        ("SPC j c", "avy 2 caractères"), ("SPC j l", "avy ligne"),
        ("SPC j w", "avy mot"), ("SPC j r", "rejouer"),
        ("SPC j b", "revenir"), ("Ctrl+o / Ctrl+i", "reculer / avancer"),
        ("g ;", "dernière modification"), ("''", "avant le saut"),
    ]),
    ("Code / LSP", [
        ("g d", "définition"), ("g D", "références"),
        ("g I", "implémentations"), ("g y", "type"),
        ("K", "documentation"), ("SPC c r", "renommer"),
        ("SPC c a", "actions"), ("SPC c f", "formater"),
        ("SPC c m", "make"), ("C-c o", "combobulate"),
    ]),
    ("Fenêtres", [
        ("SPC w v / s", "split vert. / horiz."), ("SPC w c", "fermer"),
        ("SPC w o", "fermer les autres"), ("SPC w a", "ace-window"),
        ("SPC w z", "zoom"), ("Ctrl+w h j k l", "focus"),
        ("Ctrl+w H J K L", "déplacer"), ("Ctrl+w =", "rééquilibrer"),
        ("SPC TAB n", "nouveau workspace"), ("SPC TAB 1..9", "workspace N"),
    ]),
    ("Git", [
        ("SPC g g", "statut magit"), ("SPC g b", "blame"),
        ("SPC g f f", "historique du fichier"), ("SPC g L", "log"),
        ("SPC g B", "branche"), ("SPC g t", "time machine"),
        ("s / u", "stager / déstager"), ("c c", "commit"),
        ("P p / F p", "push / pull"), ("?", "aide magit"),
    ]),
    ("Débogueur", [
        ("SPC d d", "démarrer"), ("SPC d b", "point d'arrêt"),
        ("SPC d c", "conditionnel"), ("SPC d l", "traçant"),
        ("SPC d n", "pas à pas"), ("SPC d i / o", "entrer / sortir"),
        ("SPC d s", "panneaux"), ("SPC d e", "REPL"),
        ("SPC d E", "évaluer"), ("SPC d q", "quitter"),
    ]),
    ("Structurel", [
        ("SPC k f / b", "sexp suiv. / préc."), ("SPC k u / d", "parent / enfant"),
        ("SPC k s / r", "avaler / recracher"), ("SPC k e", "remonter"),
        ("SPC k x", "supprimer délimiteurs"), ("SPC k i", "vider l'intérieur"),
        ("M-d", "evil-multiedit"), ("SPC o u", "vundo"),
        ("u / Ctrl+r", "annuler / refaire"), ("SPC c ;", "comment-dwim-2"),
    ]),
    ("Bascules & outils", [
        ("SPC t z", "zen"), ("SPC t t", "transparence"),
        ("SPC t f", "diagnostics"), ("SPC o p", "treemacs"),
        ("SPC o -", "dired"), ("SPC P s / r", "profiler start / rapport"),
        ("SPC l l", "leetcode"), ("g h", "discover-my-major"),
        ("g /", "copilot à la demande"), ("SPC h r r", "recharger"),
    ]),
  ])
))

# ── assemblage ──────────────────────────────────────────────────────
HERE = os.path.dirname(os.path.abspath(__file__))

cv = cover(
    "référence · poste de travail",
    "Doom&nbsp;Emacs",
    subtitle="usage approfondi",
    sub="Le poste tel qu'il est réellement configuré : daemon, Evil, eglot et basedpyright, "
        "dape, avy, combobulate. Chaque réglage documenté avec la raison qui l'a motivé.",
    toc=TOC,
    stats=[("15", "sections"), ("9", "modules clés"), ("0", "diagnostic affiché"), ("70 %", "opacité")],
    meta_left=VER,
    meta_right=DATE,
)

open(os.path.join(HERE, "doom-cover.html"), "w").write(page("Doom Emacs — couverture", cv, full_bleed=True))
open(os.path.join(HERE, "doom-body.html"), "w").write(page("Doom Emacs — référence", "".join(S)))
print("doom-cover.html + doom-body.html")
