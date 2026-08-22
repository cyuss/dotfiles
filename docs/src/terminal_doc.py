# -*- coding: utf-8 -*-
"""Terminal & zsh — reference d'usage approfondi."""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from lib import *

VER = os.environ.get("ZSH_VER", "zsh 5.9")
DATE = os.environ.get("DOC_DATE", "2026-08-21")

TOC = [
    ("01", "L'architecture du shell"),
    ("02", "Le démarrage"),
    ("03", "La complétion"),
    ("04", "fzf — les widgets"),
    ("05", "L'historique — atuin"),
    ("06", "Se déplacer"),
    ("07", "Chercher & inspecter"),
    ("08", "Git au terminal"),
    ("09", "Python, Node, make"),
    ("10", "Les TUIs"),
    ("11", "Les scripts maison"),
    ("12", "Recettes"),
    ("13", "Dépannage"),
    ("14", "Index"),
]

S = []

# ══════════════════════════════════════════════════════════════════ 01
S.append(section("01", "L'architecture du shell",
  lede="Le principe directeur : <b>rien de coûteux au démarrage</b>. Sur une machine où l'on ouvre un pane herdr par tâche, chaque milliseconde du <code>.zshrc</code> est payée des dizaines de fois par jour.",
  body=""
  + "<h3>Qui lit quoi</h3>"
  + table(("fichier", "lu par", "contenu"), [
      (mono("~/.zshenv"), "<b>tous</b> les shells, y compris non interactifs — scripts Emacs, herdr, launchd, <code>zsh -c</code>", "PATH et variables d'environnement. <b>Rien d'autre ne doit y aller</b>, et rien de tout cela ne doit être ailleurs"),
      (mono("~/.zshrc"), "les shells interactifs", "prompt, plugins, complétion, raccourcis, intégrations"),
      (mono("~/.config/zsh/aliases.zsh"), "sourcé par <code>.zshrc</code>", "alias et fonctions — versionné avec le reste de la config"),
      (mono("~/.config/zsh/completion.zsh"), "sourcé par <code>.zshrc</code>, <b>après</b> compinit", "les trois couches de complétion (section 03)"),
      (mono("~/.config/zsh/completions/"), "ajouté au <code>fpath</code> <b>avant</b> compinit", "complétions générées par les outils eux-mêmes"),
    ], widths=(24, 30, 46), classes=["c", "d", "d"])
  + "<h3>L'ordre de chargement — il est contraignant</h3>"
  + steps([
      "<b>Prompt</b> — <code>oh-my-posh</code>, thème <code>amro</code>",
      "<b>antidote (fpath)</b> — les plugins qui n'apportent que des complétions. <b>Obligatoirement avant compinit</b>",
      "<b>fpath des complétions générées</b> — <code>~/.config/zsh/completions</code>. Même contrainte : <b>compinit ne relit pas le fpath une fois passé</b>",
      "<b>compinit</b> — le dump n'est revérifié qu'une fois par jour ; sinon <code>compinit -C</code>, qui saute la vérification de sécurité",
      "<b>antidote (plugins)</b> — les plugins normaux. <b>Après</b> compinit : fzf-tab l'exige",
      "<b>completion.zsh</b> — carapace, fzf-tab, autosuggestions. Après compinit <b>et</b> après les plugins",
    ])
  + note("warn", "l'erreur qui coûte le plus cher",
    "Placer le <code>fpath=(…)</code> des complétions générées dans <code>completion.zsh</code> plutôt que dans <code>.zshrc</code>. <code>completion.zsh</code> est sourcé <b>après</b> <code>compinit</code> : les fichiers <code>_uv</code>, <code>_ruff</code>, <code>_herdr</code> sont bien là, mais plus personne ne les regarde. Aucun message d'erreur — juste une complétion qui ne répond pas.")
  + note("note", "antidote plutôt qu'antigen",
    "antigen n'est plus maintenu depuis 2021. antidote génère un fichier <b>statique</b> (<code>~/.zsh_plugins.zsh</code>), régénéré uniquement si la liste a changé : le coût au démarrage est celui d'un simple <code>source</code>, pas d'une résolution de dépendances.")
))

# ══════════════════════════════════════════════════════════════════ 02
S.append(section("02", "Le démarrage",
  lede="Environ <b>310–340 ms</b>. Ce chiffre est le résultat d'un travail précis : les gestionnaires de versions coûtaient à eux seuls 750 ms par shell.",
  body=""
  + "<h3>Chargement paresseux</h3>"
  + table(("outil", "coût évité", "mécanisme"), [
      ("<b>nvm</b>", "593 ms par shell", "<code>node</code>, <code>npm</code>, <code>npx</code>, <code>yarn</code>, <code>pnpm</code>, <code>corepack</code> sont des <em>fonctions</em> qui, au premier appel, se suppriment, sourcent nvm et se relancent. Le binaire est dans le PATH tout de suite"),
      ("<b>jenv</b>", "153 ms par shell", "même schéma ; les shims Java sont déjà dans le PATH"),
      ("<b>chruby</b>", "—", "même schéma, sur <code>ruby</code>, <code>gem</code>, <code>bundle</code>, <code>rake</code>"),
      ("<b>carapace</b>", "un sous-processus par shell", "l'initialisation est écrite dans <code>~/.cache/carapace-init.zsh</code> et régénérée <b>seulement si le binaire est plus récent</b>"),
      ("<b>pyenv</b>", "—", "chargé normalement : un seul <code>eval</code>, qui gère aussi le PATH"),
      ("<b>conda</b>", "retiré", "<code>uv</code> fait le travail. L'installation existe toujours (<code>~/miniconda3</code>, 7,9 Go)"),
    ], widths=(14, 18, 68), classes=["", "d", "d"])
  + "<h3>Mesurer</h3>"
  + cmds([
      ("time zsh -i -c exit", "temps de démarrage d'un shell interactif"),
      ("bench 'zsh -i -c exit'", "<code>hyperfine --warmup 3</code> — une mesure statistique, pas un tirage"),
      ("zsh -xv -i -c exit 2>&1 | head -100", "voir ce qui est réellement exécuté, dans l'ordre"),
      ("zr", "<code>exec zsh</code> — recharger le shell dans la même fenêtre"),
    ], widths=(38, 62))
  + note("tip", "la bannière",
    "<code>fastfetch</code> ne s'affiche qu'<b>une fois par fenêtre de terminal</b> : la variable <code>FASTFETCH_SHOWN</code> est exportée, donc les sous-shells et les panes ouverts depuis cette session ne la réaffichent pas.")
))

# ══════════════════════════════════════════════════════════════════ 03
S.append(section("03", "La complétion",
  lede="Trois couches distinctes, souvent confondues. Les séparer est ce qui permet de diagnostiquer quand quelque chose ne complète pas.",
  body=""
  + flow([
    ("1", "les données", "Ce que zsh <em>sait</em> proposer. zsh-completions couvre le classique ; carapace ajoute 653 commandes modernes ; les outils absents génèrent la leur."),
    ("2", "la présentation", "Comment le menu s'affiche. fzf-tab remplace le menu natif par fzf, avec un aperçu à droite."),
    ("3", "les suggestions", "La ligne grisée qui devine la suite depuis l'historique. zsh-autosuggestions."),
  ])
  + "<h3>1 · Les données</h3>"
  + table(("source", "couvre", "note"), [
      ("<b>zsh-completions</b>", "le classique — git, ssh, tar, brew…", "via antidote, dans le <code>fpath</code>"),
      ("<b>carapace</b>", "<b>653 commandes</b> modernes : gh, docker, jj, aws, cargo, ollama…", "ponts <code>zsh,fish,bash</code> : il délègue aux systèmes existants quand il n'a pas de spec, au lieu de faire disparaître la complétion. <code>kubectl</code> est exclu — la complétion zsh native est meilleure"),
      ("<b>générées</b>", "uv · uvx · ruff · herdr · jj · just · glow · atuin", "les outils qui savent produire leur propre <code>_nom</code>. Régénérables par <code>zsh-completions-refresh</code>"),
    ], widths=(18, 32, 50), classes=["", "d", "d"])
  + "<h3>2 · La présentation — fzf-tab</h3>"
  + "<p>fzf-tab était chargé mais <b>sans aucun zstyle</b> : il tournait donc sans aperçu, et n'était qu'un menu un peu plus joli. Vingt règles lui donnent maintenant un aperçu adapté à chaque nature de complétion.</p>"
  + table(("on complète…", "l'aperçu montre"), [
      ("un <b>fichier</b>", "les 80 premières lignes, coloriées par <code>bat</code>"),
      ("un <b>dossier</b>", "l'arborescence sur 2 niveaux, par <code>eza</code>"),
      ("<code>cd</code> · <code>z</code>", "l'arborescence seulement — jamais de fichiers"),
      ("<code>git add</code> · <code>diff</code> · <code>restore</code> · <code>checkout</code>", "le diff réel du fichier, passé dans <code>delta</code>"),
      ("<code>git log</code> · <code>show</code> · <code>branch</code>", "les 20 derniers commits de la ref"),
      ("une <b>variable</b>", "sa valeur"),
      ("<code>kill</code> · <code>ps</code>", "la ligne de commande <b>complète</b>, pas le nom tronqué"),
      ("<code>brew install</code> · <code>info</code>", "les 30 premières lignes de <code>brew info</code>"),
      ("une cible <code>make</code>", "la description <code>##</code>, la recette, <b>et la valeur des variables</b>"),
      ("une recette <code>just</code>", "<code>just --show</code> — la recette et ses dépendances"),
    ], widths=(30, 70), classes=["", "d"])
  + keys([
      ("Tab", "ouvrir le menu de complétion"),
      ("Ctrl+/", "afficher / masquer l'aperçu"),
      ("/", "descendre dans le dossier sélectionné, sans quitter le menu"),
      ("< · >", "changer de groupe de complétion"),
      ("Ctrl+u · Ctrl+d", "faire défiler l'aperçu"),
    ], widths=(20, 80))
  + "<h3>3 · Les suggestions</h3>"
  + table(("réglage", "valeur", "pourquoi"), [
      ("<code>STRATEGY</code>", "<code>history completion</code>", "l'historique d'abord — précis, jamais surprenant. La complétion prend le relais sur une commande neuve"),
      ("<code>BUFFER_MAX_SIZE</code>", "60", "au-delà, la suggestion est plus longue que la ligne et gêne la lecture"),
      ("<code>MANUAL_REBIND</code>", "1", "ne suggère pas pendant un collage : la surbrillance recalculée à chaque caractère collé fait ramer un gros <em>paste</em>"),
      ("<code>HIGHLIGHT_STYLE</code>", "<code>fg=#5b6272</code>", "gris accordé au fond du terminal"),
    ], widths=(24, 20, 56), classes=["c", "c", "d"])
  + keys([
      ("→", "accepter toute la suggestion (en fin de ligne)"),
      ("Ctrl+Espace", "accepter la suggestion"),
      ("Ctrl+→", "accepter <b>un mot</b> de la suggestion"),
    ], widths=(20, 80))
))

# ══════════════════════════════════════════════════════════════════ 04
S.append(section("04", "fzf — les widgets",
  lede="C'est ici que la souris devient inutile. Cinq touches, cinq gestes qui remplacent chacun une série de <code>ls</code> / <code>cd</code> / copier-coller.",
  body=""
  + table(("touche", "widget", "ce qu'il fait", "aperçu"), [
      (kbd("Ctrl+o"), mono("fe"), "chercher un fichier n'importe où → l'ouvrir dans l'éditeur", "son contenu"),
      (kbd("Ctrl+f"), mono("fcd"), "chercher un dossier → <code>cd</code> dedans", "son arborescence"),
      (kbd("Ctrl+g"), mono("fbr"), "chercher une branche git → <code>switch</code>", "ses 20 derniers commits"),
      (kbd("Ctrl+p"), mono("fpj"), "chercher un projet dans <code>~/Desktop/projects</code> → <code>cd</code>", "son contenu, avec statut git"),
      (kbd("Ctrl+n"), mono("fmod"), "chercher un fichier <b>modifié</b> → l'ouvrir", "son diff"),
    ], widths=(13, 12, 47, 28), classes=["k", "c", "d", "d"])
  + "<h3>Les fonctions sans raccourci</h3>"
  + cmds([
      ("fshow", "chercher un commit dans les 300 derniers → afficher son diff complet"),
      ("fkill", "chercher un processus (via <code>procs</code>) → le tuer"),
      ("tve_", "television : chercher un fichier → l'ouvrir"),
      ("tvcd", "television : chercher un dossier → <code>cd</code>"),
    ], widths=W_CMD)
  + "<h3>Les réglages fzf</h3>"
  + cmds([
      ("FZF_DEFAULT_COMMAND", "<code>fd --type f --hidden --follow --exclude .git</code> — respecte le <code>.gitignore</code>"),
      ("FZF_ALT_C_COMMAND", "idem, sur les dossiers"),
      ("--height 60%", "n'occupe jamais tout l'écran : le contexte reste visible au-dessus"),
      ("--preview-window", "<code>right:55%:wrap:hidden</code> — <b>masqué par défaut</b>, révélé par <kbd>Ctrl+/</kbd>"),
      ("--bind", "<code>ctrl-/</code> bascule l'aperçu, <code>ctrl-u</code> / <code>ctrl-d</code> le font défiler"),
    ], widths=(30, 70))
  + note("tip", "l'aperçu masqué par défaut",
    "C'est délibéré : dans la majorité des cas on sait déjà ce qu'on cherche, et l'aperçu ne fait que ralentir l'affichage sur un gros dépôt. <kbd>Ctrl+/</kbd> le fait apparaître au moment où on hésite — c'est-à-dire au moment où il sert vraiment.")
))

# ══════════════════════════════════════════════════════════════════ 05
S.append(section("05", "L'historique — atuin",
  lede="atuin remplace l'historique zsh par une base SQLite : recherche plein texte, contexte (dossier, hôte, code de sortie, durée) et synchronisation entre machines.",
  body=""
  + mixed_pair("Dans le shell", [
      ("↑", "recherche atuin, filtrée sur le dossier courant"),
      ("Ctrl+r", "recherche atuin, sur tout l'historique"),
    ], "En commande", [
      ("hist", "<code>atuin search -i</code> — l'interface complète"),
      ("hstats", "<code>atuin stats</code> — les commandes les plus utilisées"),
    ])
  + note("key", "l'historique comme source de vérité",
    "Les alias de cette configuration ne sont pas devinés : ils sont construits à partir des statistiques atuin réelles. <code>brew</code> 517 appels, <code>make</code> 220, <code>ll</code> 215, <code>cd</code> 189, <code>doom</code> 149, <code>nvim</code> 67, <code>git</code> 57. C'est pourquoi <code>brew</code> a dix alias et que <code>mk</code> existe.")
  + cmds([
      ("atuin search <mot>", "chercher dans tout l'historique"),
      ("atuin search --cwd . <mot>", "restreindre au dossier courant"),
      ("atuin search --exit 0 <mot>", "seulement les commandes qui ont réussi"),
      ("atuin stats --count 20", "le classement des commandes"),
      ("hsync", "<code>atuin sync</code> — synchroniser entre machines"),
    ], widths=W_CMD_L)
))

# ══════════════════════════════════════════════════════════════════ 06
S.append(section("06", "Se déplacer",
  cmd_pair("Navigation", [
      ("j <mot>", "zoxide — saute vers un dossier déjà visité"),
      ("ji", "zoxide interactif (fzf)"),
      ("..  ...  ....", "remonter d'un, deux, trois niveaux"),
      ("-", "revenir au dossier précédent"),
      ("pj", "<code>~/Desktop/projects</code>"),
      ("cfg", "<code>~/.config</code>"),
      ("dl", "<code>~/Downloads</code>"),
      ("groot", "la racine du dépôt git courant"),
      ("mkcd <dir>", "créer le dossier <b>et</b> entrer dedans"),
    ], "Lister", [
      ("ls", "eza avec icônes, dossiers d'abord, statut git"),
      ("ll", "détaillé"),
      ("la", "détaillé, fichiers cachés compris"),
      ("l", "une colonne"),
      ("lt · lt3", "arborescence sur 2 · 3 niveaux"),
      ("ltg", "arborescence, en ignorant ce que git ignore"),
      ("lm", "triés par date de modification"),
      ("lsize", "triés par taille, les plus gros d'abord"),
      ("recent", "les 20 derniers modifiés"),
    ])
  + note("note", "ce qui n'est PAS masqué",
    "<code>grep</code>, <code>cat</code>, <code>du</code> et <code>ps</code> gardent leur binaire d'origine. Leurs remplaçants modernes ont des drapeaux différents — <code>rg -E</code> n'est pas <code>grep -E</code> — et les masquer casse les scripts autant que les réflexes. Les versions modernes ont leurs propres noms : <code>rg</code>, <code>bat</code>, <code>dust</code>, <code>procs</code>.")
  + "<h3>Explorateurs</h3>"
  + cmds([
      ("yz", "yazi — explorateur complet, aperçus, opérations par lot"),
      ("br", "broot — arborescence filtrable en direct, <code>:cd</code> pour y aller"),
      ("tvd", "television — sélecteur flou de dossiers"),
      ("lt3", "un simple coup d'œil sur trois niveaux"),
    ], widths=W_CMD)
))

# ══════════════════════════════════════════════════════════════════ 07
S.append(section("07", "Chercher & inspecter",
  cmd_pair("Chercher", [
      ("rg <motif>", "ripgrep — respecte le <code>.gitignore</code>"),
      ("rgi", "insensible à la casse"),
      ("rgf <motif>", "chercher dans les <b>noms</b> de fichiers"),
      ("rgh", "y compris cachés et ignorés"),
      ("fd <motif>", "trouver un fichier par son nom"),
      ("tvt", "television — recherche plein texte interactive"),
      ("navi", "antisèches interactives — alias <code>cheat</code>"),
    ], "Inspecter", [
      ("b <fichier>", "<code>bat --style=plain</code> — cat colorié"),
      ("bn", "avec les numéros de ligne"),
      ("dust", "arbre des tailles, lisible"),
      ("dush", "les 20 plus gros éléments du dossier"),
      ("loc", "<code>tokei</code> — lignes de code par langage"),
      ("jsonpp · yamlpp", "<code>jq .</code> · <code>yq .</code>"),
      ("versions", "node, python, uv, git, nvim, emacs d'un coup"),
    ])
  + "<h3>Les questions que tout dev se pose</h3>"
  + table(("la question", "la commande"), [
      ("Qui occupe le port 3000 ?", mono("port 3000")),
      ("Tue ce qui occupe le port 3000", mono("killport 3000")),
      ("Quels ports sont ouverts ?", mono("ports")),
      ("C'est un alias, une fonction ou un binaire ?", mono("whichall <nom>")),
      ("Quelle est mon IP ?", mono("myip") + " · " + mono("localip")),
      ("Combien pèse ce dossier ?", mono("dush")),
      ("Qu'est-ce qui a changé récemment ici ?", mono("recent")),
      ("Combien de lignes de code ?", mono("loc")),
      ("Mon PATH, lisible ?", mono("path")),
      ("Combien de temps prend cette commande ?", mono("bench '<cmd>'")),
      ("Extraire cette archive ?", mono("extract <fichier>") + " — tous formats"),
      ("Sauvegarder avant de bidouiller ?", mono("bak <fichier>") + " — horodaté"),
    ], widths=(50, 50), classes=["d", ""])
  + note("note", "le manuel",
    "<code>MANPAGER</code> passe les pages de manuel dans <code>bat</code> avec la coloration <code>man</code> : <code>man ffmpeg</code> devient lisible. C'est une variable, pas un alias — donc <code>git help</code> et tout ce qui appelle le pager en profite aussi.")
))

# ══════════════════════════════════════════════════════════════════ 08
S.append(section("08", "Git au terminal",
  cmd_pair("Au quotidien", [
      ("gs", "<code>status -sb</code> — l'état, en deux lignes"),
      ("gd · gds", "diff · diff des fichiers stagés"),
      ("gdt", "diff <b>syntaxique</b> (difftastic) — compare les AST"),
      ("gl", "log en graphe, 20 derniers"),
      ("glog", "log en graphe, complet et colorié"),
      ("ga · gaa", "ajouter · ajouter tout"),
      ("gcm \"…\"", "commiter avec un message"),
      ("gca", "amender sans rouvrir l'éditeur"),
      ("gp", "<code>pull --rebase</code> — historique linéaire"),
      ("gpu · gpf", "push · push <code>--force-with-lease</code>"),
    ], "Les gestes qui sauvent", [
      ("gundo", "défait le commit, <b>garde le travail</b>"),
      ("gwip", "tout ajouter et commiter « wip »"),
      ("gab", "<code>absorb --and-rebase</code> — range les correctifs"),
      ("gst · gstp", "remiser · reprendre"),
      ("groot", "remonter à la racine du dépôt"),
      ("lg", "<b>lazygit</b> — la revue complète"),
      ("fbr", "changer de branche, en flou"),
      ("fshow", "chercher un commit → son diff"),
      ("fmod", "chercher un fichier modifié → l'éditer"),
      ("ghpr", "<code>gh pr create --web</code>"),
    ])
  + note("warn", "jamais --force nu",
    "<code>gpf</code> est <code>push --force-with-lease</code>. La différence n'est pas cosmétique : <code>--force</code> écrase la branche distante sans regarder, <code>--force-with-lease</code> refuse si quelqu'un a poussé depuis ta dernière récupération. C'est la seule forme de force acceptable sur une branche partagée.")
  + note("key", "gab — la commande la moins connue et la plus utile",
    "<code>git absorb</code> regarde tes modifications non commitées, trouve <b>pour chaque bloc</b> le commit qui a introduit ces lignes, et crée les <code>fixup!</code> correspondants. <code>--and-rebase</code> les applique ensuite. Sur une branche de review où l'on corrige dix remarques, cela remplace dix <code>rebase -i</code> manuels.")
  + "<h3>jj (Jujutsu)</h3>"
  + cmds([
      ("jj l", "l'arbre des changements"),
      ("jj d · jj s", "diff · statut du changement courant"),
      ("jj ci -m \"…\"", "décrire le changement et en ouvrir un nouveau"),
      ("jj into <rev>", "déplacer le travail courant dans une révision existante"),
      ("jj out", "extraire le travail courant vers un nouveau changement"),
    ], widths=W_CMD)
))

# ══════════════════════════════════════════════════════════════════ 09
S.append(section("09", "Python, Node, make",
  cmd_pair("Python — uv", [
      ("uvs", "<code>uv sync</code> — aligner l'environnement"),
      ("uva · uvad", "ajouter une dépendance · de dev"),
      ("uvr <cmd>", "exécuter dans le venv"),
      ("py", "<code>uv run python</code>"),
      ("pt", "<code>uv run pytest -q</code>"),
      ("ruffc", "<code>ruff check --fix</code> puis <code>ruff format</code>"),
      ("venv", "créer un venv et l'activer"),
    ], "Node", [
      ("ni · nci", "<code>npm install</code> · <code>npm ci</code>"),
      ("nr <script>", "<code>npm run</code>"),
      ("nrd · nrb", "<code>run dev</code> · <code>run build</code>"),
      ("nrt", "<code>npm test</code>"),
      ("nsize", "poids de <code>node_modules</code>"),
    ])
  + note("note", "pourquoi pt et pas pytest",
    "<code>pt</code> est <code>uv run pytest -q</code>, jamais <code>pytest</code> nu. Un <code>pytest</code> direct hors projet uv attrape l'interpréteur système et échoue de manière déroutante — ou pire, réussit avec les mauvaises dépendances.")
  + "<h3>make &amp; just — 220 + 46 appels</h3>"
  + table(("commande", "ce que ça fait"), [
      (mono("mk"), "<b>fzf-make</b> — le catalogue interactif du projet. Lit Makefile, justfile, <code>package.json</code> et Taskfile, affiche la recette en aperçu, et garde un historique des cibles lancées"),
      (mono("make") + " " + kbd("Tab"), "les cibles réelles, avec en aperçu la description <code>##</code>, la recette, <b>et la valeur des variables</b> qu'elle utilise"),
      (mono("mtargets"), "lister les cibles d'un Makefile, même sans cible <code>help</code>"),
      (mono("m") + " · " + mono("mr") + " · " + mono("mt"), "<code>make</code> · <code>make run</code> · <code>make test</code>"),
      (mono("mdev") + " · " + mono("mg"), "<code>make dev</code> · <code>make gui</code>"),
      (mono("mf") + " · " + mono("mc"), "<code>make format</code> · <code>make clean</code>"),
      (mono("ju") + " · " + mono("jl"), "<code>just</code> · <code>just --list</code>"),
    ], widths=(22, 78), classes=["", "d"])
  + note("key", "l'aperçu des cibles make",
    "Il exploite une convention que tes Makefile suivent déjà : <code>cible: ## description</code>. <code>make-target-info</code> imprime la description, la recette, puis les variables résolues par <code>make -pn</code> — donc les <b>vraies</b> valeurs, pas ce qui est écrit dans le fichier. On voit ce que la commande va faire avant de la lancer.")
))

# ══════════════════════════════════════════════════════════════════ 10
S.append(section("10", "Les TUIs",
  lede="Des interfaces plein écran qui remplacent une série de commandes et de lectures de sortie. Toutes sont pilotables au clavier.",
  body=""
  + table(("outil", "alias", "remplace", "geste clé"), [
      ("<b>lazygit</b>", mono("lg"), "git add / diff / rebase -i", "<code>Espace</code> stage, <code>c</code> commit, <code>P</code> push, <code>?</code> aide"),
      ("<b>lazydocker</b>", mono("ldo"), "docker ps / logs / exec", "navigation aux flèches, <code>d</code> supprimer"),
      ("<b>btop</b>", mono("top"), "top / htop / iftop", "<code>f</code> filtrer, <code>k</code> tuer, <code>m</code> trier par mémoire"),
      ("<b>taproom</b>", mono("tap"), "brew list / outdated / info", "<code>tapo</code> ce qui est à mettre à jour, <code>tapsz</code> trier par poids"),
      ("<b>television</b>", mono("tv"), "un sélecteur flou pour tout", "<code>tvf</code> fichiers, <code>tvt</code> texte, <code>tvg</code> git-log, <code>tvb</code> branches"),
      ("<b>navi</b>", mono("cheat"), "les commandes qu'on réapprend tous les six mois", "<code>nvedit</code> pour ajouter les siennes"),
      ("<b>yazi</b>", mono("yz"), "Finder", "<code>y</code> copier, <code>p</code> coller, <code>.</code> fichiers cachés"),
      ("<b>atuin</b>", mono("hist"), "Ctrl+r natif", "filtres par dossier, hôte, code de sortie"),
      ("<b>fzf-make</b>", mono("mk"), "make help", "historique des cibles lancées"),
      ("<b>pk</b>", "—", "ps | grep | kill", "recherche floue sur la <b>ligne de commande complète</b>"),
      ("<b>dive</b>", "—", "docker history", "explorer une image couche par couche"),
      ("<b>act</b>", "—", "pousser pour tester un workflow", "exécuter GitHub Actions en local"),
    ], widths=(15, 10, 30, 45), classes=["", "c", "d", "d"])
  + note("tip", "les filtres taproom",
    "<code>brew</code> est ta commande n°1 avec 517 appels. <code>tapo</code> (obsolètes), <code>tape</code> (installés explicitement) et <code>tapsz</code> (triés par poids disque) répondent aux trois questions réelles : qu'est-ce qui doit être mis à jour, qu'est-ce que j'ai vraiment voulu installer, et qu'est-ce qui prend de la place.")
))

# ══════════════════════════════════════════════════════════════════ 11
S.append(section("11", "Les scripts maison",
  "<p><code>~/.config/workflow-tools/</code> — versionné avec le reste de la configuration.</p>"
  + "<h3>herdr</h3>"
  + cmds([
      ("herdr-open-project <nom>", "focaliser le space s'il existe, le monter sinon"),
      ("herdr-new <type> <nom>", "créer un space / une session / une quick session"),
      ("herdr-nav-create", "le pont appelé par le navigator sur un nom inexistant"),
      ("herdr-manage", "renommer / supprimer un space ou une session"),
      ("herdr-rename-session", "renommer une session, répertoire compris"),
      ("herdr-session-presets", "les gabarits de session"),
      ("herdr-sync-projects", "régénérer un template herdr-plus par dépôt"),
    ], widths=W_CMD_L)
  + "<h3>AeroSpace &amp; fenêtres</h3>"
  + cmds([
      ("appswitcher next · prev", "⌘Tab qui traverse les espaces plein écran natifs"),
      ("apppicker", "choisir une application au clavier"),
      ("spaceswitcher", "changer de workspace depuis un script"),
      ("aerospacectl", "pilotage global"),
      ("gap-mode", "basculer entre plusieurs jeux de marges"),
    ], widths=W_CMD_L)
  + "<h3>Système &amp; divers</h3>"
  + cmds([
      ("repos-status", "l'état git de tous les dépôts d'un coup"),
      ("net", "état du réseau, wifi, bluetooth"),
      ("pk", "chercher un processus et le tuer"),
      ("mdv [chemin]", "parcourir du Markdown rendu — wrapper autour de glow"),
      ("make-target-info <cible>", "description, recette et variables d'une cible make"),
      ("zsh-completions-refresh", "régénérer les complétions des outils"),
      ("workflow-on · workflow-off", "activer / désactiver l'ensemble de la chaîne"),
    ], widths=W_CMD_L)
  + note("note", "pourquoi mdv et pas glow directement",
    "Dans glow 3.0.0, le fichier de configuration <b>et</b> <code>GLAMOUR_STYLE</code> sont ignorés — vérifié en posant <code>width: 40</code> sans effet. Seul le drapeau <code>-s</code> est pris en compte. <code>mdv</code> est le wrapper qui le passe, avec le thème <code>premium-noir</code>.")
))

# ══════════════════════════════════════════════════════════════════ 12
S.append(section("12", "Recettes",
  grid([
      card("Reprendre un projet", None,
        "De zéro à un environnement complet, sans taper un chemin.",
        [("Ctrl+b p", "sélecteur de projets herdr-plus → le space se monte entier"),
         ("Ctrl+p", "ou, dans le shell : <code>fpj</code> pour un simple <code>cd</code>"),
         ("gs", "où en est le dépôt"),
         ("uvs", "aligner l'environnement Python"),
         ("mk", "voir ce que le projet propose comme commandes")], widths=(24, 76)),
      card("Traquer une régression", None,
        "Du symptôme au commit fautif.",
        [("rg <symptôme>", "trouver où ça se passe"),
         ("fshow", "chercher le commit qui a touché ça"),
         ("gdt", "diff syntaxique — ce qui a <em>vraiment</em> changé"),
         ("git bisect start", "et laisser <code>pt</code> trancher"),
         ("gundo", "défaire le commit sans perdre le travail")], widths=(24, 76)),
    ])
  + grid([
      card("Nettoyer la machine", None,
        "Ce qui prend de la place, et pourquoi.",
        [("dush", "les 20 plus gros éléments du dossier"),
         ("dust", "l'arbre des tailles"),
         ("tapsz", "les paquets brew triés par poids"),
         ("bclean", "<code>brew cleanup --prune=all</code> + <code>autoremove</code>"),
         ("nsize", "le poids de <code>node_modules</code>")], widths=(24, 76)),
      card("Un port occupé", None,
        "Le cas le plus fréquent, en deux commandes.",
        [("port 3000", "qui écoute"),
         ("killport 3000", "le libérer"),
         ("ports", "tout ce qui écoute sur la machine"),
         ("Ctrl+b Alt+k", "ou <code>pk</code> : chercher par ligne de commande complète"),
         ("", "")], widths=(24, 76)),
    ])
  + grid([
      card("Après une mise à jour d'outil", None,
        "Ce qu'il faut penser à régénérer.",
        [("bup", "<code>brew update &amp;&amp; brew upgrade</code>"),
         ("zsh-completions-refresh", "régénérer les complétions"),
         ("zr", "recharger le shell"),
         ("hr", "recharger herdr"),
         ("ar", "recharger AeroSpace")], widths=(30, 70)),
      card("Éditer la configuration", None,
        "Chaque fichier a son alias.",
        [("ze · zev", "<code>.zshrc</code> · <code>.zshenv</code>"),
         ("za", "les alias"),
         ("cfg", "aller dans <code>~/.config</code>"),
         ("Ctrl+b Alt+m", "relire la documentation en Markdown rendu"),
         ("alias | rg <mot>", "retrouver un alias oublié")], widths=(30, 70)),
    ])
))

# ══════════════════════════════════════════════════════════════════ 13
S.append(section("13", "Dépannage",
  table(("symptôme", "cause", "geste"), [
      ("<b>Une complétion ne répond pas</b>",
       "le <code>fpath</code> a été ajouté après <code>compinit</code>",
       "il doit être dans <code>.zshrc</code> <b>avant</b> <code>compinit</code>, pas dans <code>completion.zsh</code>"),
      ("<b>Une complétion est périmée</b>",
       "l'outil a été mis à jour, pas son fichier <code>_nom</code>",
       "<code>zsh-completions-refresh</code>"),
      ("<b>fzf-tab n'affiche aucun aperçu</b>",
       "les zstyles sont chargés avant les plugins",
       "<code>completion.zsh</code> doit être sourcé <b>après</b> antidote"),
      ("<b>Le shell démarre lentement</b>",
       "un gestionnaire de versions chargé au démarrage",
       "<code>zsh -xv -i -c exit</code> pour voir ce qui s'exécute ; le passer en fonction paresseuse"),
      ("<b>La mauvaise version d'un outil répond</b>",
       "un shim pyenv masque le binaire Homebrew",
       "<code>whichall <nom></code> montre toutes les résolutions dans l'ordre"),
      ("<b>Un gros collage fait ramer</b>",
       "les suggestions recalculées à chaque caractère",
       "<code>ZSH_AUTOSUGGEST_MANUAL_REBIND=1</code> est déjà posé — vérifier qu'il n'a pas été perdu"),
      ("<b>Le dump de complétion est corrompu</b>",
       "interruption pendant l'écriture",
       "<code>rm ~/.zcompdump*</code> puis <code>zr</code>"),
      ("<b>Une commande ne trouve pas son binaire dans Emacs ou herdr</b>",
       "le PATH est dans <code>.zshrc</code> et non dans <code>.zshenv</code>",
       "les shells non interactifs ne lisent que <code>.zshenv</code>"),
    ], widths=(24, 30, 46), classes=["", "d", "d"])
))

# ══════════════════════════════════════════════════════════════════ 14
S.append(section("14", "Index",
  idx([
    ("Raccourcis clavier", [
        ("Ctrl+o", "fichier → éditeur"), ("Ctrl+f", "dossier → cd"),
        ("Ctrl+g", "branche git"), ("Ctrl+p", "projet → cd"),
        ("Ctrl+n", "fichier modifié"), ("Ctrl+r", "historique atuin"),
        ("↑", "historique, dossier courant"), ("Ctrl+/", "aperçu fzf"),
        ("Ctrl+Espace", "accepter la suggestion"), ("Ctrl+→", "un mot de la suggestion"),
    ]),
    ("Navigation", [
        ("j / ji", "zoxide / interactif"), ("pj / cfg / dl", "projets / config / téléch."),
        ("..  ...", "remonter"), ("-", "dossier précédent"),
        ("groot", "racine du dépôt"), ("mkcd", "créer et entrer"),
        ("ll / la / l", "lister"), ("lt / lt3 / ltg", "arborescence"),
        ("lm / lsize", "par date / par taille"), ("yz / br", "yazi / broot"),
    ]),
    ("Git", [
        ("gs", "statut"), ("gd / gds", "diff / stagé"),
        ("gdt", "diff syntaxique"), ("gl / glog", "log"),
        ("gcm / gca", "commit / amend"), ("gp / gpu / gpf", "pull / push / force-lease"),
        ("gundo", "défaire le commit"), ("gab", "absorb + rebase"),
        ("gwip", "commit rapide"), ("lg", "lazygit"),
    ]),
    ("Python / Node / make", [
        ("uvs / uva / uvad", "sync / add / add dev"),
        ("uvr / py / pt", "run / python / pytest"),
        ("ruffc", "check --fix + format"), ("venv", "créer et activer"),
        ("ni / nr / nrd", "install / run / dev"),
        ("mk", "fzf-make"), ("m / mr / mt", "make / run / test"),
        ("mtargets", "lister les cibles"), ("ju / jl", "just / --list"),
        ("nsize", "poids node_modules"),
    ]),
    ("Inspection", [
        ("port / killport", "qui écoute / libérer"), ("ports", "tout ce qui écoute"),
        ("whichall", "alias, fonction ou binaire"), ("path", "PATH lisible"),
        ("dush / dust", "tailles"), ("loc", "lignes de code"),
        ("versions", "toutes les versions"), ("myip / localip", "adresses IP"),
        ("bench", "hyperfine"), ("recent", "derniers modifiés"),
    ]),
    ("TUIs & rechargement", [
        ("lg / ldo / top", "git / docker / système"),
        ("tap / tapo", "brew / obsolètes"),
        ("tv / tvf / tvt", "television"),
        ("cheat", "navi"), ("hist / hstats", "atuin"),
        ("zr / ze / za", "recharger / éditer zsh"),
        ("hr", "recharger herdr"), ("ar", "recharger AeroSpace"),
        ("ds / dd", "doom sync / doctor"),
        ("bup / bclean", "mettre à jour / nettoyer brew"),
    ]),
  ])
))

HERE = os.path.dirname(os.path.abspath(__file__))
cv = cover(
    "référence · ligne de commande",
    "Terminal",
    subtitle="zsh, de bout en bout",
    sub="L'architecture du shell, les trois couches de complétion, les widgets fzf "
        "et les outils qui remplacent la souris. Chaque réglage est justifié par "
        "une mesure ou par un bug qu'il corrige.",
    toc=TOC,
    stats=[("~320 ms", "démarrage"), ("653", "cmd. carapace"), ("20", "règles fzf-tab"), ("Alacritty", "terminal")],
    meta_left=f"{VER} · antidote · oh-my-posh",
    meta_right=DATE,
)
open(os.path.join(HERE, "terminal-cover.html"), "w").write(page("Terminal — couverture", cv, full_bleed=True))
open(os.path.join(HERE, "terminal-body.html"), "w").write(page("Terminal — référence", "".join(S)))
print("terminal-cover.html + terminal-body.html")
