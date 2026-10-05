# -*- coding: utf-8 -*-
"""herdr — reference d'usage approfondi."""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from lib import *

VER = os.environ.get("HERDR_VER", "herdr 0.8.2")
DATE = os.environ.get("DOC_DATE", "2026-08-21")

TOC = [
    ("01", "Le modèle mental"),
    ("02", "La grammaire des touches"),
    ("03", "Spaces & tabs"),
    ("04", "Panes & splits"),
    ("05", "Le navigator"),
    ("06", "herdr-plus — projets"),
    ("07", "Agents — états & quotas"),
    ("08", "Les plugins installés"),
    ("09", "Popups & outils"),
    ("10", "Sessions, worktrees, jj"),
    ("11", "La ligne de commande"),
    ("12", "Dépannage"),
    ("13", "Index des touches"),
]

S = []

# ══════════════════════════════════════════════════════════════════ 01
S.append(section("01", "Le modèle mental",
  lede="herdr est un gestionnaire d'espaces de travail conçu autour des agents. Ce n'est pas « tmux avec des couleurs » : le serveur connaît l'état de chaque agent et sait lequel t'attend.",
  body=""
  + flow([
    ("A", "server", "Un processus. Il survit à la fermeture du terminal ; tout est restauré à la reconnexion."),
    ("B", "space", "Un contexte de travail complet. En pratique : un projet. Porte sa branche git et son état."),
    ("C", "tab", "Une disposition de panes à l'intérieur d'un space. Se nomme tout seul."),
    ("D", "pane", "Un terminal. Peut héberger un agent, que herdr surveille."),
  ])
  + "<h3>Ce qui distingue herdr d'un multiplexeur classique</h3>"
  + table(("capacité", "ce que ça change concrètement"), [
      ("<b>États d'agent</b>", "Le serveur sait si un agent <em>travaille</em>, <em>attend une réponse</em>, est <em>inactif</em> ou a <em>fini</em>. La sidebar les trie, et une notification macOS native arrive quand l'un d'eux te bloque."),
      ("<b>Sidebar structurée</b>", "Deux panneaux : les spaces (avec branche git et statut) et les agents (avec contexte consommé et quota restant). Aucune commande à taper pour savoir où en est le travail."),
      ("<b>Plugins</b>", "Un vrai système d'extensions avec actions bindables. 21 sont installés ici — navigator, reviewr, memex, usagebar, floax…"),
      ("<b>Templates déclaratifs</b>", "Un fichier TOML par projet décrit tabs, panes et commandes de démarrage. Le space se monte entier en une touche."),
      ("<b>Worktrees git</b>", "Un space peut être adossé à un worktree dédié, pour faire travailler un agent sans polluer ta copie de travail."),
    ], widths=(19, 81), classes=["", "d"])
  + note("key", "le geste central", "<b>"+kbd("Ctrl+b w")+"</b> — le <em>navigator</em>. Quand on ne sait pas où est la chose (un space, un agent, une session, un projet), c'est toujours la bonne touche. Tout le reste de ce document en découle.")
))

# ══════════════════════════════════════════════════════════════════ 02
S.append(section("02", "La grammaire des touches",
  lede="Le préfixe est <kbd>Ctrl+b</kbd>. Cette configuration s'écarte des défauts herdr sur un point de principe : <b>aucune majuscule dans les gestes quotidiens</b>.",
  body=""
  + "<h3>Le principe</h3>"
  + table(("règle", "exemple", "pourquoi"), [
      ("<b>La lettre = le mot</b>", kbd("prefix+n")+" = <em>new</em>, "+kbd("prefix+c")+" = <em>create</em>, "+kbd("prefix+w")+" = <em>where</em>", "aucune table à mémoriser : le nom de l'action donne la touche"),
      ("<b>Pas de Shift</b>", "les paires vont sur des touches voisines : "+kbd("[")+" "+kbd("]")+", "+kbd(",")+" "+kbd("."), "le défaut herdr mettait <code>next_tab</code> sur <code>prefix+n</code> et <code>new_workspace</code> sur <code>prefix+shift+n</code> — deux actions très différentes sur la même lettre, dont une avec Shift"),
      ("<b>Alt = variante</b>", kbd("prefix+g")+" lazygit en popup, "+kbd("prefix+alt+v")+" en pane latéral", "le second niveau, pour tout ce qui est outil plutôt que navigation"),
      ("<b>Sans préfixe = traversant</b>", kbd("alt+h")+" "+kbd("alt+j")+" "+kbd("alt+k")+" "+kbd("alt+l"), "traverse les panes herdr <em>et</em> les splits Neovim de façon continue"),
    ], widths=(18, 32, 50), classes=["", "d", "d"])
  + "<h3>Les six familles</h3>"
  + table(("famille", "touches", "contenu"), [
      ("<b>Spaces</b>", kbd("n")+" "+kbd("[")+" "+kbd("]")+" "+kbd("/")+" "+kbd("'")+" "+kbd("g"), "créer, parcourir, renommer, fermer, choisir"),
      ("<b>Tabs</b>", kbd("c")+" "+kbd(",")+" "+kbd(".")+" "+kbd(";")+" "+kbd("1..9"), "créer, parcourir, renommer, aller au n°N"),
      ("<b>Panes</b>", kbd("s")+" "+kbd("v")+" "+kbd("i j k l")+" "+kbd("x")+" "+kbd("z"), "diviser, se déplacer, fermer, zoomer"),
      ("<b>Agents</b>", kbd("alt+,")+" "+kbd("alt+.")+" "+kbd("alt+1..9"), "parcourir, sauter au n°N"),
      ("<b>Navigation</b>", kbd("w")+" "+kbd("alt+w")+" "+kbd("alt+i")+" "+kbd("p")+" "+kbd("h"), "navigator, projets, space quotidien"),
      ("<b>Outils</b>", kbd("alt+g")+" "+kbd("alt+d")+" "+kbd("alt+b")+" "+kbd("alt+k")+" "+kbd("alt+m"), "git, docker, système, processus, docs"),
    ], widths=(15, 33, 52), classes=["", "k", "d"])
  + note("warn", "touches à ne jamais utiliser dans un picker",
    "<b>"+kbd("Ctrl+s")+"</b> est XOFF : le terminal gèle son affichage jusqu'à un <kbd>Ctrl+q</kbd> (XON). C'est exactement le bug qui faisait « disparaître » le texte du navigator pendant la frappe. "
    "Sont également piégés : <b>"+kbd("Ctrl+q")+"</b> (XON), <b>"+kbd("Ctrl+c")+"</b> (SIGINT), <b>"+kbd("Ctrl+z")+"</b> (SIGTSTP), <b>"+kbd("Ctrl+d")+"</b> (EOF). "
    "Vérifier avec <code>stty -a</code> : la ligne <code>stop = ^S</code> confirme que <code>ixon</code> est actif.")
))

# ══════════════════════════════════════════════════════════════════ 03
S.append(section("03", "Spaces & tabs",
  keys_pair("Spaces", [
      ("prefix+n", "nouveau space"),
      ("prefix+g", "sélecteur de spaces"),
      ("prefix+shift+w", "goto — saut direct"),
      ("prefix+[ · prefix+]", "précédent · suivant, sans sélecteur"),
      ("prefix+shift+1..9", "aller au space n°N"),
      ("prefix+/", "renommer le space"),
      ("prefix+'", "fermer le space"),
      ("prefix+h", "space <b>quotidien</b> (hors projet)"),
      ("prefix+p", "monter un space de projet"),
    ], "Tabs", [
      ("prefix+c", "nouveau tab"),
      ("prefix+1..9", "aller au tab n°N"),
      ("prefix+, · prefix+.", "précédent · suivant"),
      ("prefix+;", "renommer le tab"),
      ("prefix+alt+, · alt+.", "agent précédent · suivant"),
      ("prefix+alt+1..9", "sauter à l'agent n°N"),
      ("prefix+`", "aller-retour entre deux panes"),
      ("prefix+z", "zoom sur le pane courant"),
      ("prefix+shift+r", "recharger la configuration"),
    ])
  + note("note", "les tabs se nomment tout seuls",
    "<code>prompt_new_tab_name</code> et <code>prompt_new_workspace_name</code> sont à <code>false</code> : rien ne demande de nom à la création. Le plugin <code>herdr-automatic-rename</code> nomme le tab d'après ce qui y tourne. <code>hide_tab_bar_when_single_tab</code> fait disparaître la barre quand il n'y a qu'un onglet — donc, dans un space à un seul tab, aucun chrome inutile.")
  + "<h3>Le space quotidien</h3>"
  + "<p>Tout ce qui n'appartient à aucun projet : l'état des dépôts, le système, les fichiers, les mises à jour. Une seule touche — <b>"+kbd("prefix+h")+"</b> — et il apparaît ou reprend le focus.</p>"
  + note("key", "pourquoi un script et pas la commande directe",
    "<code>herdr-plus open</code> crée <b>toujours</b> un nouveau space (<code>workspaceCreate</code>) : appuyer deux fois donnait deux « quotidien ». Le binding passe donc par <code>herdr-open-project quotidien</code>, qui focalise l'existant s'il est là. Le chemin est absolu dans la config : un binding <code>type = \"shell\"</code> tourne détaché et n'hérite pas forcément du PATH du shell de connexion.")
))

# ══════════════════════════════════════════════════════════════════ 04
S.append(section("04", "Panes & splits",
  lede="Deux schémas de déplacement coexistent délibérément, un par réflexe. Ils ne se concurrencent pas : ils servent deux moments différents.",
  body=""
  + table(("schéma", "touches", "quand", "portée"), [
      ("<b>AeroSpace</b>", kbd("prefix+i")+" "+kbd("j")+" "+kbd("k")+" "+kbd("l"), "quand la main vient de bouger une fenêtre macOS", "identique à <kbd>Hyper+ijkl</kbd> dans AeroSpace : même cluster, même sens (i&nbsp;haut, j&nbsp;gauche, k&nbsp;bas, l&nbsp;droite). Seul le modificateur change"),
      ("<b>nvim</b>", kbd("alt+h")+" "+kbd("alt+j")+" "+kbd("alt+k")+" "+kbd("alt+l"), "<b>tous les jours</b>", "sans préfixe. Traverse les panes herdr <em>et</em> les splits Neovim de façon continue (plugin <code>vim-herdr-navigation</code>)"),
    ], widths=(13, 22, 22, 43), classes=["", "k", "d", "d"])
  + note("warn", "pourquoi pas Ctrl+hjkl",
    "herdr capte ces touches <b>sans préfixe</b>. Résultat : <kbd>Ctrl+l</kbd> n'atteignait plus le shell (effacer l'écran) et <kbd>Ctrl+h</kbd> n'atteignait plus tmux — et c'est aussi la touche Retour arrière. D'où <kbd>Alt</kbd>.")
  + "<h3>Diviser</h3>"
  + keys_pair("Splits", [
      ("prefix+s", "split <b>horizontal</b> — le nouveau pane dessous"),
      ("prefix+v", "split <b>vertical</b> — le nouveau pane à côté"),
      ("prefix+x", "fermer le pane"),
      ("prefix+z", "zoom / dézoom"),
      ("prefix+r", "mode redimensionnement"),
    ], "Se déplacer", [
      ("prefix+i j k l", "focus haut / gauche / bas / droite"),
      ("alt+h j k l", "idem, sans préfixe, traverse nvim"),
      ("prefix+`", "aller-retour entre deux panes"),
      ("prefix+shift+p", "renommer le pane"),
      ("h j k l", "en mode <em>navigate</em> : déplacements locaux"),
    ])
  + note("tip", "la convention nvim",
    "<code>prefix+s</code> et <code>prefix+v</code> reprennent exactement <code>:sp</code> et <code>:vs</code> de Vim : <b>s</b> pose le pane <em>dessous</em>, <b>v</b> le pose <em>à côté</em>. C'est contre-intuitif la première fois — « split horizontal » désigne la ligne de séparation, pas la direction de l'empilement — mais c'est le même réflexe que dans l'éditeur, ce qui est le seul critère qui compte.")
))

# ══════════════════════════════════════════════════════════════════ 05
S.append(section("05", "Le navigator",
  lede="Le point d'entrée universel. Un seul picker flou pour les spaces, les agents, les sessions, les serveurs, les plugins et les projets — avec des filtres pour restreindre la portée.",
  body=""
  + keys_pair("Ouvrir", [
      ("prefix+w", "plein écran — <b>le geste par défaut</b>"),
      ("prefix+alt+w", "en pane latéral, à côté du travail"),
      ("prefix+alt+i", "revenir d'où l'on vient après un saut"),
    ], "Dans le picker", [
      ("Ctrl+w", "filtrer sur les <b>spaces</b>"),
      ("Ctrl+a", "filtrer sur les <b>agents</b>"),
      ("Ctrl+l", "filtrer sur les <b>sessions</b>"),
    ])
  + "<h3>Les filtres</h3>"
  + table(("touche", "filtre", "ce qu'on y trouve"), [
      (kbd("Ctrl+w"), "<b>workspace</b>", "tous les spaces ouverts, avec leur branche"),
      (kbd("Ctrl+a"), "<b>agent</b>", "tous les agents, avec leur état"),
      (kbd("Ctrl+l"), "<b>session</b>", "les sessions enregistrées — <em>l</em> pour <em>list</em>"),
      (kbd("Ctrl+g"), "<b>server</b>", "les serveurs herdr joignables"),
      (kbd("Ctrl+n"), "<b>intégrations</b>", "les créations maison <b>et</b> les 61 actions des 20 plugins installés"),
      (kbd("Ctrl+p"), "<b>project</b>", "les templates herdr-plus, montables en un RET"),
    ], widths=(13, 17, 70), classes=["k", "", "d"])
  + note("note", "le filtre Ctrl+N ne s'appelait « plugin » que par accident",
    "Il montre la source <code>Source::Integration</code>, c'est-à-dire les blocs <code>[[integrations]]</code> de la config du navigator — <b>pas</b> les plugins herdr. D'où l'impression qu'il n'y contenait que la gestion des spaces. "
    "Une intégration <code>actions</code> y verse maintenant les <b>61 actions</b> réellement exposées par les 20 plugins (<code>herdr plugin action list</code>). Les actions du navigator lui-même en sont retirées — les invoquer depuis le navigator ouvrirait un navigator dans le navigator. "
    "Autre chemin vers la même chose : <b>"+kbd("prefix+a")+"</b>, la palette de commandes.")
  + note("warn", "pourquoi ces lettres et pas les initiales",
    "Le schéma « initiale de la catégorie » plaçait les sessions sur <b>"+kbd("Ctrl+s")+"</b>. Or <kbd>Ctrl+s</kbd> est XOFF : le terminal gelait son affichage et le texte semblait disparaître pendant la frappe. Les sessions sont donc sur <kbd>Ctrl+l</kbd> et les serveurs sur <kbd>Ctrl+g</kbd>.")
  + "<h3>Créer, renommer, supprimer depuis le navigator</h3>"
  + steps([
      "Ouvrir le navigator : <b>"+kbd("prefix+w")+"</b>",
      "Taper un nom qui n'existe pas encore — l'entrée <em>create</em> apparaît en bas de liste",
      "<kbd>RET</kbd> lance <code>herdr-nav-create</code>, qui demande le type (space, session, quick) puis monte l'objet via <code>herdr-new</code>",
      "Pour renommer ou supprimer : <b>"+kbd("Ctrl+n")+"</b> sur l'entrée choisie, ou le popup dédié <b>"+kbd("prefix+alt+e")+"</b> (<code>herdr-manage</code>)",
    ])
  + "<h3>La vue arborescente — les tabs sous leur space</h3>"
  + "<p>Comme <code>choose-tree</code> dans tmux : les tabs d'un space apparaissent comme lignes enfant, dans la section <b>open</b>, juste sous lui.</p>"
  + pre('<span class="a">open</span>   [3] project: un-projet-long        <span class="c">2 tabs · 3 panes</span>\n'
        '<span class="a">open</span>     ├ agent                         <span class="c">tab 1</span>\n'
        '<span class="a">open</span>     └ run                           <span class="c">tab 2 · 2 panes</span>')
  + table(("détail", "comment ça marche"), [
      ("<b>L'ordre</b>", "les lignes enfant sont insérées juste après leur space. Le tri du picker retombe sur l'ordre d'insertion à score égal — c'est ce qui fait tenir l'arbre à requête vide"),
      ("<b>La recherche</b>", "le libellé du space est dans le <em>haystack</em> de chaque tab : taper le nom du space fait remonter le space <b>et</b> ses tabs"),
      ("<b>Ctrl+X</b>", "sur une ligne enfant, ferme le <b>tab</b> — pas le space. Sans ce garde-fou, viser un tab fermerait tout ce qu'il contient"),
      ("<b>Spaces à un seul tab</b>", "pas d'enfant : l'unique tab ferait doublon avec la ligne du space. <code>tab_children_min = 1</code> les affiche quand même"),
      ("<b>La numérotation</b>", "la ligne enfant affiche la <b>position</b> (« tab 2 »), pas le champ <code>number</code>. <code>number</code> est un compteur de création qui garde les trous des tabs fermés : un space où l'on a fermé un tab affiche <code>[2]</code> sur un tab dont <code>number</code> vaut 4. Et <code>switch_tab = \"prefix+1..9\"</code> est une liaison <b>indexée</b> — elle vise la position"),
      ("<b>Désactiver</b>", "<code>tab_children = false</code> dans <code>[picker]</code>"),
    ], widths=(22, 78), classes=["", "d"])
  + note("warn", "c'est un correctif local du plugin, pas une option d'origine",
    "Une <code>[[integrations]]</code> ne peut pas produire ce résultat : le plugin n'interprète <code>kind</code> que pour <code>server</code> / <code>remote-terminal</code> (→&nbsp;Source::Server) et <code>session</code> (→&nbsp;Source::Session) ; tout le reste tombe dans Source::Integration. <b>Aucun chemin ne mène à la section <em>open</em>.</b> "
    "Le correctif est dans <code>~/.config/herdr/patches/</code>. <b>Une mise à jour du plugin l'écrase</b> — le README y donne les trois commandes pour le réappliquer. En cas d'échec, le plugin d'origine fonctionne normalement, sans l'arborescence.")
  + note("note", "les sessions ne sont pas imbriquées, et ce n'est pas un oubli",
    "Une <b>session</b> est un serveur herdr séparé, pas un enfant d'un space : elle a sa propre fenêtre et son propre jeu de spaces. L'imbriquer serait faux. Elle garde son filtre, <kbd>Ctrl+L</kbd>. La hiérarchie réelle est <em>session → space → tab → pane</em> ; le navigator en montre maintenant deux niveaux d'un coup.")
  + note("note", "sources désactivées",
    "<code>zoxide</code>, <code>roots</code> et <code>quick</code> sont éteints dans <code>herdr-navigator/config.toml</code> : ils noyaient la liste sous des chemins récents sans rapport avec le travail en cours. Les projets sont placés <b>en dernier</b> dans <code>source_order</code>, pour que les spaces réellement ouverts remontent en tête. zoxide reste accessible séparément sur <b>"+kbd("prefix+alt+z")+"</b>.")
))

# ══════════════════════════════════════════════════════════════════ 06
S.append(section("06", "herdr-plus — projets & actions",
  lede="Un fichier TOML par projet décrit le space entier : tabs, panes, répertoire, commandes de démarrage. Une touche, et l'environnement est monté.",
  body=""
  + keys_pair("Projets", [
      ("prefix+p", "sélecteur de projets, groupé par <code>group</code>"),
      ("prefix+w Ctrl+p", "les mêmes, dans le navigator"),
      ("prefix+h", "le space quotidien (hors projet)"),
    ], "Actions rapides", [
      ("prefix+alt+a", "lanceur flou dans le répertoire courant"),
      ("prefix+a", "palette de commandes (tout ce qui est bindable)"),
      ("prefix+alt+p", "gestionnaire de plugins"),
    ])
  + "<h3>Anatomie d'un template</h3>"
  + pre('<span class="c"># ~/.config/herdr/plugins/config/cloudmanic.herdr-plus/projects/my-app.toml</span>\n'
        '<span class="k">name</span>  = <span class="s">"my-app"</span>\n'
        '<span class="k">group</span> = <span class="s">"python"</span>          <span class="c"># regroupe le picker</span>\n'
        '<span class="k">root</span>  = <span class="s">"~/Desktop/projects/my-app"</span>\n\n'
        '<span class="a">[[tabs]]</span>\n'
        '<span class="k">name</span>    = <span class="s">"code"</span>\n'
        '<span class="k">command</span> = <span class="s">"nvim ."</span>\n\n'
        '<span class="a">[[tabs]]</span>\n'
        '<span class="k">name</span>    = <span class="s">"agent"</span>\n'
        '<span class="k">command</span> = <span class="s">"claude"</span>\n\n'
        '<span class="a">[[tabs]]</span>\n'
        '<span class="k">name</span>    = <span class="s">"serveur"</span>\n'
        '<span class="k">command</span> = <span class="s">"make dev"</span>')
  + "<h3>Ce qui est installé</h3>"
  + table(("catégorie", "combien", "détail"), [
      ("<b>Projets</b>", "30", "un par dépôt de <code>~/Desktop/projects</code>, plus <code>00-config</code> et <code>00-quotidien</code>"),
      ("<b>Quick actions</b>", "12", "sync des projets, GitHub, PR, tests, rechargement des configs, review, recherche, wifi, bluetooth, état réseau, kill, docs"),
    ], widths=(16, 12, 72), classes=["", "num", "d"])
  + note("tip", "synchroniser après un nouveau dépôt",
    "<code>herdr-sync-projects</code> (aussi disponible en quick action <code>00-sync-projects</code>) régénère un template pour chaque dépôt trouvé. Les fichiers <code>00-*</code> sont préfixés pour rester en tête du sélecteur.")
))

# ══════════════════════════════════════════════════════════════════ 07
S.append(section("07", "Agents — états & quotas",
  lede="C'est la raison d'être de herdr : savoir, sans rien taper, quel agent te bloque.",
  body=""
  + "<h3>Les états</h3>"
  + table(("état", "signification", "ce qu'on en fait"), [
      ("<b>working</b>", "l'agent calcule", "on le laisse ; on va sur un autre space"),
      ("<b>blocked</b>", "il attend une réponse de ta part", "<b>c'est celui-là qu'il faut traiter.</b> Une notification macOS native arrive après 2 s"),
      ("<b>idle</b>", "session ouverte, rien en cours", "on peut la reprendre ou la fermer"),
      ("<b>done</b>", "le tour est terminé", "à relire — <kbd>prefix+d</kbd> ouvre reviewr sur le diff"),
    ], widths=(14, 30, 56), classes=["", "d", "d"])
  + "<h3>Le panneau agents</h3>"
  + "<p>Règle appliquée : <b>une idée par ligne</b>, la plus importante en premier. La disposition précédente mettait <code>state_icon</code>, <code>workspace</code> et <code>tab</code> sur une seule ligne de 24 colonnes — tout était tronqué, et le projet, l'information la plus utile, était le premier sacrifié.</p>"
  + pre('<span class="s">◐</span> <span class="a">refactor du panier</span>      <span class="c">état + tâche en cours</span>\n'
        '  <span class="a">un-projet-long  </span>         <span class="c">DANS QUEL PROJET</span>\n'
        '  <span class="c">run                     où exactement dedans</span>\n'
        '  <span class="c">⛁ 13%   5h 72%          ce qu\'il consomme</span>')
  + table(("famille", "jetons"), [
      ("<b>Built-ins herdr</b>", "<code>state_icon</code> · <code>state_text</code> · <code>workspace</code> · <code>tab</code> · <code>pane</code> · <code>agent</code> · <code>terminal_title</code> · <code>terminal_title_stripped</code>"),
      ("<b>Métadonnées de pane</b>", "les jetons en <code>$</code>, publiés par un plugin via <code>herdr pane report-metadata</code>"),
      ("<b>Style en ligne</b>", "<code>{ token = \"workspace\", fg = \"#ffc799\", bold = true, dim = true }</code> — seuls <code>#RGB</code> et <code>#RRGGBB</code> sont acceptés, pas de noms de couleur"),
    ], widths=(24, 76), classes=["", "d"])
  + note("note", "il n'existe pas de jeton « dossier »",
    "La liste ci-dessus est complète. Pour savoir d'où vient un agent, c'est <code>workspace</code> qui répond — le libellé du space. C'est pourquoi <code>prefix_workspace_labels</code> est passé à <code>false</code> : le préfixe <code>project:</code> mangeait 9 colonnes sur chaque libellé. Le changement ne vaut que pour les <b>nouveaux</b> spaces ; renommer un space existant se fait avec <b>"+kbd("prefix+/")+"</b>.")
  + "<h3>Les jetons d'usage</h3>"
  + "<p>Fournis par le plugin <code>usagebar</code>. Ils ne s'affichaient nulle part : le plugin tournait, calculait tout, et n'avait aucun emplacement dans les lignes de la sidebar.</p>"
  + table(("jeton", "affiche", "exemple"), [
      (mono("$context"), "contexte consommé par le pane", "<code>⛁ 13% (130k)</code>"),
      (mono("$limit"), "la fenêtre de quota la plus courte, ou la dépense sur une clé API", "<code>5h 72%</code> · <code>Σ 425k $0.04</code>"),
      (mono("$provider"), "le backend qui répond", "<code>anthropic</code>, <code>ollama</code>"),
      (mono("$title"), "le titre du terminal, déjà nettoyé", "—"),
    ], widths=(16, 50, 34), classes=["c", "d", "d"])
  + note("warn", "quand ces chiffres sont mis à jour",
    "Quand l'agent s'est <b>posé</b>, pas pendant qu'il travaille. Les valeurs correspondent donc toujours au <em>dernier tour terminé</em>. Un agent en <code>working</code> affiche l'état d'avant son tour en cours — c'est voulu, mais il faut le savoir avant d'en tirer une conclusion.")
  + "<h3>Les touches</h3>"
  + keys_pair("Parcourir", [
      ("prefix+alt+, · alt+.", "agent précédent · suivant"),
      ("prefix+alt+1..9", "sauter à l'agent n°N"),
      ("prefix+w Ctrl+a", "filtrer les agents dans le navigator"),
      ("prefix+d", "reviewr — relire le travail de l'agent"),
      ("prefix+m", "memex — chercher dans les transcripts"),
    ], "Usage & quotas", [
      ("prefix+alt+u", "jauges de contexte et limites"),
      ("prefix+alt+t", "dashboard de dépense en direct"),
      ("prefix+alt+l", "llmtrim — économie de contexte"),
      ("prefix+alt+r", "statut de claude-auto-retry"),
    ])
))

# ══════════════════════════════════════════════════════════════════ 08
S.append(section("08", "Les plugins installés",
  lede="Vingt-et-un plugins actifs. Regroupés ici par ce à quoi ils servent, avec la touche qui les déclenche.",
  body=""
  + table(("plugin", "touche", "rôle"), [
      ("<b>herdr-navigator</b>", kbd("prefix+w"), "le picker universel — spaces, agents, sessions, serveurs, plugins, projets"),
      ("<b>cloudmanic.herdr-plus</b>", kbd("prefix+p"), "projets déclaratifs et quick actions"),
      ("<b>jt.command-palette</b>", kbd("prefix+a"), "palette fzf de tout ce qui est bindable"),
      ("<b>herdr-file-viewer</b>", kbd("prefix+f"), "explorateur git-aware, en split"),
      ("<b>ray.file-explorer</b>", kbd("prefix+y"), "yazi — explorateur de fichiers complet"),
      ("<b>termscope</b>", kbd("prefix+t")+" "+kbd("prefix+u"), "ouvrir un fichier / un lien <em>visible à l'écran</em>"),
      ("<b>persiyanov.reviewr</b>", kbd("prefix+d"), "relecture du diff produit par l'agent"),
      ("<b>nicosuave.memex</b>", kbd("prefix+m"), "recherche dans les transcripts d'agents"),
      ("<b>usagebar</b>", kbd("prefix+alt+u"), "contexte et quotas — fournit <code>$context</code> et <code>$limit</code>"),
      ("<b>dave.token-dashboard</b>", kbd("prefix+alt+t"), "dépense en jetons, en direct"),
      ("<b>llmtrim.proxy</b>", kbd("prefix+alt+l"), "réduit le contexte envoyé aux modèles"),
      ("<b>claude-auto-retry</b>", kbd("prefix+alt+r"), "relance automatique après une coupure"),
      ("<b>herdr-lazygit</b>", kbd("prefix+alt+v"), "lazygit dans un pane latéral"),
      ("<b>herdr-floax</b>", kbd("prefix+alt+f"), "shell flottant, un par space, qui garde son état"),
      ("<b>herdr-zoxide</b>", kbd("prefix+alt+z"), "sauter dans un dossier fréquenté → space"),
      ("<b>ray.plugin-manager</b>", kbd("prefix+alt+p"), "installer / retirer des plugins"),
      ("<b>vim-herdr-navigation</b>", kbd("alt+h j k l"), "navigation continue panes herdr ↔ splits nvim"),
      ("<b>herdr-automatic-rename</b>", "—", "nomme les tabs d'après ce qui y tourne"),
      ("<b>herdr-focus-notify</b>", "—", "notification quand un agent passe en <em>blocked</em>"),
      ("<b>herdr-lazy</b>", "—", "chargement différé des plugins lourds"),
      ("<b>herdr-remote.relay</b>", "—", "relais vers un serveur herdr distant"),
    ], widths=(24, 22, 54), classes=["", "k", "d"])
))

# ══════════════════════════════════════════════════════════════════ 09
S.append(section("09", "Popups & outils",
  lede="Un popup s'ouvre au-dessus du travail et se ferme à la sortie du processus. La disposition en cours n'est jamais touchée.",
  body=""
  + table(("touche", "outil", "ce qu'il apporte"), [
      (kbd("prefix+alt+g"), "<b>lazygit</b>", "revue complète, mise en scène par blocs, rebase interactif — 90 % × 90 %"),
      (kbd("prefix+alt+v"), "<b>lazygit (pane)</b>", "le même, en latéral, quand on veut garder le code visible"),
      (kbd("prefix+alt+d"), "<b>lazydocker</b>", "conteneurs, images, logs, volumes"),
      (kbd("prefix+alt+b"), "<b>btop</b>", "processus, CPU, mémoire, réseau — on y va pour <em>regarder</em>"),
      (kbd("prefix+alt+k"), "<b>pk</b>", "on y va pour <em>agir</em> : recherche floue sur le nom <b>et</b> la ligne de commande complète, puis kill"),
      (kbd("prefix+alt+e"), "<b>herdr-manage</b>", "renommer ou supprimer un space / une session, avec fzf et gum"),
      (kbd("prefix+alt+f"), "<b>floax</b>", "shell flottant : « j'ai une commande à taper » sans casser la disposition"),
      (kbd("prefix+alt+m"), "<b>mdv</b>", "parcourir <code>~/.config/docs</code> en Markdown rendu"),
    ], widths=(17, 17, 66), classes=["k", "", "d"])
  + note("note", "pourquoi pk en plus de btop",
    "btop sait tuer un processus, mais c'est un moniteur : on y arrive pour observer. <code>pk</code> fait l'inverse — on y arrive pour agir, et la recherche floue porte sur la ligne de commande entière, ce qui permet de retrouver un processus par son argument (un port, un chemin, un nom de script) et pas seulement par le nom du binaire.")
  + note("warn", "le protocole graphique kitty",
    "<code>kitty_graphics = false</code> : Alacritty n'implémente <b>pas</b> ce protocole (Ghostty et kitty, oui). Les aperçus d'images de yazi restent donc en mode texte. À repasser à <code>true</code> en cas de retour sur Ghostty.")
))

# ══════════════════════════════════════════════════════════════════ 10
S.append(section("10", "Sessions, worktrees, jj",
  "<h3>Sessions</h3>"
  + "<p>Une session est un répertoire : elle survit au redémarrage du serveur. <code>resume_agents_on_restore = true</code> reprend les conversations d'agents avec elle.</p>"
  + mixed_pair("Dans herdr", [
      ("prefix+w Ctrl+l", "lister et rejoindre une session"),
      ("prefix+alt+e", "renommer ou supprimer"),
      ("prefix+w", "créer depuis le navigator"),
    ], "En ligne de commande", [
      ("herdr-session-presets", "les gabarits de session prêts à l'emploi"),
      ("herdr-rename-session", "renommer proprement (répertoire compris)"),
      ("herdr-new <type> <nom>", "créer un space / une session / une quick"),
    ])
  + "<h3>Worktrees git</h3>"
  + "<p>Répertoire : <code>~/.herdr/worktrees</code>. Un worktree donne à un agent une copie de travail à lui, sur sa propre branche — il peut donc modifier des fichiers pendant que tu travailles sur les mêmes, sans conflit et sans <code>stash</code>.</p>"
  + keys([
      ("prefix+shift+g", "créer un worktree et le space qui va avec"),
      ("prefix+d", "reviewr — relire ce que l'agent y a fait avant de fusionner"),
    ], widths=(24, 76))
  + "<h3>jj (Jujutsu) — colocalisé avec git</h3>"
  + "<p>jj vit dans le même dépôt que git : les deux outils voient les mêmes commits, on peut alterner sans rien migrer. Utile ici parce que jj n'a pas de zone de staging — chaque modification est déjà « dans » le changement courant, ce qui colle au rythme d'un agent qui écrit en continu.</p>"
  + cmds([
      ("jj l", "log — l'arbre des changements"),
      ("jj d", "diff du changement courant"),
      ("jj s", "statut"),
      ("jj ci -m \"…\"", "décrire le changement et en commencer un nouveau"),
      ("jj into <rev>", "déplacer le travail courant dans une révision existante"),
      ("jj out", "extraire le travail courant vers un nouveau changement"),
      ("gdt", "<code>git dft</code> — diff syntaxique difftastic, le même moteur que jj"),
    ], widths=W_CMD)
  + note("note", "les alias jj",
    "<code>fix</code> entrait en conflit avec la commande intégrée du même nom : l'alias a été renommé <code>into</code>. Le formateur de diff est difftastic, qui compare les arbres syntaxiques au lieu des lignes — sur un reformatage, il montre ce qui a vraiment changé.")
))

# ══════════════════════════════════════════════════════════════════ 11
S.append(section("11", "La ligne de commande",
  cmds([
      ("herdr", "démarrer ou rejoindre le serveur"),
      ("herdr --version", "version installée"),
      ("herdr server reload-config", "recharger la config sans redémarrer — alias <code>hr</code>"),
      ("herdr server stop", "arrêter le serveur (les sessions sont conservées)"),
      ("herdr list", "spaces, tabs et panes du serveur courant"),
      ("herdr attach <space>", "rejoindre un space depuis l'extérieur"),
      ("herdr plugin list", "les plugins installés et leur état"),
      ("herdr plugin install <nom>", "installer un plugin"),
    ], widths=W_CMD_L)
  + "<h3>Les scripts maison</h3>"
  + cmds([
      ("herdr-open-project <nom>", "focaliser le space s'il existe, le monter sinon — <b>c'est ce qui évite les doublons</b>"),
      ("herdr-new <type> <nom>", "créer un space, une session ou une quick session"),
      ("herdr-nav-create", "le pont appelé par le navigator quand on tape un nom inexistant"),
      ("herdr-nav-tabs", "repli si le correctif du navigator ne peut plus être réappliqué"),
      ("herdr-manage", "renommer / supprimer, en fzf + gum"),
      ("herdr-rename-session", "renommer une session, répertoire compris"),
      ("herdr-session-presets", "gabarits de session"),
      ("herdr-sync-projects", "régénérer un template herdr-plus par dépôt trouvé"),
    ], widths=W_CMD_L)
  + note("tip", "recharger après édition",
    "Toute modification de <code>~/.config/herdr/config.toml</code> se recharge à chaud avec <b>"+kbd("prefix+shift+r")+"</b> ou <code>hr</code>. Inutile de fermer les panes : seules les liaisons de touches et l'UI sont reconstruites.")
))

# ══════════════════════════════════════════════════════════════════ 12
S.append(section("12", "Dépannage",
  table(("symptôme", "cause", "geste"), [
      ("<b>Le texte disparaît pendant la frappe dans un picker</b>",
       "<kbd>Ctrl+s</kbd> a été envoyé — c'est XOFF, le terminal a gelé son affichage",
       "<kbd>Ctrl+q</kbd> pour reprendre. Ne jamais lier <kbd>Ctrl+s</kbd> dans un picker"),
      ("<b>Deux spaces « quotidien »</b>",
       "<code>herdr-plus open</code> crée toujours un nouveau space",
       "passer par <code>herdr-open-project</code>, qui focalise l'existant"),
      ("<b>Ctrl+l n'efface plus l'écran</b>",
       "herdr captait <kbd>Ctrl+hjkl</kbd> sans préfixe",
       "la navigation est sur <kbd>Alt</kbd> — vérifier qu'aucun binding <kbd>Ctrl</kbd> n'a été réintroduit"),
      ("<b>Un binding shell ne trouve pas la commande</b>",
       "un binding <code>type = \"shell\"</code> tourne détaché, sans le PATH du shell de connexion",
       "mettre le <b>chemin absolu</b> dans la config"),
      ("<b>Les jetons $context / $limit restent vides</b>",
       "l'agent n'a pas encore terminé un tour",
       "les valeurs n'apparaissent qu'après le premier tour <em>terminé</em>"),
      ("<b>Aperçus d'images absents dans yazi</b>",
       "Alacritty n'implémente pas le protocole graphique kitty",
       "attendu ; <code>kitty_graphics = false</code> est le bon réglage ici"),
      ("<b>Le navigator est noyé de chemins</b>",
       "les sources <code>zoxide</code> / <code>roots</code> / <code>quick</code> sont réactivées",
       "les remettre à <code>false</code> dans <code>herdr-navigator/config.toml</code>"),
      ("<b>prefix+w ne fait plus rien</b>",
       "un pane « Herdr Navigator » périmé : le processus est mort, herdr a laissé un shell, mais le pane garde son libellé — le picker le focalisait indéfiniment",
       "corrigé dans le correctif du plugin (auto-réparation). Manuellement : <code>herdr pane list</code> puis <code>herdr pane close &lt;id&gt;</code>"),
      ("<b>Deux cadres « Herdr Navigator » imbriqués</b>",
       "herdr encadre déjà le pane du plugin et y écrit son libellé ; le plugin redessinait le sien par-dessus",
       "<code>own_frame = false</code> (défaut du correctif). À remettre à <code>true</code> si <code>pane_borders = false</code>"),
      ("<b>Un plugin ne répond plus</b>",
       "action renommée en amont, ou plugin non chargé",
       kbd("prefix+alt+p")+" pour vérifier son état, puis <code>hr</code>"),
    ], widths=(24, 32, 44), classes=["", "d", "d"])
))

# ══════════════════════════════════════════════════════════════════ 13
S.append(section("13", "Index des touches",
  "<p class=small>Préfixe = <kbd>Ctrl+b</kbd>, omis dans cet index.</p>"
  + idx([
    ("Spaces", [
        ("n", "nouveau"), ("g", "sélecteur"), ("Shift+w", "goto"),
        ("[  ]", "précédent / suivant"), ("Shift+1..9", "aller au n°N"),
        ("/", "renommer"), ("'", "fermer"), ("h", "space quotidien"),
        ("p", "projets"), ("Shift+r", "recharger la config"),
    ]),
    ("Tabs & panes", [
        ("c", "nouveau tab"), ("1..9", "tab n°N"),
        (",  .", "tab préc. / suiv."), (";", "renommer le tab"),
        ("s", "split horizontal"), ("v", "split vertical"),
        ("x", "fermer le pane"), ("z", "zoom"),
        ("`", "aller-retour"), ("Shift+p", "renommer le pane"),
    ]),
    ("Navigation", [
        ("i j k l", "focus (façon AeroSpace)"),
        ("Alt+h j k l", "focus (nvim, sans préfixe)"),
        ("w", "navigator"), ("Alt+w", "navigator latéral"),
        ("Alt+i", "revenir en arrière"), ("Alt+z", "zoxide"),
        ("y", "yazi"), ("f", "file viewer"),
        ("t", "termscope fichiers"), ("u", "termscope liens"),
    ]),
    ("Agents", [
        ("Alt+,  Alt+.", "agent préc. / suiv."),
        ("Alt+1..9", "agent n°N"), ("d", "reviewr"),
        ("m", "memex"), ("Alt+u", "usage & limites"),
        ("Alt+t", "dashboard tokens"), ("Alt+l", "llmtrim"),
        ("Alt+r", "auto-retry"), ("w Ctrl+a", "filtrer les agents"),
        ("Shift+g", "nouveau worktree"),
    ]),
    ("Outils", [
        ("Alt+g", "lazygit (popup)"), ("Alt+v", "lazygit (pane)"),
        ("Alt+d", "lazydocker"), ("Alt+b", "btop"),
        ("Alt+k", "pk — tuer un process"), ("Alt+e", "herdr-manage"),
        ("Alt+f", "shell flottant"), ("Alt+m", "docs en markdown"),
        ("a", "palette de commandes"), ("Alt+a", "quick actions"),
    ]),
    ("Filtres du navigator", [
        ("Ctrl+w", "spaces"), ("Ctrl+a", "agents"),
        ("Ctrl+l", "sessions"), ("Ctrl+g", "serveurs"),
        ("Ctrl+n", "plugins"), ("Ctrl+p", "projets"),
        ("—", "—"), ("Ctrl+s", "INTERDIT — XOFF"),
        ("Ctrl+q", "INTERDIT — XON"), ("Ctrl+c/z/d", "INTERDITS — signaux"),
    ]),
  ])
))

HERE = os.path.dirname(os.path.abspath(__file__))
cv = cover(
    "référence · espace de travail",
    "herdr",
    subtitle="usage approfondi",
    sub="Le multiplexeur pensé pour les agents : spaces, panes, états, quotas. "
        "Cette configuration s'écarte des défauts sur un point de principe — aucune "
        "majuscule dans les gestes quotidiens.",
    toc=TOC,
    stats=[("21", "plugins"), ("30", "projets"), ("12", "quick actions"), ("Ctrl+b", "préfixe")],
    meta_left=VER, meta_right=DATE,
)
open(os.path.join(HERE, "herdr-cover.html"), "w").write(page("herdr — couverture", cv, full_bleed=True))
open(os.path.join(HERE, "herdr-body.html"), "w").write(page("herdr — référence", "".join(S)))
print("herdr-cover.html + herdr-body.html")
