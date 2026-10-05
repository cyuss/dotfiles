# -*- coding: utf-8 -*-
import sys, os
sys.path.insert(0, os.path.dirname(__file__))
from lib import *
K = kbd
def L(r): return K("SPC") + " " + K(r)
def H(r): return K("Ctrl+B") + " " + K(r)
S = []

# ══ 01 ════════════════════════════════════════════════════════════════
s1 = """
<p class=lede>Ce qui suit n'est pas une impression : ce sont tes 28 dépôts,
tes 328 commits signés de ta main, et l'état de ton disque au moment
d'écrire ces lignes.</p>

<h3>Le rythme</h3>
""" + table(("Mesure","Valeur","Lecture"),[
 ("Commits à toi","<b>328</b>, sur 48 jours distincts","Tu travailles par sessions denses, pas en filet continu"),
 ("Répartition","270 en 2026, 28 en 2023, le reste avant","<b>2026 est ton année</b> — 82 % de ton historique"),
 ("Conventional commits","<b>93 %</b>","Discipline réelle et rare. Rien à corriger ici"),
 ("Longueur médiane du sujet","59 caractères","Sous la limite de 72. Rien à corriger non plus"),
 ("Préfixes dominants","<code>config</code> 31 · <code>fix</code> 24 · <code>feat</code> 23 · <code>docs</code> 20","Un tiers de ton activité est de l'outillage"),
], classes=["d","c","d"]) + """
<h3>La taille des changements</h3>
""" + table(("Mesure","Valeur","Lecture"),[
 ("Lignes ajoutées, médiane","<b>72</b>","La moitié de tes commits sont petits et lisibles"),
 ("Lignes ajoutées, p90","<b>1 613</b>","Mais la queue est longue"),
 ("Commits de plus de 300 lignes","<b>55 sur 196</b> (28 %)","Un commit sur quatre est irrelisible en relecture"),
 ("Dépôts non commités <i>maintenant</i>","<b>15 sur 28</b>","Le travail stationne avant d'atterrir en bloc"),
], classes=["d","c","d"]) + note('note','Ces deux chiffres racontent la même chose',
 "Un p90 à 1 613 lignes et 15 dépôts sales au même instant ne sont pas deux "
 "problèmes : c'est le même. Le travail reste dans l'arbre de travail, "
 "s'accumule, puis part en un seul commit. La section 06 propose le seul "
 "outil qui attaque ça sans rien changer à tes habitudes.") + """

<h3>L'équipement</h3>
""" + table(("Pratique","Couverture","Lecture"),[
 ("Makefile ou justfile","<b>15 / 28</b> (53 %)","Bonne habitude — tu normalises tes commandes"),
 ("README","21 / 28 (75 %)",""),
 ("Tests présents","<b>10 / 28</b> (35 %)","Et le détail ci-dessous est plus dur"),
 ("Intégration continue","8 / 28 (28 %)",""),
 ("<code>ruff</code> configuré","9 / 28 (32 %)","Alors que ruff est installé sur la machine"),
 ("<code>mypy</code> configuré","9 / 28 (32 %)",""),
 ("<code>pre-commit</code>","<b>1 / 28</b> (3 %)","Le trou le plus large, et le moins cher à combler"),
], classes=["d","c","d"]) + """
<h3>Les tests, dépôt par dépôt</h3>
""" + table(("Dépôt","Fichiers source","Fichiers de test"),[
 ("<code>stats_hdj</code>","26","<b>0</b>"),
 ("<code>target-cx</code>","23","5"),
 ("<code>fastapi-template</code>","13","1"),
 ("<code>planning_hdj</code>","12","<b>0</b>"),
 ("<code>ml-utils</code>","10","<b>0</b>"),
 ("<code>footy-hub</code>","5","<b>0</b>"),
], classes=["c","c","c"]) + note('warn','Le constat qui compte',
 "Ton <code>AGENTS.md</code> dit, mot pour mot : « Bug corrigé = test qui "
 "échouait avant et passe après. » Tu as écrit la règle, tu as un subagent "
 "<code>tester</code> dans opencode, des snippets pytest dans Doom, et "
 "hypothesis dans tes dépendances. <b>Quatre projets Python sur six n'ont "
 "aucun test.</b> Ce n'est pas un manque d'outils — c'est le seul endroit où "
 "l'outillage ne peut rien pour toi.")
S.append(section("01","Ce que disent tes dépôts", s1))

# ══ 02 ════════════════════════════════════════════════════════════════
s2 = """
<p class=lede>La bonne façon de choisir un geste de navigation, c'est la
<b>distance</b>. Chaque échelle a son outil ; utiliser celui de l'échelle
au-dessus coûte du temps, celui de l'échelle en dessous coûte des
répétitions.</p>
""" + table(("Distance","L'outil","Le geste"),[
 ("<b>Dans la ligne</b>","Vim natif", K("f")+K("x")+" jusqu'au caractère · "+K(";")+" répéter · "+K("t")+" juste avant"),
 ("<b>Dans l'écran</b>","<b>avy</b>", K("g")+" "+K("a")+" puis tape les lettres de la cible, puis l'étiquette"),
 ("<b>Dans le fichier</b>","imenu · combobulate", L("s i")+" les symboles · "+K("C-c o")+" par nœud syntaxique"),
 ("<b>Dans le fichier, par sens</b>","LSP", K("g")+" "+K("d")+" définition · "+K("g")+" "+K("D")+" références · "+K("C-o")+" revenir"),
 ("<b>Dans le projet</b>","projectile · ripgrep", L("SPC")+" un fichier · "+L("/")+" une chaîne · "+L("c j")+" un symbole"),
 ("<b>Entre projets</b>","herdr", H("p")+" monter un projet · "+H("w")+" sauter n'importe où"),
 ("<b>Entre univers</b>","sessions herdr", H("w")+" puis "+K("Ctrl+S")),
], classes=["d","d","k"]) + """

<h3>avy en détail — l'échelle la plus rentable</h3>
<p>Deux réglages viennent d'être changés et ils comptent : avy étiquette
maintenant <b>toutes les fenêtres</b> (tu peux sauter dans l'autre split, et
le focus suit), et il saute directement quand il n'y a qu'un candidat.</p>
""" + table(("Geste","Effet"),[
 (K("g")+" "+K("a"),"<b>Le geste principal.</b> Tape ce que tu vois, autant de lettres qu'il faut, puis choisis l'étiquette"),
 (L("j c"),"Deux caractères exactement, plus rapide quand la cible est évidente"),
 (L("j l")+" · "+L("j w"),"Une ligne · un début de mot"),
 (L("j r"),"Reprendre le dernier saut · "+L("j b")+" revenir d'où tu venais"),
], classes=["k","d"]) + note('tip','Ce que presque personne ne découvre',
 "Pendant que les étiquettes sont affichées, tu peux taper une <b>action</b> "
 "au lieu d'une étiquette, puis désigner la cible :<br>"
 + K("x") + " tuer la ligne visée · " + K("X") + " tuer la région · "
 + K("t") + " la téléporter ici · " + K("m") + " la déplacer ici · "
 + K("y") + " la copier · " + K("z") + " zapper jusque-là.<br>"
 "C'est ce qui fait passer avy de « un saut » à « un opérateur qui agit à "
 "distance ». Ce sont ses réglages d'origine : rien à configurer, seulement "
 "à se rappeler une fois.") + """

<h3>combobulate — naviguer par la syntaxe</h3>
<p class=lede>Nouveau. Il lit l'arbre tree-sitter, donc il comprend qu'un
bloc Python est un bloc — ce que <code>puni</code>, qui raisonne sur les
délimiteurs, ne peut pas faire dans un langage sans accolades.</p>
""" + table(("Geste","Effet"),[
 (K("C-c o")+" "+K("n")+" / "+K("p"),"Nœud frère suivant / précédent — la fonction d'à côté, la clé d'à côté"),
 (K("C-c o")+" "+K("u")+" / "+K("d"),"Remonter au parent · descendre dans l'enfant"),
 (K("C-c o")+" "+K("t"),"Afficher l'arbre syntaxique du buffer — pour comprendre ce qu'on manipule"),
 (K("C-c o")+" "+K("m"),"Marquer le nœud courant, puis étendre à chaque appui"),
 ("Actif sur","python · js · typescript · tsx · json · yaml · toml · css · html"),
], classes=["k","d"])
S.append(section("02","Naviguer, échelle par échelle", s2))

# ══ 03 ════════════════════════════════════════════════════════════════
def scene(title, ctx, steps_list):
    return ('<div class="card"><h5><span>' + title + '</span></h5>'
            '<div class="role">' + ctx + '</div>' + steps(steps_list) + '</div>')

s3 = "<p class=lede>Six situations réelles, du premier geste au dernier.</p>"

s3 += scene("Un bug remonte, tu n'as que le message d'erreur",
  "Le réflexe coûteux est d'ouvrir le projet et de lire. Le bon réflexe est de "
  "laisser la machine localiser.",
  [H("p") + " → le projet se monte : agent, code, git, run.",
   L("/") + " la chaîne exacte du message d'erreur. ripgrep sur tout le dépôt.",
   K("Entrée") + " sur le résultat, puis " + K("g") + " " + K("d") + " pour remonter aux définitions "
   "et " + K("C-o") + " pour revenir. L'historique de sauts est ton fil d'Ariane.",
   "<code>/fix</code> dans opencode — le subagent <code>debugger</code> est en "
   "lecture seule et sa consigne est de <b>reproduire avant de corriger</b>.",
   "Le test qui échoue d'abord : <code>/test</code>. C'est ta propre règle, "
   "écrite dans ton <code>AGENTS.md</code>.",
   L("d b") + " sur la ligne suspecte puis " + L("d d") + " si la lecture ne suffit pas. "
   "<code>uv add --dev debugpy</code> une fois par projet."])

s3 += scene("Explorer un dépôt que tu ne connais pas",
  "Le piège est de lire des fichiers au hasard. Il faut une carte avant un texte.",
  ["<code>/scout</code> dans opencode. C'est la seule commande marquée "
   "<code>subtask: true</code> : elle explore dans un contexte <b>isolé</b> et "
   "ne te rend que la carte. Trente fichiers lus, dix lignes rendues.",
   L("s i") + " sur les fichiers qu'elle nomme — les symboles avant le code.",
   K("C-c o") + " " + K("n") + " pour parcourir les définitions de haut niveau sans lire les corps.",
   H("f") + " (file viewer) avec " + K("]") + " " + K("[") + " pour voir <b>ce qui a bougé récemment</b> : "
   "c'est là qu'est le code vivant.",
   "<code>tokei</code> pour le volume, <code>serie</code> pour la forme de l'historique."])

s3 += scene("Une idée à tester en trois minutes",
  "Le coût réel n'est pas le code : c'est de décider où le mettre.",
  [H("w") + " puis " + K("Ctrl+N") + " → <b>⚡ scratch</b>. Un space jetable, un shell, "
   "un opencode, dans <code>~/scratch/&lt;horodatage&gt;</code>. <b>Aucun nom à choisir</b> — "
   "c'est le point : une décision de nommage en moins, c'est un essai de plus.",
   "<code>uv init &amp;&amp; uv add pandas</code>. Trois secondes.",
   "Si ça vaut quelque chose : <code>git init</code>, et le dossier devient un projet. "
   "Sinon tu le laisses mourir là."])

s3 += scene("Trois agents en parallèle",
  "Le régime pour lequel herdr existe. Toute la difficulté est de ne pas regarder.",
  [H("Shift+G") + " trois fois — <b>un worktree par tâche</b>. Trois branches, trois "
   "répertoires, aucun conflit d'index possible.",
   "Brief les trois, puis <b>pars</b>. Un agent qui travaille est un agent qu'on ne regarde pas.",
   "Notification macOS → " + H("o") + " saute sur le pane qui t'attend.",
   H("d") + " (reviewr) : lis le diff, " + K("v") + " sélectionne, " + K("c") + " commente, "
   "<b>" + K("s") + " renvoie tout à l'agent</b>. Tu ne lis jamais le code dans le chat.",
   H("Alt+G") + " lazygit, commit, retour. Le suivant t'attend déjà.",
   "Surveille " + H("Alt+U") + " : le contexte consommé par pane. Un agent à 90 % "
   "raconte n'importe quoi bien avant de le dire."])

s3 += scene("Ajouter une fonctionnalité à un projet existant",
  "Là où la taille de tes commits se joue.",
  ["Cadrer d'abord : " + K("Tab") + " vers l'agent <code>plan</code> d'opencode, ou "
   "<code>/explain</code>. Un modèle de raisonnement sur la conception, un modèle "
   "de code sur l'exécution.",
   "Écrire. " + L("w V") + " pour ouvrir le test à côté du code — <b>la majuscule</b>, "
   "qui splitte <i>et</i> y va.",
   "<b>Committer en cours de route</b>, pas à la fin. C'est le point qui change tes "
   "chiffres : médiane 72 lignes, p90 1 613.",
   "Un oubli sur un commit déjà fait ? <code>git absorb</code> — il est installé "
   "chez toi et range la correction dans le bon commit tout seul.",
   "<code>/review</code> avant de pousser. Le subagent <code>reviewer</code> est en "
   "lecture seule et ne remonte que les vrais défauts."])

s3 += scene("Relire et livrer",
  "",
  [H("d") + " reviewr, portée " + K("b") + " (toute la branche) pour voir ce que tu "
   "livres vraiment, pas seulement le dernier tour.",
   "<code>make test</code> — ou <code>Ctrl+B Alt+A</code> → « Tests », qui détecte "
   "le runner tout seul.",
   "<code>/commit</code> puis <code>/pr</code>. Le <code>git push</code> demandera "
   "confirmation : c'est voulu, c'est dans tes règles de permission.",
   "<code>repos-status</code> de temps en temps : ce qui traîne, en une seconde, sur "
   "les 28 dépôts."])
S.append(section("03","Mises en situation", s3))

# ══ 04 ════════════════════════════════════════════════════════════════
s4 = """
<p class=lede>Ton outillage est déjà organisé en couches. Les nommer aide à
savoir où ajouter quelque chose — et où ne surtout rien ajouter.</p>
""" + table(("Couche","Ce qu'elle porte","Chez toi"),[
 ("<b>Fenêtres</b>","Où vivent les applications","AeroSpace, 9 spaces nommés, assignation par app"),
 ("<b>Terminal</b>","Où vivent les processus","Alacritty + herdr : spaces, tabs, panes, agents"),
 ("<b>Agents</b>","Qui écrit le code avec toi","opencode (local, Ollama) · Claude Code · Copilot inline"),
 ("<b>Éditeur</b>","Où tu lis et corriges","Doom Emacs en daemon, basedpyright, dape"),
 ("<b>Projet</b>","Ce qui rend une commande reproductible","<code>Makefile</code>, <code>pyproject.toml</code>, <code>uv</code>"),
 ("<b>Historique</b>","Ce qui reste quand tu fermes tout","git, lazygit, <code>gh</code>"),
], classes=["d","d","d"]) + """
<h3>La règle qui traverse les couches</h3>
<p>Chaque couche a un <b>point d'entrée unique</b>, et c'est ce qui rend
l'ensemble tenable :</p>
""" + table(("Question","Le geste"),[
 ("Où est-ce ?", H("w") + " (navigator) — spaces, agents, sessions, projets"),
 ("Quelle est cette touche ?", L("h k") + " dans Doom · " + H("?") + " dans herdr"),
 ("Que puis-je faire ici ?", H("a") + " (palette) · " + K("C-c o") + " (combobulate) · " + L("c a") + " (LSP)"),
 ("Qu'est-ce qui a changé ?", H("d") + " (reviewr) · " + H("f") + " " + K("]") + " · <code>repos-status</code>"),
 ("Combien ça coûte ?", H("Alt+U") + " contexte · " + H("Alt+T") + " dépense"),
], classes=["d","k"]) + note('tip','Ce que tu as déjà et qui vaut d\'être conscient',
 "170 formules Homebrew, 21 plugins herdr, 51 modules Doom, un daemon Emacs "
 "à 1,4 s, des agents locaux qui ne sortent rien de la machine, et une "
 "convention de commit tenue à 93 %. L'outillage n'est pas ton facteur "
 "limitant — c'est pour ça que les propositions qui suivent portent toutes "
 "sur la <b>méthode</b>, et une seule sur le confort.")
S.append(section("04","La boucle de dev, en couches", s4))

# ══ 05 ════════════════════════════════════════════════════════════════
s5 = """
<p class=lede>Quatre propositions, classées par rapport entre l'effort et ce
que les chiffres de la section 01 disent. Aucune n'est un gadget.</p>
""" + card("1 · pre-commit — 3 % de tes dépôts", None,
  "<b>Le meilleur rapport de tout ce document.</b> Tu as <code>ruff</code> sur la "
  "machine et configuré dans 9 dépôts sur 28. Un hook le fait tourner <b>avant</b> "
  "que le code entre dans l'historique, au lieu d'espérer que tu y penses.",
  [("Installer","<code>brew install pre-commit</code> puis <code>pre-commit install</code> par dépôt"),
   ("Le fichier","Tu as déjà le snippet : <code>pc</code> dans Doom (ruff + ruff-format + mypy + hooks de base)"),
   ("Alternative","<code>lefthook</code> — même service, écrit en Go, pas de venv à gérer, démarre plus vite"),
   ("Ce que ça change","Le formatage cesse d'être un commit <code>style:</code> — tu en as 13")],
  headers=("","")) + card("2 · jj (Jujutsu) — pour ta queue de gros commits", None,
  "C'est le changement de fond de 2026 côté VCS, et il vise <b>exactement</b> ton "
  "problème : p90 à 1 613 lignes, 15 dépôts sales. jj s'installe <b>par-dessus</b> "
  "git — même dépôt, même remote, tes collègues ne voient rien.",
  [("Le modèle","Ton travail est <b>toujours</b> dans un commit ; il n'y a pas d'index ni de <i>stash</i>. On ne « pense pas à committer », c'est déjà fait"),
   ("<code>jj split</code>","Découper après coup un gros changement en commits lisibles — le geste qui manque à git"),
   ("<code>jj undo</code>","Annuler <b>n'importe quelle</b> opération, y compris un rebase raté"),
   ("Adoption","<code>jj git init --colocate</code> dans un dépôt existant. lazygit et <code>gh</code> continuent de fonctionner"),
   ("Bonus herdr","Le plugin <code>NathanFlurry/herdr-plugin-jj-workspace</code> crée des workspaces jj comme tes worktrees git")],
  headers=("","")) + card("3 · mise — remplacer quatre gestionnaires par un", None,
  "Tu as <b>pyenv, nvm, jenv et chruby</b> installés simultanément. Ce n'est pas "
  "théorique : les shims pyenv ont causé <b>deux pannes réelles</b> cette semaine — "
  "un <code>uv</code> 0.5.11 qui masquait le 0.12.5, et un "
  "<code>pyright-langserver</code> qui passait par un shim à chaque démarrage de "
  "serveur LSP.",
  [("Ce que ça fait","Un seul outil pour python, node, java, ruby, go — et les variables d'environnement par projet"),
   ("Pourquoi c'est plus sûr","Pas de shims : mise modifie le PATH à l'entrée du répertoire. Rien ne peut masquer un binaire global"),
   ("En prime","Il remplace aussi <code>direnv</code>, que tu as déjà"),
   ("Migration","<code>brew install mise</code>, un <code>mise.toml</code> par projet, puis retirer pyenv en dernier — pas avant")],
  headers=("","")) + card("4 · La boucle de test — le seul point sans outil", None,
  "Quatre projets Python sur six n'ont aucun test. Aucun outil ne corrige ça ; "
  "seule la <b>friction</b> peut être réduite.",
  [("<code>pytest-watcher</code>","<code>uv add --dev pytest-watcher</code> puis <code>ptw .</code> — les tests se relancent à chaque sauvegarde. La boucle rouge/vert sans quitter l'éditeur"),
   ("La cible <code>make test</code>","Tu as 15 Makefiles. Une cible <code>test</code> dans chacun, et <code>Ctrl+B Alt+A</code> la trouve toute seule"),
   ("Commencer par le bug","Pas de couverture rétroactive. La prochaine correction, un test qui échoue d'abord — c'est déjà ta règle écrite"),
   ("<code>ty</code>","Le vérificateur de types d'Astral, en Rust, bien plus rapide que mypy. <b>Encore en 0.0.x</b> : à essayer, pas à mettre en CI")],
  headers=("",""))
S.append(section("05","Ce que je propose, et pourquoi", s5))

# ══ 06 ════════════════════════════════════════════════════════════════
s6 = """
<p class=lede>Les outils qui n'ont pas besoin d'une justification longue :
ils font une chose, tu la fais déjà à la main.</p>
""" + table(("Outil","Ce qu'il remplace","Quand"),[
 ("<b><code>vd</code></b> (visidata)","Ouvrir un CSV dans pandas juste pour le regarder","CSV, Excel, Parquet, SQLite — tu manipules du xlsx dans <code>stats_hdj</code>"),
 ("<b><code>posting</code></b>","<code>curl</code> recopié depuis l'historique","Tester une route FastAPI, garder les requêtes dans le dépôt"),
 ("<b><code>lnav</code></b>","<code>tail -f | grep</code>","Il comprend les formats et sait faire du SQL sur un log"),
 ("<code>jnv</code>","Réécrire un filtre <code>jq</code> dix fois","Explorer un JSON inconnu, interactivement"),
 ("<code>dive</code>","Deviner pourquoi l'image pèse 2 Go","Couche par couche, ce qui pèse"),
 ("<code>act</code>","Pousser pour voir si la CI passe","Faire tourner le workflow GitHub en local"),
 ("<code>serie</code>","<code>git log --graph</code> illisible","Voir la forme réelle de l'historique"),
 ("<code>git absorb</code>","<code>rebase -i</code> pour un oubli","<b>Déjà installé chez toi</b>, et ton p90 dit que tu ne l'utilises pas"),
 ("<code>gping</code> · <code>trip</code>","<code>ping</code> et <code>traceroute</code>","Quand « le réseau est bizarre »"),
], classes=["c","d","d"]) + """
<h3>Ce que je ne recommande pas, et pourquoi</h3>
""" + table(("Outil","Verdict"),[
 ("<code>pulsar</code>","Doublon. <code>:ui nav-flash</code> est actif et fait la même chose, avec le même <code>pulse.el</code>"),
 ("<code>treemacs</code>","Tu l'as, mais tu as aussi yazi, dirvish et le file viewer de herdr. Trois de trop"),
 ("Un client HTTP en plus","<code>posting</code> suffit. <code>httpie</code> et <code>xh</code> sont déjà là pour le non interactif"),
 ("Un nouveau thème","Le sujet est réglé : Horizon, et <code>horizon-noir</code> en réserve"),
], classes=["c","d"])
S.append(section("06","Outils, sans discours", s6))

# ══ 07 ════════════════════════════════════════════════════════════════
s7 = """<p class=lede>Les gestes de navigation, du plus proche au plus lointain.</p>""" + idx([
 ("Dans la ligne", [
  ("f x  ;  ,","jusqu'au caractère"), ("t x","juste avant"),
  ("0  ^  $","début / premier mot / fin"), ("w  b  e","par mot"),
 ]),
 ("Dans l'écran", [
  ("g a","avy — n'importe où"), ("SPC j c","deux caractères"),
  ("SPC j l","une ligne"), ("SPC j w","un mot"),
  ("x X t m y z","actions avy pendant les étiquettes"),
  ("SPC j r","reprendre"), ("SPC j b","revenir"),
 ]),
 ("Dans le fichier", [
  ("SPC s i","symboles (imenu)"), ("SPC s s","chercher"),
  ("C-c o n / p","nœud frère"), ("C-c o u / d","parent / enfant"),
  ("g d","définition"), ("g D","références"), ("C-o  C-i","retour / avance"),
 ]),
 ("Dans le projet", [
  ("SPC SPC","un fichier"), ("SPC /","une chaîne"),
  ("SPC c j","un symbole LSP"), ("SPC ,","un buffer"),
  ("SPC p p","changer de projet"),
 ]),
 ("Entre projets (herdr)", [
  ("Ctrl+B p","monter un projet"), ("Ctrl+B h","le space quotidien"),
  ("Ctrl+B w","navigator"), ("Ctrl+B w Ctrl+W","spaces"),
  ("Ctrl+B w Ctrl+A","agents"), ("Ctrl+B w Ctrl+S","sessions"),
  ("Ctrl+B w Ctrl+N","créer"), ("Ctrl+B o","aller à la notif"),
 ]),
 ("Fenêtres", [
  ("SPC w V","split vertical + focus"), ("SPC w S","horizontal + focus"),
  ("C-w h j k l","circuler"), ("SPC w m m","plein écran"),
  ("SPC w u","annuler la disposition"),
 ]),
 ("Agir", [
  ("SPC c a","actions de code"), ("SPC c r","renommer"),
  ("SPC d d","déboguer"), ("SPC t f","diagnostics"),
  ("Ctrl+B d","reviewer un diff"), ("Ctrl+B Alt+K","tuer un process"),
 ]),
])
S.append(section("07","Index de navigation", s7))

# ══════════════════════════════════════════════════════════════════════
TOC = [("01","Ce que disent tes dépôts"),("02","Naviguer, échelle par échelle"),
       ("03","Mises en situation"),("04","La boucle de dev"),
       ("05","Ce que je propose"),("06","Outils"),("07","Index")]
cov = cover("Étude de poste","Naviguer & coder",
  "Ce que 28 dépôts et 328 commits disent de ta façon de travailler,<br>"
  "et ce qu'on peut en faire.",
  TOC, "analyse du 21 août 2026", "Doom Emacs · herdr · opencode",
  stats=[("328","commits analysés"),("93 %","conventional"),("35 %","dépôts testés"),("3 %","avec pre-commit")])

d = os.path.dirname(__file__)
open(os.path.join(d,"workflow-cover.html"),"w",encoding="utf-8").write(page("Naviguer & coder", cov))
body = "".join(S).replace('<section>','<section class="cont">',1)
open(os.path.join(d,"workflow-body.html"),"w",encoding="utf-8").write(page("Naviguer & coder", body))
print("workflow:", len(S), "sections,", len(body), "octets")
