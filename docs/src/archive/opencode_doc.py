# -*- coding: utf-8 -*-
import sys, os
sys.path.insert(0, os.path.dirname(__file__))
from lib import *

K = kbd
L = '<span class="mono">&lt;leader&gt;</span>'   # = Ctrl+X
S = []

def lk(rest):
    """Raccourci leader : Ctrl+X puis <rest>."""
    return K("Ctrl+X") + " " + K(rest)

# ── 1. Modèle mental ─────────────────────────────────────────────────
s1 = """
<p class=lede>opencode est un agent de code en terminal, <b>entièrement local chez
toi</b> : tes modèles tournent sur Ollama, rien ne sort de la machine. Sa
particularité par rapport aux autres agents CLI est la <b>composition</b> — un agent
principal peut déléguer à des subagents spécialisés, chacun avec son modèle, sa
température et ses permissions propres.</p>

<h3>Les sept objets</h3>
""" + table(("Objet","Ce que c'est","Où il vit"),[
 ("<b>session</b>","Un fil de conversation, avec son historique, son coût et ses snapshots. Peut être forkée, exportée, reprise.","<code>opencode session</code>"),
 ("<b>agent</b>","Un mode de travail : modèle + température + prompt + permissions. <code>build</code> et <code>plan</code> chez toi.","<code>opencode.json</code> → <code>agent</code>"),
 ("<b>subagent</b>","Un agent invoquable <i>par</i> un agent. Il a son propre contexte — c'est ce qui protège le contexte principal.","<code>agent/*.md</code>"),
 ("<b>command</b>","Un prompt réutilisable, appelé par <code>/nom</code>. Peut cibler un agent et s'exécuter en sous-tâche.","<code>command/*.md</code>"),
 ("<b>skill</b>","Un bloc d'expertise chargé <b>à la demande</b> selon le contexte du projet.","<code>skill/&lt;nom&gt;/SKILL.md</code>"),
 ("<b>plugin</b>","Du JS qui écoute les évènements d'opencode et réagit.","<code>plugins/*.js</code>"),
 ("<b>permission</b>","La règle qui décide : autorisé, refusé, ou on demande.","<code>opencode.json</code> → <code>permission</code>"),
], classes=["k","d","c"]) + """

<h3>Ce qui distingue skill, command et subagent</h3>
<p>La confusion est classique et elle coûte cher en tokens. La règle :</p>
""" + table(("","Déclenché par","Coût en contexte","Bon pour"),[
 ("<b>skill</b>","opencode, automatiquement, quand le projet matche","<b>Nul</b> tant qu'il ne sert pas — seule la description est chargée","Des conventions (« en Python, on utilise uv et ruff »)"),
 ("<b>command</b>","Toi, en tapant <code>/nom</code>","Le prompt est injecté dans la session courante","Une tâche répétitive et cadrée (<code>/commit</code>, <code>/review</code>)"),
 ("<b>subagent</b>","L'agent principal, ou une command avec <code>subtask: true</code>","<b>Isolé</b> — seul le résultat revient","Un travail qui produit beaucoup de bruit (exploration, debug)"),
], classes=["k","d","d","d"]) + note('tip','La raison d\'être des subagents',
 "Un <code>scout</code> qui lit trente fichiers pour en trouver un remplit son "
 "contexte, pas le tien. Il rend une carte de dix lignes. C'est la seule technique "
 "qui permette de travailler longtemps sans compaction — et la compaction, c'est de "
 "l'oubli.") + """

<h3>Contexte : ce qui le protège chez toi</h3>
""" + table(("Réglage","Ta valeur","Effet"),[
 ("<code>compaction.auto</code>","<code>true</code>","Résume automatiquement quand la fenêtre sature"),
 ("<code>compaction.prune</code>","<code>true</code>","Élague les vieux résultats d'outils plutôt que tout résumer"),
 ("<code>tool_output.max_lines</code>","<code>1200</code>","Tronque une sortie d'outil trop longue"),
 ("<code>tool_output.max_bytes</code>","<code>40960</code>","40 Ko max par résultat"),
 ("<code>subagent_depth</code>","<code>1</code>","Un subagent ne peut pas en appeler un autre — pas de récursion"),
 ("<code>snapshot</code>","<code>true</code>","Instantanés du dépôt : permet <code>"+"undo"+"</code> après une édition"),
 ("<code>experimental.batch_tool</code>","<code>true</code>","Plusieurs appels d'outils groupés en un tour"),
], classes=["c","c","d"])
S.append(section("01", "Modèle mental", s1))

# ── 2. Configuration ─────────────────────────────────────────────────
s2 = """
<p class=lede>Deux fichiers, deux rôles. <code>opencode.json</code> décrit
<b>le comportement</b> (modèles, agents, permissions) ; <code>tui.json</code> décrit
<b>l'interface</b> (thème, clavier, notifications). Les deux acceptent commentaires
et virgules finales.</p>

<h3>Tes modèles — tout en local</h3>
""" + table(("Rôle","Modèle","Quand il sert"),[
 ("<code>model</code>","<code>ollama/qwen3-coder:30b</code>","Le modèle principal, agent <code>build</code>"),
 ("<code>small_model</code>","<code>ollama/qwen2.5vl:7b</code>","Titres de session, résumés, tâches secondaires. Fait aussi la vision"),
 ("<i>déclaré</i>","<code>ollama/gpt-oss:120b</code>","Raisonnement : agent <code>plan</code>, <code>reviewer</code>, <code>debugger</code>"),
 ("<i>déclaré</i>","<code>ollama/devstral:24b</code>","Agentique — bon en boucle outil"),
 ("<i>déclaré</i>","<code>ollama/qwen3-coder:480b-cloud</code>","Le gros calibre, en cloud Ollama"),
], classes=["c","c","d"]) + """
<pre><span class=k>"provider"</span>: {
  <span class=s>"ollama"</span>: {
    <span class=s>"npm"</span>: <span class=s>"@ai-sdk/openai-compatible"</span>,   <span class=c># Ollama expose une API compatible OpenAI</span>
    <span class=s>"options"</span>: { <span class=s>"baseURL"</span>: <span class=s>"http://localhost:11434/v1"</span> },
    <span class=s>"models"</span>: { <span class=s>"qwen3-coder:30b"</span>: { <span class=s>"name"</span>: <span class=s>"Qwen3 Coder 30B — build"</span> } }
  }
}</pre>
""" + note('note','Ajouter un provider cloud',
 "<code>opencode auth login</code> (alias de <code>opencode providers</code>) puis "
 "<code>\"model\": \"anthropic/claude-opus-5\"</code>. Les deux cohabitent : un agent "
 "peut rester local pendant qu'un autre part en cloud.") + """

<h3>Le contexte injecté à chaque session</h3>
""" + table(("Fichier","Portée","Précédence"),[
 ("<code>~/.config/opencode/AGENTS.md</code>","Global — tes conventions de méthode, code, vérification, git, shell","Le plus faible"),
 ("<code>AGENTS.md</code> (projet)","Le dépôt courant","<b>Prime sur le global</b>"),
 ("<code>CONTRIBUTING.md</code>","Le dépôt courant, si présent",""),
 ("<code>docs/architecture.md</code>","Le dépôt courant, si présent",""),
], classes=["c","d","d"]) + """

<h3>Interface — <code>tui.json</code></h3>
""" + table(("Clé","Ta valeur","Effet"),[
 ("<code>theme</code>","<code>system</code>","<b>opencode hérite des 16 couleurs du terminal</b> — cohérence parfaite avec Alacritty"),
 ("<code>diff_style</code>","<code>auto</code>","Côte à côte si la largeur le permet, sinon unifié"),
 ("<code>attention.enabled</code>","<code>true</code>","Notification + son quand l'agent a besoin de toi"),
 ("<code>attention.volume</code>","<code>0.35</code>",""),
 ("<code>prompt.max_height</code>","<code>20</code>","Hauteur maximale de la zone de saisie"),
 ("<code>leader_timeout</code>","<code>800</code>","Millisecondes pour enchaîner après le leader"),
 ("<code>cursor.blinking</code>","<code>false</code>",""),
 ("<code>mouse</code>","<code>true</code>","+ <code>scroll_acceleration</code>"),
], classes=["c","c","d"]) + """

<h3>Où vit quoi</h3>
""" + table(("Chemin","Contenu"),[
 ("<code>~/.config/opencode/opencode.json</code>","Comportement : modèles, agents, skills, permissions, watcher"),
 ("<code>~/.config/opencode/tui.json</code>","Interface : thème, clavier, notifications"),
 ("<code>~/.config/opencode/AGENTS.md</code>","Tes conventions globales"),
 ("<code>~/.config/opencode/agent/*.md</code>","Un fichier par subagent"),
 ("<code>~/.config/opencode/command/*.md</code>","Un fichier par commande <code>/nom</code>"),
 ("<code>~/.config/opencode/skill/&lt;nom&gt;/SKILL.md</code>","Un dossier par skill"),
 ("<code>~/.config/opencode/plugins/*.js</code>","Plugins JS"),
 ("<code>opencode debug paths</code>","Imprime data / config / cache / state réels"),
], classes=["c","d"])
S.append(section("02", "Ta configuration", s2))

# ── 3. Agents ────────────────────────────────────────────────────────
ag = []
ag.append(card("build <span class=tag>principal</span>", None,
  "Le mode par défaut (<code>default_agent</code>). Il édite, teste, itère. "
  "Température <b>0.1</b> : on veut du déterminisme, pas de la créativité.",
  [("Modèle","<code>ollama/qwen3-coder:30b</code>"),
   ("Édition","autorisée"),
   ("Bascule", K("Tab")+" cycle les agents · "+lk("a")+" ouvre la liste")],
  headers=("","")))
ag.append(card("plan <span class=tag>principal</span>", None,
  "Analyse et conception, <b>sans écrire de code</b>. Température 0.3 — un peu plus "
  "de latitude pour explorer des options. À utiliser avant toute tâche dont tu ne "
  "connais pas encore la forme.",
  [("Modèle","<code>ollama/gpt-oss:120b</code> — le modèle de raisonnement"),
   ("Édition","refusée par construction")],
  headers=("","")))
ag.append(card("scout <span class=tag>subagent</span>", None,
  "Explore la codebase et rend une <b>carte compacte</b> : fichiers, symboles, points "
  "d'entrée. Lecture seule, modèle rapide. C'est lui qui absorbe le bruit de "
  "l'exploration à la place de ta session.",
  [("Modèle · température","<code>qwen3-coder:30b</code> · <code>0</code>"),
   ("edit","<code>deny</code>"),
   ("bash","<code>deny</code> sauf <code>rg</code>, <code>fd</code>, <code>git ls-files</code>, <code>git log</code>"),
   ("Appel","<code>/scout</code> (avec <code>subtask: true</code>)")],
  headers=("","")))
ag.append(card("reviewer <span class=tag>subagent</span>", None,
  "Relit un diff ou un fichier et ne remonte <b>que les vrais défauts</b> : bugs, "
  "sécurité, performance. Pas de commentaire de style.",
  [("Modèle · température","<code>gpt-oss:120b</code> · <code>0.1</code>"),
   ("edit","<code>deny</code>"),
   ("bash","<code>deny</code> sauf <code>git diff</code>, <code>git log</code>, <code>git show</code>, <code>git status</code>"),
   ("Appel","<code>/review</code>")],
  headers=("","")))
ag.append(card("debugger <span class=tag>subagent</span>", None,
  "Diagnostique à partir d'une erreur, d'une stacktrace ou d'un comportement "
  "inattendu. <b>Reproduit avant de corriger</b> — c'est sa consigne centrale. "
  "Température 0.",
  [("Modèle · température","<code>gpt-oss:120b</code> · <code>0</code>"),
   ("edit","<code>deny</code>"),
   ("bash","<code>ask</code> par défaut, <code>allow</code> pour <code>rg</code>, <code>fd</code>, <code>git log</code>, <code>git diff</code>"),
   ("Appel","<code>/fix</code>")],
  headers=("","")))
ag.append(card("tester <span class=tag>subagent</span>", None,
  "Écrit des tests qui <b>échouent d'abord</b>. Sa consigne : « un test qui passe du "
  "premier coup sans avoir jamais échoué ne prouve rien ». Il lit les tests "
  "existants avant d'écrire, pour respecter framework et nommage.",
  [("Modèle · température","<code>qwen3-coder:30b</code> · <code>0.1</code>"),
   ("Édition","autorisée — c'est le seul subagent qui écrit"),
   ("Appel","<code>/test</code>")],
  headers=("","")))

s3 = """
<p class=lede>Deux agents principaux, quatre subagents. La distinction qui compte :
un agent <b>principal</b> est un mode dans lequel <i>tu</i> te places ; un
<b>subagent</b> est un outil que l'agent principal invoque, et dont seul le
résultat te revient.</p>
""" + "".join(ag) + """
<h3>Écrire un subagent</h3>
<pre><span class=c>--- ~/.config/opencode/agent/mon-agent.md</span>
<span class=k>---</span>
description: <span class=s>Ce que fait cet agent. C'est CE TEXTE qui décide quand</span>
             <span class=s>l'agent principal choisit de l'appeler — sois précis.</span>
mode: <span class=s>subagent</span>          <span class=c># subagent | primary | all</span>
model: <span class=s>ollama/gpt-oss:120b</span>
temperature: <span class=s>0</span>
color: <span class=s>warning</span>          <span class=c># info | success | warning | error</span>
permission:
  edit: <span class=s>deny</span>
  bash:
    <span class=s>"*"</span>: <span class=s>deny</span>
    <span class=s>"git diff*"</span>: <span class=s>allow</span>
<span class=k>---</span>

Le corps est le prompt système de l'agent. Écris-y sa méthode,
pas seulement son rôle.</pre>
""" + note('warn','La description est la seule interface',
 "L'agent principal ne lit pas le corps de tes subagents pour décider : il lit la "
 "<code>description</code>. Une description vague donne un subagent jamais appelé, "
 "ou appelé à tort. Écris-y le <b>déclencheur</b>, pas le CV.") + """
<h3>Inspecter</h3>
""" + table(("Commande","Effet"),[
 ("<code>opencode agent</code>","Gérer les agents"),
 ("<code>opencode debug agent &lt;nom&gt;</code>","Config résolue d'un agent : modèle, prompt, permissions effectives"),
 ("<code>opencode --agent &lt;nom&gt;</code>","Démarrer directement dans un agent"),
], classes=["c","d"])
S.append(section("03", "Agents & subagents", s3))

# ── 4. Commandes ─────────────────────────────────────────────────────
s4 = """
<p class=lede>Neuf commandes chez toi. Une commande est un prompt réutilisable dans
un fichier Markdown : le frontmatter dit <i>qui</i> l'exécute, le corps dit
<i>quoi</i>. Tape <code>/</code> dans la saisie pour les lister, ou """ + K("Ctrl+P") + """.</p>
""" + table(("Commande","Ce qu'elle fait","Agent","Sous-tâche"),[
 ("<code>/commit</code>","Rédige et crée un commit à partir du <i>staging</i>","build","—"),
 ("<code>/absorb</code>","Range les modifications en cours dans les bons commits de la pile","build","—"),
 ("<code>/pr</code>","Rédige et ouvre une pull request","build","—"),
 ("<code>/review</code>","Relit les changements non commités","<b>reviewer</b>","—"),
 ("<code>/fix</code>","Diagnostique une erreur puis corrige la cause racine","<b>debugger</b>","—"),
 ("<code>/test</code>","Lance les tests du projet et corrige ce qui casse","build","—"),
 ("<code>/scout</code>","Localise du code dans la codebase","<b>scout</b>","<b>oui</b>"),
 ("<code>/explain</code>","Explique un fichier, un symbole ou un flux","<b>plan</b>","—"),
 ("<code>/perf</code>","Mesure <b>avant</b> d'optimiser (hyperfine / profilage)","build","—"),
], classes=["c","d","d","d"]) + note('note','subtask: true',
 "<code>/scout</code> est la seule commande marquée <code>subtask</code> : elle "
 "s'exécute dans un contexte <b>séparé</b> et ne rend que sa conclusion. C'est ce "
 "qu'on veut d'une exploration — le résultat, pas les trente fichiers lus.") + """

<h3>Écrire une commande</h3>
<pre><span class=c>--- ~/.config/opencode/command/audit.md</span>
<span class=k>---</span>
description: <span class=s>Audite les dépendances et signale les CVE</span>
agent: <span class=s>reviewer</span>       <span class=c># optionnel : force l'agent</span>
subtask: <span class=s>true</span>         <span class=c># optionnel : contexte isolé</span>
model: <span class=s>ollama/gpt-oss:120b</span>  <span class=c># optionnel : force le modèle</span>
<span class=k>---</span>

Le corps est le prompt. <span class=s>$ARGUMENTS</span> reçoit ce qui suit /audit.
Tu peux aussi injecter la sortie d'une commande shell :

Dépendances actuelles :
!`uv pip list --outdated`</pre>
""" + note('tip','Les deux échappatoires utiles',
 "<code>$ARGUMENTS</code> pour paramétrer, et <code>!`commande`</code> pour injecter la "
 "sortie d'un shell dans le prompt <b>au moment de l'appel</b>. Une commande "
 "<code>/ctx</code> qui fait <code>!`git status -sb &amp;&amp; git diff --stat`</code> "
 "économise trois allers-retours à chaque session.")
S.append(section("04", "Commandes", s4))

# ── 5. Skills ────────────────────────────────────────────────────────
s5 = """
<p class=lede>Un skill est un bloc d'expertise que opencode charge <b>uniquement si
le contexte du projet le justifie</b>. Au démarrage, seule la <code>description</code>
est lue — quelques dizaines de tokens. Le corps n'est injecté que lorsqu'il sert.
C'est le mécanisme le moins cher pour transporter des conventions.</p>
""" + table(("Skill","Déclencheur","Couvre"),[
 ("<code>python-uv</code>","<code>pyproject.toml</code>, <code>requirements.txt</code>, <code>.py</code>, <code>setup.py</code>, <code>pytest.ini</code>, <code>tox.ini</code>","uv, ruff, pyright, pytest, venv, pièges d'empaquetage"),
 ("<code>typescript-node</code>","<code>package.json</code>, <code>tsconfig.json</code>, <code>.ts/.tsx/.js/.jsx</code>","npm, TypeScript strict, prettier, tests, pièges async"),
 ("<code>rust</code>","<code>Cargo.toml</code>, <code>.rs</code>","cargo, clippy, gestion d'erreurs, emprunt"),
 ("<code>go</code>","<code>go.mod</code>, <code>.go</code>","commandes go, erreurs, concurrence, tests table-driven"),
 ("<code>containers</code>","<code>Dockerfile</code>, <code>docker-compose.yml</code>, <code>Chart.yaml</code>, <code>kustomization.yaml</code>","Docker, Compose, Kubernetes"),
 ("<code>data-science</code>","<code>.ipynb</code>, pandas, numpy, sklearn, polars, torch, <code>data/</code>","notebooks, analyse, ML"),
 ("<code>shell</code>","<code>.sh/.bash/.zsh</code>, <code>.zshrc</code>, <code>Makefile</code>, shebang shell","scripts et config de shell"),
], classes=["c","c","d"]) + """
<h3>Écrire un skill</h3>
<pre><span class=c>--- ~/.config/opencode/skill/terraform/SKILL.md</span>
<span class=k>---</span>
name: <span class=s>terraform</span>
description: <span class=s>Infrastructure as code. À utiliser dès qu'un .tf,</span>
             <span class=s>terraform.tfvars ou un dossier .terraform est présent.</span>
<span class=k>---</span>

<span class=c># Le corps : les conventions, chargées seulement quand ça matche.</span>

## Commandes
- `terraform fmt -recursive` avant tout commit
- `terraform plan -out=tfplan` puis `terraform apply tfplan` — jamais
  `apply` sans plan enregistré</pre>
""" + table(("Commande","Effet"),[
 ("<code>opencode debug skill</code>","Liste tous les skills visibles et leur origine"),
 ("<code>\"skills\": {\"paths\": [...]}</code>","Où opencode les cherche — chez toi <code>~/.config/opencode/skill</code>"),
], classes=["c","d"]) + note('warn','La description est un déclencheur, pas un résumé',
 "Comme pour les subagents : ce texte est la <b>seule</b> chose lue au démarrage. "
 "Nomme-y les fichiers et extensions qui doivent l'activer. « Bonnes pratiques "
 "Python » ne déclenche rien ; « dès qu'un <code>pyproject.toml</code> est présent » "
 "déclenche.")
S.append(section("05", "Skills", s5))

# ── 6. Permissions ───────────────────────────────────────────────────
s6 = """
<p class=lede>Ta ligne de conduite : <b>lecture et édition libres, confirmation sur
tout ce qui sort du dépôt ou détruit du travail</b>. C'est le bon réglage — il
supprime la fatigue d'approbation sur ce qui est réversible, et ne la garde que là
où elle protège vraiment.</p>
""" + table(("Outil","Réglage","Pourquoi"),[
 ("<code>read</code> <code>grep</code> <code>glob</code> <code>list</code>","<code>allow</code>","Lire ne casse rien"),
 ("<code>edit</code>","<code>allow</code>","Git est le filet ; <code>snapshot: true</code> ajoute l'annulation"),
 ("<code>webfetch</code>","<code>allow</code>","Consultation de doc"),
], classes=["c","c","d"]) + """
<h3>Répertoires hors du projet</h3>
<pre><span class=k>"external_directory"</span>: {
  <span class=s>"*"</span>: <span class=s>"ask"</span>,
  <span class=s>"/Users/youcef/.config/opencode/**"</span>: <span class=s>"allow"</span>,
  <span class=s>"/Users/youcef/.agents/skills/**"</span>: <span class=s>"allow"</span>
}</pre>
""" + note('note','Pourquoi ces deux exceptions',
 "Les skills et la config d'opencode vivent <b>hors</b> du projet courant. Sans ces "
 "règles, chaque lecture de skill déclenchait une demande d'autorisation — soit une "
 "interruption à chaque changement de langage.") + """
<h3>Les commandes shell qui demandent</h3>
<p>Tout est <code>allow</code> par défaut, sauf cette liste. Elle est bien construite :
elle couvre les trois familles de dégâts — <b>détruire des fichiers</b>,
<b>détruire du travail git</b>, <b>agir sur le monde extérieur</b>.</p>
""" + table(("Famille","Motifs mis en ask"),[
 ("Destruction de fichiers","<code>rm -rf *</code> · <code>rm -r *</code> · <code>sudo *</code>"),
 ("Destruction de travail git","<code>git reset --hard*</code> · <code>git clean*</code> · <code>git checkout -- *</code>"),
 ("Effets externes irréversibles","<code>git push*</code> · <code>gh pr create*</code> · <code>gh pr merge*</code> · <code>npm publish*</code> · <code>cargo publish*</code>"),
 ("Infrastructure","<code>docker system prune*</code> · <code>kubectl delete*</code> · <code>terraform apply*</code> · <code>terraform destroy*</code>"),
 ("Système &amp; chaîne d'appro.","<code>brew uninstall*</code> · <code>curl * | sh</code> · <code>curl * | bash</code>"),
], classes=["k","d"]) + note('warn','--auto et --pure',
 "<code>opencode --auto</code> approuve automatiquement tout ce qui n'est pas "
 "explicitement <code>deny</code> — <b>y compris tes règles <code>ask</code></b>. "
 "Réserve-le à un conteneur jetable. <code>--pure</code> démarre sans les plugins "
 "externes : c'est le premier réflexe quand opencode se comporte bizarrement.") + """
<h3>Les autres garde-fous actifs</h3>
""" + table(("Réglage","Valeur","Effet"),[
 ("<code>share</code>","<code>disabled</code>","<b>Aucune session ne peut être partagée en ligne.</b> Cohérent avec le tout-local"),
 ("<code>autoupdate</code>","<code>notify</code>","Prévient, ne met pas à jour tout seul"),
 ("<code>watcher.ignore</code>","10 motifs","<code>node_modules</code>, <code>.git</code>, <code>dist</code>, <code>build</code>, <code>target</code>, <code>.venv</code>, <code>__pycache__</code>, <code>.next</code>, <code>coverage</code>"),
 ("<code>lsp</code> · <code>formatter</code>","<code>true</code>","rust-analyzer, gopls, pyright, ruff, eslint détectés seuls"),
], classes=["c","c","d"])
S.append(section("06", "Permissions & garde-fous", s6))

# ── 7. Clavier ───────────────────────────────────────────────────────
s7 = """
<p class=lede>Ton leader est """ + K("Ctrl+X") + """ (défaut opencode), avec 800 ms pour
enchaîner. Dans les tableaux qui suivent, """ + L + """ signifie « """ + K("Ctrl+X") + """
puis la touche ». Les lignes marquées <b>◆</b> sont des surcharges de ta
<code>tui.json</code> ; le reste est le défaut opencode.</p>

<h3>Session</h3>
""" + table(("Touche","Action"),[
 (lk("n"),"Nouvelle session"),
 (lk("l"),"Lister les sessions"),
 (lk("g"),"Timeline de la session"),
 (lk("c"),"Compacter la session (résumer pour libérer du contexte)"),
 (lk("x"),"Exporter la session"),
 (K("Ctrl+R"),"Renommer la session"),
 (K("Ctrl+D"),"Supprimer la session (dans la liste)"),
 (K("Échap"),"<b>Interrompre l'agent</b>"),
 (lk("q")+" "+K("Ctrl+C"),"Quitter"),
 (lk("s"),"Voir le statut"),
 (lk("b"),"Replier / déplier la sidebar"),
], classes=["k","d"]) + """
<h3>Sessions enfants (subagents)</h3>
""" + table(("Touche","Action"),[
 (lk("↓"),"Entrer dans la première session enfant"),
 (K("→")+" / "+K("←"),"Cycler entre les sessions enfants"),
 (K("↑"),"Remonter à la session parente"),
], classes=["k","d"]) + """
<h3>Modèles &amp; agents</h3>
""" + table(("Touche","Action"),[
 (lk("m"),"Liste des modèles"),
 (lk("a"),"Liste des agents"),
 (K("Tab")+" / "+K("Shift+Tab"),"Agent suivant / précédent"),
 (K("Ctrl+A"),"Liste des providers"),
 (K("Ctrl+F"),"Marquer le modèle comme favori"),
 (K("F2")+" / "+K("Shift+F2"),"Modèle récent suivant / précédent"),
 (K("Ctrl+T"),"Cycler les variantes de modèle"),
], classes=["k","d"]) + """
<h3>Lire la conversation</h3>
""" + table(("Touche","Action"),[
 (K("PgUp")+" / "+K("PgDn"),"Page haut / bas · aussi "+K("Ctrl+Alt+B")+" / "+K("Ctrl+Alt+F")),
 (K("Ctrl+Alt+U")+" / "+K("Ctrl+Alt+D"),"Demi-page haut / bas"),
 (K("Ctrl+Alt+Y")+" / "+K("Ctrl+Alt+E"),"Ligne haut / bas"),
 (K("Ctrl+G")+" / "+K("Ctrl+Alt+G"),"Premier / dernier message"),
 (lk("y"),"Copier le message"),
 (lk("u")+" / "+lk("r"),"<b>Annuler / refaire</b> — s'appuie sur les snapshots"),
 (lk("h"),"Masquer / afficher les détails (et les astuces)"),
], classes=["k","d"]) + """
<h3>Éditer le prompt</h3>
""" + table(("Touche","Action"),[
 ("<b>◆</b> "+K("Shift+Entrée")+" "+K("Alt+Entrée")+" "+K("Ctrl+J"),"<b>Retour à la ligne sans envoyer.</b> Trois formes : Alacritty envoie ESC+CR sur ⇧⏎ et ⌘⏎, <code>ctrl+j</code> est le repli universel (tmux, SSH)"),
 (K("Entrée"),"Envoyer"),
 ("<b>◆</b> "+K("Ctrl+A")+" / "+K("Ctrl+E"),"Début / fin de ligne (+ "+K("Home")+" "+K("Fin")+")"),
 ("<b>◆</b> "+K("Alt+←")+" / "+K("Alt+→"),"Mot précédent / suivant (+ "+K("Alt+B")+" "+K("Alt+F")+")"),
 ("<b>◆</b> "+K("Ctrl+W")+" / "+K("Alt+⌫"),"Supprimer le mot précédent"),
 ("<b>◆</b> "+K("Ctrl+U")+" / "+K("Ctrl+K"),"Supprimer jusqu'au début / à la fin de la ligne"),
 (K("Alt+D"),"Supprimer le mot suivant"),
 (K("Ctrl+Shift+D"),"Supprimer la ligne"),
 (K("Ctrl+-")+" / "+K("Ctrl+."),"Annuler / refaire la saisie"),
 (K("Ctrl+V"),"Coller (images comprises)"),
 (K("Ctrl+C"),"Vider la saisie"),
 (K("Cmd+A"),"Tout sélectionner · "+K("Shift+←→↑↓")+" étendre la sélection"),
 (K("↑")+" / "+K("↓"),"Historique des prompts"),
], classes=["k","d"]) + """
<h3>Dialogues, autocomplétion, diff</h3>
""" + table(("Touche","Action"),[
 (K("↑")+" "+K("Ctrl+P")+" / "+K("↓")+" "+K("Ctrl+N"),"Élément précédent / suivant"),
 (K("Entrée"),"Valider · "+K("Tab")+" complète dans l'autocomplétion"),
 (K("Échap"),"Fermer"),
 (K("Espace"),"Cocher (MCP, plugins)"),
 (K("Ctrl+P"),"<b>Palette de commandes</b>"),
 (K("Ctrl+F"),"Plein écran sur une demande de permission"),
 ("Diff",K("]")+" "+K("[")+" hunk · fichier suivant/précédent · repli de l'arbre"),
], classes=["k","d"]) + """
<h3>Divers</h3>
""" + table(("Touche","Action"),[
 (lk("e"),"Ouvrir l'éditeur externe"),
 (lk("t"),"Changer de thème"),
 (K("Ctrl+Z"),"Suspendre le process (<code>fg</code> pour revenir)"),
 (K("Ctrl+Alt+K"),"<b>which-key</b> — l'aide-mémoire des accords en direct"),
 (K("Ctrl+Alt+←")+" / "+K("Ctrl+Alt+→"),"which-key : groupe précédent / suivant"),
], classes=["k","d"]) + note('tip','which-key',
 K("Ctrl+Alt+K")+" affiche en direct toutes les continuations possibles après le "
 "leader. C'est l'équivalent de "+K("Ctrl+B")+" "+K("?")+" dans herdr : à utiliser "
 "au lieu de mémoriser.")
S.append(section("07", "Le clavier", s7))

# ── 8. CLI ───────────────────────────────────────────────────────────
def cli(rows):
    return table(("Commande","Effet"), rows, cls="tight", classes=["c","d"])

s8 = """
<p class=lede>opencode s'utilise autant sans interface qu'avec. Le mode
<code>run</code> en fait un outil de pipeline — c'est ce qu'appelle ta quick action
herdr <code>opencode run '/review'</code>.</p>

<h3>Démarrer</h3>
""" + cli([
 ("opencode [chemin]","Lancer le TUI (commande par défaut)"),
 ("opencode -c","<b>Continuer la dernière session</b>"),
 ("opencode -s &lt;id&gt;","Reprendre une session précise"),
 ("opencode --fork","Forker la session au lieu de la continuer (avec <code>-c</code> ou <code>-s</code>)"),
 ("opencode -m ollama/gpt-oss:120b","Forcer le modèle"),
 ("opencode --agent plan","Démarrer dans un agent donné"),
 ("opencode --prompt \"…\"","Pré-remplir le prompt"),
 ("opencode --mini","Interface minimale"),
 ("opencode --pure","<b>Sans les plugins externes</b> — le premier réflexe de diagnostic"),
 ("opencode --auto","Auto-approuve tout ce qui n'est pas <code>deny</code>. <b>Dangereux</b>"),
 ("opencode run [message…]","Un tour, sans TUI. Idéal en script"),
]) + """
<h3>Sessions &amp; données</h3>
""" + cli([
 ("opencode session","Gérer les sessions"),
 ("opencode export [sessionID]","Exporter en JSON"),
 ("opencode import &lt;fichier|url&gt;","Importer"),
 ("opencode stats","<b>Tokens et coûts</b>"),
 ("opencode db","Outils base de données"),
]) + """
<h3>Modèles, providers, agents</h3>
""" + cli([
 ("opencode models [provider]","Lister tous les modèles disponibles"),
 ("opencode providers","Gérer providers et identifiants — <b>alias : <code>auth</code></b>"),
 ("opencode agent","Gérer les agents"),
 ("opencode mcp","Gérer les serveurs MCP"),
 ("opencode plugin &lt;module&gt;","Installer un plugin et mettre à jour la config — <b>alias : <code>plug</code></b>"),
]) + """
<h3>Serveur, web, GitHub</h3>
""" + cli([
 ("opencode serve [--port N] [--hostname H]","Serveur headless"),
 ("opencode web","Serveur + interface web"),
 ("opencode attach &lt;url&gt;","S'attacher à un serveur qui tourne"),
 ("opencode acp","Serveur ACP (Agent Client Protocol) — pour Zed et consorts"),
 ("opencode --mdns","Découverte de service mDNS (<code>opencode.local</code>)"),
 ("opencode github","Gérer l'agent GitHub"),
 ("opencode pr &lt;numéro&gt;","<b>Récupère la branche d'une PR, la checkout, lance opencode dessus</b>"),
]) + """
<h3>Diagnostic</h3>
""" + cli([
 ("opencode debug config","<b>Configuration résolue</b> — la vérité après fusion des fichiers"),
 ("opencode debug paths","Chemins réels : data, config, cache, state"),
 ("opencode debug agent &lt;nom&gt;","Config effective d'un agent"),
 ("opencode debug skill","Skills visibles et leur origine"),
 ("opencode debug lsp | rg | file","Diagnostic LSP / ripgrep / système de fichiers"),
 ("opencode debug startup","Chronométrage du démarrage"),
 ("opencode debug info","Informations générales"),
 ("opencode --print-logs --log-level DEBUG","Logs sur stderr"),
 ("opencode upgrade [cible]","Mettre à jour"),
])
S.append(section("08", "La CLI", s8))

# ── 9. Intégration ───────────────────────────────────────────────────
s9 = """
<h3>Dans herdr</h3>
<p class=lede>Le plugin <code>~/.config/opencode/plugins/herdr-agent-state.js</code>
(intégration <b>v9</b>, posée par <code>herdr integration install opencode</code>)
projette l'état d'opencode vers herdr par la socket. C'est lui qui fait qu'un pane
opencode apparaît <i>working</i>, <i>blocked</i> ou <i>done</i> dans la sidebar — et
donc que les notifications macOS et """ + K("Ctrl+B") + " " + K("o") + """ fonctionnent.</p>
""" + table(("Évènement opencode","État herdr projeté"),[
 ("<code>permission.asked</code> · <code>question.asked</code>","<code>blocked</code> — <b>c'est toi qu'on attend</b>"),
 ("<code>permission.replied</code> · <code>question.replied</code> · <code>question.rejected</code>","<code>working</code>"),
 ("Fin de tour, non vue","<code>done</code> → notification"),
], classes=["c","d"]) + note('warn','Ne pas éditer ce fichier',
 "Il est géré par herdr : réinstaller ou mettre à jour l'intégration l'écrase. "
 "Pour ajouter tes propres hooks, crée un <b>autre</b> fichier à côté dans "
 "<code>plugins/</code>. Après un <code>herdr update</code>, vérifie avec "
 "<code>herdr integration status</code> — une intégration <i>outdated</i> et les "
 "états ne remontent plus.") + """
<p>Les sessions enfants (subagents) sont suivies séparément : leurs évènements ne
peuvent pas remplacer la session racine du pane, mais leurs demandes de permission
te bloquent quand même — ce qui est le comportement voulu.</p>

<h3>MCP</h3>
<p>Désactivé chez toi, l'exemple est en commentaire dans <code>opencode.json</code> :</p>
<pre><span class=k>"mcp"</span>: {
  <span class=s>"github"</span>: {
    <span class=s>"type"</span>: <span class=s>"local"</span>,
    <span class=s>"command"</span>: [<span class=s>"npx"</span>, <span class=s>"-y"</span>, <span class=s>"@modelcontextprotocol/server-github"</span>],
    <span class=s>"enabled"</span>: <span class=k>true</span>,
    <span class=s>"environment"</span>: { <span class=s>"GITHUB_TOKEN"</span>: <span class=s>"{env:GITHUB_TOKEN}"</span> }
  }
}</pre>
<p><code>{env:VAR}</code> est la syntaxe d'interpolation : <b>aucun secret en dur</b>
dans le fichier — conforme à ce que dit ton <code>AGENTS.md</code>.
<code>opencode mcp</code> gère les serveurs, "
""" + K("Espace") + """ les active dans le dialogue.</p>

<h3>Ollama — les vérifications qui servent</h3>
<pre>ollama list                    <span class=c># les modèles présents localement</span>
ollama ps                      <span class=c># ce qui est chargé en mémoire maintenant</span>
curl -s localhost:11434/v1/models | jq -r '.data[].id'   <span class=c># ce que voit opencode</span>
opencode models ollama         <span class=c># ce que opencode expose</span></pre>
""" + note('note','Si opencode ne voit aucun modèle',
 "Dans l'ordre : le serveur Ollama tourne-t-il (<code>ollama ps</code>) ? "
 "l'endpoint <code>http://localhost:11434/v1</code> répond-il ? le modèle est-il "
 "<b>tiré</b> (<code>ollama pull qwen3-coder:30b</code>) ? "
 "<code>opencode debug config</code> montre la config réellement chargée.")
S.append(section("09", "Intégrations", s9))

# ── 10. Recettes ─────────────────────────────────────────────────────
s10 = """
<h3>La boucle qui marche</h3>
""" + steps([
 "<b>Cadrer avant d'écrire.</b> "+K("Tab")+" jusqu'à <code>plan</code>, ou "
 "<code>/explain</code>. Un modèle de raisonnement (<code>gpt-oss:120b</code>) sur la "
 "conception, un modèle de code sur l'exécution.",
 "<b>Localiser sans polluer.</b> <code>/scout où est géré le rate-limit</code> — "
 "l'exploration se fait dans un contexte isolé, tu ne reçois que la carte.",
 "<b>Implémenter.</b> "+K("Tab")+" vers <code>build</code>. Édition libre : git est le filet.",
 "<b>Prouver.</b> <code>/test</code> — le subagent <code>tester</code> écrit des tests "
 "qui échouent d'abord. Un test vert du premier coup ne prouve rien.",
 "<b>Relire.</b> <code>/review</code> — <code>reviewer</code> est en lecture seule et ne "
 "remonte que les vrais défauts.",
 "<b>Livrer.</b> <code>/commit</code> puis <code>/pr</code>. Le <code>git push</code> "
 "demandera confirmation — c'est voulu.",
]) + """
<h3>Quand ça part de travers</h3>
""" + table(("Situation","Le geste"),[
 ("L'agent part dans la mauvaise direction", K("Échap")+" interrompt immédiatement. Puis "+lk("u")+" annule le dernier tour"),
 ("Il a édité un fichier qu'il ne fallait pas", lk("u")+" — les snapshots permettent de revenir"),
 ("Le contexte est saturé", lk("c")+" compacte. Mieux : déléguer à un subagent <b>avant</b> d'en arriver là"),
 ("La réponse est mauvaise, le prompt était bon","Change de modèle ("+lk("m")+") plutôt que de reformuler trois fois"),
 ("opencode se comporte bizarrement","<code>opencode --pure</code> — sans plugins. Si ça marche, c'est un plugin"),
 ("Tu ne sais plus quelle config s'applique","<code>opencode debug config</code>"),
], classes=["d","d"]) + """
<h3>Le duo herdr + opencode</h3>
""" + table(("Geste","Où"),[
 ("Monter un projet complet avec opencode déjà lancé", K("Ctrl+B")+" "+K("Alt+O")+" — tes templates ont tous un tab <code>agent: opencode</code>"),
 ("Un worktree par tâche, un opencode par worktree", K("Ctrl+B")+" "+K("Shift+G")),
 ("Sauter sur celui qui te bloque", K("Ctrl+B")+" "+K("o")),
 ("Relire son diff et lui renvoyer des commentaires", K("Ctrl+B")+" "+K("d")+" (reviewr), puis "+K("s")),
 ("Retrouver une conversation opencode passée", K("Ctrl+B")+" "+K("m")+" (memex)"),
 ("Voir ce que la session a coûté", K("Ctrl+B")+" "+K("Alt+T")+" · <code>opencode stats</code>"),
 ("Review sans quitter le shell","<code>opencode run '/review'</code> — c'est ta quick action "+K("Ctrl+B")+" "+K("Alt+A")),
], classes=["d","k"]) + """
<h3>Index — leader """ + K("Ctrl+X") + """</h3>
""" + idx([
 ("Après le leader", [
  ("a","liste des agents"), ("b","sidebar"), ("c","compacter"),
  ("e","éditeur externe"), ("g","timeline"), ("h","masquer les détails"),
  ("l","lister les sessions"), ("m","liste des modèles"), ("n","nouvelle session"),
  ("q","quitter"), ("r","refaire"), ("s","statut"), ("t","thème"),
  ("u","annuler"), ("x","exporter"), ("↓","session enfant"),
 ]),
 ("Sans leader", [
  ("Échap","interrompre l'agent"), ("Tab","agent suivant"),
  ("Ctrl+P","palette de commandes"), ("Ctrl+R","renommer la session"),
  ("Ctrl+A","providers"), ("Ctrl+F","favori / plein écran"),
  ("Ctrl+T","variantes de modèle"), ("F2","modèle récent"),
  ("Ctrl+Z","suspendre"), ("Ctrl+Alt+K","which-key"),
  ("Ctrl+G","premier message"), ("↑ ↓","historique des prompts"),
 ]),
 ("Saisie", [
  ("Ctrl+J","retour à la ligne"), ("Ctrl+A / Ctrl+E","début / fin de ligne"),
  ("Alt+B / Alt+F","mot arrière / avant"), ("Ctrl+W","supprimer le mot"),
  ("Ctrl+U / Ctrl+K","couper avant / après"), ("Ctrl+V","coller"),
  ("Ctrl+C","vider la saisie"), ("Ctrl+- / Ctrl+.","annuler / refaire"),
 ]),
])
S.append(section("10", "Recettes & index", s10))

# ══════════════════════════════════════════════════════════════════════
TOC = [("01","Modèle mental"),("02","Ta configuration"),("03","Agents & subagents"),
       ("04","Commandes"),("05","Skills"),("06","Permissions"),
       ("07","Le clavier"),("08","La CLI"),("09","Intégrations"),
       ("10","Recettes & index")]

cov = cover(
  "Carte de référence",
  "opencode",
  "Un agent de code entièrement local, qui délègue.<br>"
  "Agents, subagents, skills, permissions, clavier et CLI — au complet.",
  TOC,
  "youcef · ~/.config/opencode",
  os.environ.get("OC_VER", "opencode") + " · août 2026",
  stats=[("6", "agents"), ("9", "commandes"), ("7", "skills"), ("Ctrl+X", "le leader")])

d = os.path.dirname(__file__)
open(os.path.join(d, "opencode-cover.html"), "w", encoding="utf-8").write(page("opencode", cov))
body = "".join(S).replace('<section>', '<section class="cont">', 1)
open(os.path.join(d, "opencode-body.html"), "w", encoding="utf-8").write(
    page("opencode — carte de référence", body))
print("opencode: cover +", len(S), "sections,", len(body), "octets")
