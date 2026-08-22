# -*- coding: utf-8 -*-
"""
Systeme de mise en page pour les references PDF (rendu Chrome print).

DESIGN v2 — l'alignement est le sujet principal.

Le defaut de la v1 : `td.k { width: 1% }` laissait Chrome dimensionner la
colonne des touches d'apres son contenu. Chaque tableau avait donc une
largeur de colonne differente, et les descriptions ne demarraient jamais
au meme x d'un tableau a l'autre. Sur une page dense, l'oeil lit ca comme
un document mal fabrique.

v2 : `table-layout: fixed` + un <colgroup> explicite sur CHAQUE tableau.
Les largeurs viennent d'un jeu de gabarits partages (W_*), donc toutes les
colonnes de meme role sont alignees sur tout le document.

Grille : A4, marges 16/14mm -> 182mm de justification.
Typographie : SF Pro Text (corps) / SF Pro Display (titres) / JetBrains Mono.
"""
import html as _h

# ── gabarits de colonnes, en pourcentage ────────────────────────────
# Un seul endroit ou les largeurs sont decidees. Les documents s'y
# referent par nom : c'est ce qui garantit que tout est aligne.
W_KEY    = (22, 78)          # touche -> description
W_KEY_L  = (32, 68)          # touche longue (accords a 3 modificateurs)
W_CMD    = (28, 72)          # commande shell -> ce qu'elle fait
W_CMD_L  = (36, 64)          # commande longue
W_TRIO   = (24, 26, 50)      # touche -> commande -> role
W_TRIO_B = (22, 34, 44)
W_DEF    = (30, 70)          # terme -> definition
W_NUM    = (10, 32, 58)      # n° -> nom -> role
W_FOUR   = (18, 20, 24, 38)


def esc(s):
    return _h.escape(str(s), quote=False)


def kbd(s):
    """'prefix+alt+g' -> touches composees. ' · ' separe deux alternatives."""
    if s is None or s == "":
        return '<span class="dash">—</span>'
    parts = [p.strip() for p in str(s).split("·")]
    out = []
    for p in parts:
        out.append(" ".join(f"<kbd>{esc(k.strip())}</kbd>" for k in p.split(" ") if k.strip()))
    return '<span class="or">·</span>'.join(out)


def mono(s):
    return f"<code>{esc(s)}</code>"


CSS = r"""
/* ══════════════════════════════════════════════════════════════════
   GRILLE
   ══════════════════════════════════════════════════════════════════ */
@page { size: A4; margin: 16mm 14mm 16mm 14mm; }
/* PAS de `@page :first { margin: 0 }` ici.
   La couverture et le corps sont deux documents HTML SEPARES, rendus en
   deux PDF puis recolles. Une regle `:first` dans ce CSS partage frappe
   donc AUSSI la premiere page du corps — qui devient la page 2 du PDF
   final, et se retrouvait sans aucune marge laterale.
   La regle est injectee uniquement dans la couverture, par page(). */
*, *::before, *::after { box-sizing: border-box; }
html { -webkit-print-color-adjust: exact; print-color-adjust: exact; }

:root {
  /* encres */
  --ink:      #16191f;
  --ink-2:    #464e5c;
  --ink-3:    #7b8394;
  --ink-4:    #a8aeba;
  /* filets */
  --rule:     #d9dde4;
  --rule-2:   #edeff3;
  --rule-3:   #f5f6f9;
  /* fonds */
  --paper:    #ffffff;
  --panel:    #f7f8fa;
  --panel-2:  #fbfcfd;
  --graphite: #0e1014;
  /* accents */
  --amber:    #9d6a22;
  --amber-lt: #fdf6ea;
  --amber-br: #f0e2c9;
  --teal:     #2f6f73;
  --blue:     #40628c;
  --plum:     #6f5590;
  --moss:     #47703f;
  --brick:    #9a4749;
  /* typo */
  --sans: "SF Pro Text", "SF Pro", -apple-system, "Helvetica Neue", Helvetica, sans-serif;
  --disp: "SF Pro Display", "SF Pro", -apple-system, "Helvetica Neue", Helvetica, sans-serif;
  --code: "JetBrainsMono Nerd Font Mono", "JetBrains Mono", ui-monospace, Menlo, monospace;
}

body {
  margin: 0;
  font-family: var(--sans);
  font-size: 8.5pt;
  line-height: 1.5;
  color: var(--ink);
  background: var(--paper);
  -webkit-font-smoothing: antialiased;
  font-variant-numeric: tabular-nums;
}

/* ══════════════════════════════════════════════════════════════════
   COUVERTURE
   ══════════════════════════════════════════════════════════════════ */
.cover {
  background: var(--graphite);
  color: #ced4de;
  height: 297mm; width: 210mm;
  padding: 26mm 20mm 16mm;
  display: flex; flex-direction: column;
  page-break-after: always;
  position: relative;
}
.cover::after {          /* filet vertical d'accent, cale sur la marge */
  content: ""; position: absolute; left: 0; top: 0; bottom: 0;
  width: 3mm; background: linear-gradient(#e0b374, #9d6a22);
}
.cover .eyebrow {
  font-family: var(--code);
  font-size: 7.6pt; letter-spacing: .3em; text-transform: uppercase;
  color: #e0b374;
}
.cover h1 {
  font-family: var(--disp);
  font-size: 48pt; line-height: .98; margin: 8mm 0 0; font-weight: 600;
  letter-spacing: -.022em; color: #f2f4f8;
}
.cover h1 em {
  font-style: normal; font-weight: 200; color: #8d95a4; display: block;
  font-size: 26pt; letter-spacing: -.02em; margin-top: 2mm;
}
.cover .rule { height: 1.5px; background: #e0b374; width: 24mm; margin: 8mm 0 0; }
.cover .sub {
  font-size: 12pt; line-height: 1.5; color: #9aa2b1;
  margin: 7mm 0 0; max-width: 124mm; font-weight: 300;
}
.cover .spacer { flex: 1; }

/* statistiques : grille reelle, chiffres alignes en bas */
.cover .stats {
  display: grid; grid-auto-flow: column; grid-auto-columns: 1fr;
  gap: 6mm; margin: 0 0 10mm; padding-bottom: 8mm;
  border-bottom: 1px solid #22262f;
}
.cover .stats .fig {
  font-family: var(--disp);
  font-size: 21pt; font-weight: 250; color: #f2f4f8; letter-spacing: -.02em;
  line-height: 1; display: block;
}
.cover .stats .lab {
  font-family: var(--code);
  font-size: 6.2pt; letter-spacing: .16em; text-transform: uppercase;
  color: #666e7e; display: block; margin-top: 2mm;
}

/* sommaire : grille 2 colonnes, numeros alignes */
.cover .toc {
  display: grid; grid-template-columns: 1fr 1fr;
  grid-auto-flow: column;              /* 01..08 a gauche, 09..15 a droite */
  column-gap: 12mm; margin-bottom: 10mm;
}
.cover .toc .r {
  display: grid; grid-template-columns: 7mm 1fr;
  font-size: 8pt; color: #9aa2b1; padding: 1.35mm 0;
  border-bottom: 1px solid #1e222a; align-items: baseline;
}
.cover .toc .r b {
  font-family: var(--code); font-size: 7pt;
  color: #e0b374; font-weight: 500;
}
.cover .meta {
  font-family: var(--code);
  font-size: 7pt; color: #666e7e; border-top: 1px solid #22262f; padding-top: 4mm;
  display: flex; justify-content: space-between; letter-spacing: .04em;
}

/* ══════════════════════════════════════════════════════════════════
   TITRES  — un seul axe vertical, tout part de x=0
   ══════════════════════════════════════════════════════════════════ */
/* Les sections COULENT au lieu de commencer chacune sur une page neuve.
   Avec un saut force, une section qui deborde d'une ligne laissait une
   page presque blanche derriere elle — le defaut le plus visible d'un
   document de reference. Le titre reste solidaire de ce qui le suit
   grace aux `break-after: avoid` en cascade (h2 -> filet -> chapeau),
   donc un titre ne peut pas rester seul en bas de page.
   `section.newpage` force le saut quand on le veut vraiment. */
section { break-before: auto; margin-top: 9mm; }
section:first-of-type { margin-top: 0; }
section.newpage { break-before: page; margin-top: 0; }
section.cont { break-before: auto; }

h2 {
  font-family: var(--disp);
  font-size: 17pt; font-weight: 600; letter-spacing: -.025em;
  margin: 0 0 1.5mm; padding: 0; color: var(--graphite);
  display: grid; grid-template-columns: 11mm 1fr; align-items: baseline;
  break-after: avoid;
}
h2 .num {
  font-family: var(--code); font-size: 8.5pt; color: var(--amber);
  font-weight: 700; letter-spacing: .04em;
}
h2 + .lede { margin-left: 11mm; break-after: avoid; }
.h2rule {
  height: 1.5px; background: var(--graphite); margin: 0 0 4mm;
  break-after: avoid; break-before: avoid;
}

h3 {
  font-family: var(--disp);
  font-size: 10.5pt; font-weight: 600; margin: 6.5mm 0 2.4mm;
  letter-spacing: -.015em; color: var(--graphite); break-after: avoid;
  padding-bottom: 1.4mm; border-bottom: 1px solid var(--rule);
}
h4 {
  font-size: 7.4pt; margin: 4.5mm 0 1.8mm; color: var(--ink-3);
  text-transform: uppercase; letter-spacing: .13em; font-weight: 600;
  break-after: avoid;
}
h3 + h4 { margin-top: 2.5mm; }

p { margin: 0 0 2.6mm; }
p.lede {
  color: var(--ink-2); font-size: 9.2pt; line-height: 1.5;
  margin-bottom: 5mm; max-width: 158mm; font-weight: 300;
}
p:last-child { margin-bottom: 0; }
.dash { color: var(--ink-4); }
b, strong { font-weight: 600; color: var(--graphite); }
em { font-style: italic; color: var(--ink-2); }
a { color: var(--blue); text-decoration: none; }

/* ══════════════════════════════════════════════════════════════════
   TOUCHES ET CODE
   ══════════════════════════════════════════════════════════════════ */
kbd {
  font-family: var(--code);
  font-size: 6.9pt; line-height: 1;
  border: 1px solid var(--rule); border-bottom-width: 1.6px;
  border-radius: 2.5px; padding: 1.15mm 1.4mm .95mm;
  background: linear-gradient(#ffffff, #f4f5f8);
  color: var(--graphite); white-space: nowrap; font-weight: 500;
  display: inline-block; vertical-align: baseline;
}
.or { color: var(--ink-4); font-size: 7pt; padding: 0 1.1mm; }
code, .mono {
  font-family: var(--code);
  font-size: 7.4pt; background: var(--rule-3); padding: .35mm 1.05mm;
  border-radius: 2px; color: #2b313b; white-space: nowrap;
}
.card code, td code { background: #eef0f4; }

/* ══════════════════════════════════════════════════════════════════
   TABLEAUX  — largeurs FIXES, imposees par colgroup
   ══════════════════════════════════════════════════════════════════ */
table {
  width: 100%; border-collapse: collapse; margin: 0 0 3.8mm;
  table-layout: fixed;
}
th {
  text-align: left; font-size: 6.4pt; text-transform: uppercase;
  letter-spacing: .13em; color: var(--ink-3); font-weight: 600;
  padding: 0 2.5mm 1.5mm 0; border-bottom: 1.2px solid var(--graphite);
  vertical-align: bottom;
}
th:last-child, td:last-child { padding-right: 0; }
td {
  padding: 1.55mm 2.5mm 1.55mm 0; border-bottom: 1px solid var(--rule-2);
  vertical-align: top; overflow-wrap: break-word;
}
tr { break-inside: avoid; }
thead { display: table-header-group; }
tbody tr:last-child td { border-bottom: 1px solid var(--rule); }

/* roles de cellule */
td.k  { line-height: 1.75; }                       /* touches */
td.c  { font-family: var(--code); font-size: 7.2pt; color: #2b313b; }
td.d  { color: var(--ink-2); }
td.n  { color: var(--ink-3); font-size: 7.4pt; }
td.t  { font-weight: 600; color: var(--graphite); }
td.num{ font-family: var(--code); font-size: 7pt; color: var(--amber); font-weight: 700; }
table.zebra tbody tr:nth-child(even) { background: var(--rule-3); }
table.zebra td { padding-left: 1.6mm; }
/* tableau scinde en deux moities : un filet marque la separation */
table.split td:nth-child(3), table.split th:nth-child(3) {
  border-left: 1px solid var(--rule-2); padding-left: 4mm;
}
table.split th:nth-child(3) { border-left-color: var(--graphite); }
table.split td:empty { border-bottom-color: transparent; }
table.zebra td:first-child { padding-left: 1.6mm; }

/* ══════════════════════════════════════════════════════════════════
   GRILLES  — grid, pas column-count : les colonnes restent alignees
   ══════════════════════════════════════════════════════════════════ */
/* Mise en colonnes par TABLEAU, et non par `display:grid` ni par
   `column-count`. Les trois ont ete essayees ; seule celle-ci se
   fragmente correctement en impression :
     - grid          : une grille ne se coupe pas. Un couple de tableaux
                       qui ne tient pas dans la place restante saute
                       ENTIER a la page suivante -> demi-page blanche.
     - column-count  : Chrome coule bien d'une page a l'autre, mais la
                       reprise se fait en colonne 1 de la page suivante :
                       la colonne 2 de la page courante reste vide.
     - table         : Chrome coupe une ligne de tableau plus haute
                       qu'une page, et CHAQUE cellule reprend dans SA
                       colonne sur la page suivante. C'est le seul
                       comportement correct.
   `break-inside: auto` est indispensable ici : la regle globale
   `tr { break-inside: avoid }` empecherait justement cette coupure. */
table.lay {
  width: 100%; table-layout: fixed; border-collapse: separate;
  border-spacing: 0; margin: 0 0 1mm;
}
table.lay > tbody > tr { break-inside: auto; }
table.lay > tbody > tr > td {
  vertical-align: top; padding: 0; border: none; background: none;
}
table.lay.c2 > tbody > tr > td { padding-right: 8mm; }
table.lay.c3 > tbody > tr > td { padding-right: 6mm; }
table.lay > tbody > tr > td:last-child { padding-right: 0; }
table.lay h4:first-child { margin-top: 0; }
table.lay table { margin-bottom: 3mm; }

/* ══════════════════════════════════════════════════════════════════
   CARTES
   ══════════════════════════════════════════════════════════════════ */
.card {
  border: 1px solid var(--rule); border-radius: 3px;
  padding: 3mm 3.4mm; margin: 0 0 3.2mm;
  break-inside: avoid; background: var(--panel-2);
}
.card > .hd {
  display: grid; grid-template-columns: 1fr auto; gap: 3mm;
  align-items: baseline; margin: 0 0 1.4mm;
  padding-bottom: 1.4mm; border-bottom: 1px solid var(--rule-2);
}
.card > .hd .nm {
  font-family: var(--disp); font-size: 9.6pt; font-weight: 600;
  letter-spacing: -.015em; color: var(--graphite);
}
.card .role { color: var(--ink-2); margin: 0 0 1.8mm; font-size: 8pt; }
.card table { margin: 1.6mm 0 0; }
.card td { padding: .95mm 2mm .95mm 0; border-bottom: 1px solid var(--rule-2); }
.card table tbody tr:last-child td { border-bottom: none; }
.card:last-child { margin-bottom: 0; }
.tag {
  font-family: var(--code);
  font-size: 6.2pt; text-transform: uppercase; letter-spacing: .1em;
  color: var(--ink-3); border: 1px solid var(--rule); border-radius: 2px;
  padding: .55mm 1.4mm; white-space: nowrap; background: #fff;
}
.tag.on { color: var(--moss); border-color: #cfe0cb; background: #f5faf4; }
.tag.off{ color: var(--ink-4); }

/* ══════════════════════════════════════════════════════════════════
   ENCARTS
   ══════════════════════════════════════════════════════════════════ */
.note, .warn, .tip, .key {
  border-radius: 3px; padding: 2.7mm 3.2mm 2.7mm 3.4mm; margin: 0 0 3.6mm;
  font-size: 8.1pt; break-inside: avoid; border: 1px solid var(--rule);
  border-left-width: 2.5px;
}
.note { background: var(--panel);   border-left-color: var(--ink-3); }
.warn { background: #fdf4f4; border-color: #f0dedf; border-left-color: var(--brick); }
.tip  { background: var(--amber-lt); border-color: var(--amber-br); border-left-color: var(--amber); }
.key  { background: #f4f8f8; border-color: #d8e6e6; border-left-color: var(--teal); }
.note .lbl, .warn .lbl, .tip .lbl, .key .lbl {
  font-family: var(--code);
  font-size: 6.3pt; text-transform: uppercase; letter-spacing: .15em;
  display: block; margin-bottom: 1.3mm; font-weight: 700;
}
.note .lbl { color: var(--ink-3); }
.warn .lbl { color: var(--brick); }
.tip  .lbl { color: var(--amber); }
.key  .lbl { color: var(--teal); }
.note p:last-child, .warn p:last-child, .tip p:last-child, .key p:last-child { margin-bottom: 0; }

/* ══════════════════════════════════════════════════════════════════
   CODE EN BLOC
   ══════════════════════════════════════════════════════════════════ */
pre {
  font-family: var(--code);
  font-size: 7.1pt; line-height: 1.55; background: var(--panel);
  border: 1px solid var(--rule); border-radius: 3px;
  padding: 2.7mm 3mm; margin: 0 0 3.6mm; overflow: hidden;
  white-space: pre-wrap; break-inside: avoid; color: #2b313b;
  tab-size: 2;
}
pre.dark {
  background: var(--graphite); color: #c8cede; border-color: var(--graphite);
}
pre.dark .c { color: #6b7385; }
pre.dark .s { color: #9fbf8f; }
pre.dark .k { color: #c3a4de; }
pre.dark .a { color: #e0b374; }
pre .c { color: var(--ink-3); }                 /* commentaire */
pre .s { color: var(--moss); }                  /* chaine      */
pre .k { color: var(--plum); }                  /* mot-cle     */
pre .a { color: var(--amber); }                 /* accent      */
pre .p { color: var(--brick); }                 /* prompt      */

/* ══════════════════════════════════════════════════════════════════
   LISTES ET FLUX
   ══════════════════════════════════════════════════════════════════ */
ol.steps { margin: 0 0 3.6mm; padding: 0; list-style: none; counter-reset: s; }
ol.steps li {
  counter-increment: s; padding: 0 0 2.6mm 8mm; position: relative;
  break-inside: avoid;
}
ol.steps li::before {
  content: counter(s, decimal-leading-zero); position: absolute; left: 0; top: .1mm;
  font-family: var(--code); font-size: 6.6pt; font-weight: 700;
  color: var(--amber); letter-spacing: .04em;
}
ol.steps li::after {
  content: ""; position: absolute; left: 5.4mm; top: 1.4mm; bottom: 1mm;
  width: 1px; background: var(--rule);
}
ol.steps li:last-child::after { display: none; }

ul.plain { margin: 0 0 3.2mm; padding-left: 4.2mm; }
ul.plain li { margin-bottom: 1.3mm; }
ul.plain li::marker { color: var(--ink-4); }

/* chaine d'etapes horizontale */
.flow {
  display: flex; align-items: stretch; gap: 0; margin: 0 0 3.6mm;
  break-inside: avoid; border: 1px solid var(--rule); border-radius: 3px;
  overflow: hidden;
}
.flow > div {
  flex: 1; padding: 2.4mm 2.6mm; border-right: 1px solid var(--rule);
  background: var(--panel-2);
}
.flow > div:last-child { border-right: none; }
.flow .st {
  font-family: var(--code); font-size: 6.2pt; letter-spacing: .13em;
  text-transform: uppercase; color: var(--amber); display: block;
  margin-bottom: 1.2mm; font-weight: 700;
}
.flow .tx { font-size: 7.6pt; color: var(--ink-2); line-height: 1.42; }
.flow .tx b { display: block; color: var(--graphite); font-size: 8.2pt; margin-bottom: .6mm; }

/* ══════════════════════════════════════════════════════════════════
   INDEX FINAL
   ══════════════════════════════════════════════════════════════════ */
.idx { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 0 6mm; font-size: 7.4pt; }
.idx .grp { break-inside: avoid; margin-bottom: 3mm; }
.idx h4 { margin-top: 0; }
.idx .row {
  display: grid; grid-template-columns: 24mm 1fr; gap: 1.5mm;
  align-items: baseline; padding: .95mm 0; border-bottom: 1px solid var(--rule-2);
}
.idx .row .kk {
  font-family: var(--code); font-size: 6.6pt; color: var(--graphite);
  font-weight: 600; overflow-wrap: break-word;
}
.idx .row .vv { color: var(--ink-2); line-height: 1.35; }

/* index large : 2 colonnes, cle plus longue */
.idx.w2 { grid-template-columns: 1fr 1fr; gap: 0 9mm; }
.idx.w2 .row { grid-template-columns: 36mm 1fr; }

/* ══════════════════════════════════════════════════════════════════
   DIVERS
   ══════════════════════════════════════════════════════════════════ */
.spacer-s { height: 2mm; }
.hr { height: 1px; background: var(--rule); margin: 5mm 0; }
.small { font-size: 7.6pt; color: var(--ink-2); }
.center { text-align: center; }
.right { text-align: right; }
.nowrap { white-space: nowrap; }
"""


def page(title, body, full_bleed=False):
    """full_bleed : supprime les marges de la premiere page.
    Reserve a la couverture, qui occupe la feuille entiere. Le corps ne
    doit JAMAIS l'utiliser (voir le commentaire sur @page dans le CSS)."""
    extra = "@page :first { margin: 0; }" if full_bleed else ""
    return (
        "<!doctype html><html lang=fr><head><meta charset=utf-8>"
        f"<title>{esc(title)}</title><style>{CSS}{extra}</style></head>"
        f"<body>{body}</body></html>"
    )


def cover(eyebrow, title, sub, toc, meta_left, meta_right, stats=None, subtitle=None):
    """title accepte du HTML ; subtitle sort en <em> sous le titre."""
    st = f"<em>{subtitle}</em>" if subtitle else ""
    rows = (len(toc) + 1) // 2   # colonne gauche pleine d'abord
    items = "".join(f'<div class="r"><b>{esc(n)}</b><span>{esc(t)}</span></div>' for n, t in toc)
    stat_html = ""
    if stats:
        cells = "".join(
            f'<div><span class="fig">{esc(a)}</span><span class="lab">{esc(b)}</span></div>'
            for a, b in stats)
        stat_html = f'<div class="stats">{cells}</div>'
    return (
        f'<div class="cover"><div class="eyebrow">{esc(eyebrow)}</div>'
        f'<h1>{title}{st}</h1><div class="rule"></div>'
        f'<div class="sub">{sub}</div><div class="spacer"></div>{stat_html}'
        f'<div class="toc" style="grid-template-rows:repeat({rows},auto)">{items}</div>'
        f'<div class="meta"><span>{esc(meta_left)}</span><span>{esc(meta_right)}</span></div></div>'
    )


def section(num, title, body, lede=None, cont=False, newpage=False):
    cls = ' class="newpage"' if newpage else (' class="cont"' if cont else "")
    ld = f'<p class="lede">{lede}</p>' if lede else ""
    return (f'<section{cls}><h2><span class="num">{esc(num)}</span>'
            f'<span>{esc(title)}</span></h2><div class="h2rule"></div>{ld}{body}</section>')


def table(headers, rows, widths=None, cls="", classes=None):
    """rows : liste de tuples de HTML deja rendu.
    widths : liste de pourcentages -> <colgroup> (largeurs FIXES).
    classes : classe CSS par colonne."""
    n = len(rows[0]) if rows else (len(headers) if headers else 1)
    classes = classes or [""] * n
    cg = ""
    if widths:
        cg = "<colgroup>" + "".join(f'<col style="width:{w}%">' for w in widths) + "</colgroup>"
    th = ""
    if headers and any(headers):
        th = "<thead><tr>" + "".join(f"<th>{esc(h)}</th>" for h in headers) + "</tr></thead>"
    body = []
    for r in rows:
        tds = "".join(f'<td class="{classes[i] if i < len(classes) else ""}">{c}</td>'
                      for i, c in enumerate(r))
        body.append(f"<tr>{tds}</tr>")
    return f'<table class="{cls}">{cg}{th}<tbody>{"".join(body)}</tbody></table>'


def keys(rows, widths=W_KEY, headers=("touche", "action")):
    """Tableau touche -> description. `rows` = [(combo, desc), ...]"""
    return table(headers, [(kbd(k), d) for k, d in rows],
                 widths=widths, classes=["k", "d"])


def cmds(rows, widths=W_CMD, headers=("commande", "effet")):
    return table(headers, [(mono(c), d) for c, d in rows],
                 widths=widths, classes=["c", "d"])


def keys2(rows, headers=None, widths=(17, 33, 17, 33)):
    """Liste longue de touches en DEUX colonnes, mais dans UN SEUL tableau.

    La premiere moitie va dans les colonnes 1-2, la seconde dans les
    colonnes 3-4 : la lecture reste verticale (1..n/2 a gauche), et
    comme chaque ligne porte les deux cotes, la coupure entre deux pages
    se fait ligne par ligne. Aucune place perdue, contrairement a deux
    tableaux poses cote a cote.

    `headers` : ("Groupe gauche", "", "Groupe droit", "") ou None.
    """
    half = (len(rows) + 1) // 2
    left, right = rows[:half], rows[half:]
    right += [("", "")] * (len(left) - len(right))
    body = [(kbd(a) if a else "", b, kbd(c) if c else "", d)
            for (a, b), (c, d) in zip(left, right)]
    return table(headers, body, widths=widths,
                 classes=["k", "d", "k", "d"], cls="split")


def keys_pair(title_a, rows_a, title_b, rows_b, widths=(17, 33, 17, 33)):
    """Deux groupes NOMMES de touches, cote a cote, dans un seul tableau.
    Se coupe ligne par ligne entre deux pages (cf. keys2)."""
    n = max(len(rows_a), len(rows_b))
    a = list(rows_a) + [("", "")] * (n - len(rows_a))
    b = list(rows_b) + [("", "")] * (n - len(rows_b))
    body = [(kbd(x) if x else "", y, kbd(z) if z else "", w)
            for (x, y), (z, w) in zip(a, b)]
    return table((title_a, "", title_b, ""), body, widths=widths,
                 classes=["k", "d", "k", "d"], cls="split")


def cmd_pair(title_a, rows_a, title_b, rows_b, widths=(22, 28, 22, 28)):
    n = max(len(rows_a), len(rows_b))
    a = list(rows_a) + [("", "")] * (n - len(rows_a))
    b = list(rows_b) + [("", "")] * (n - len(rows_b))
    body = [(mono(x) if x else "", y, mono(z) if z else "", w)
            for (x, y), (z, w) in zip(a, b)]
    return table((title_a, "", title_b, ""), body, widths=widths,
                 classes=["c", "d", "c", "d"], cls="split")


def mixed_pair(title_a, rows_a, title_b, rows_b, widths=(17, 33, 22, 28)):
    """Gauche = touches, droite = commandes shell."""
    n = max(len(rows_a), len(rows_b))
    a = list(rows_a) + [("", "")] * (n - len(rows_a))
    b = list(rows_b) + [("", "")] * (n - len(rows_b))
    body = [(kbd(x) if x else "", y, mono(z) if z else "", w)
            for (x, y), (z, w) in zip(a, b)]
    return table((title_a, "", title_b, ""), body, widths=widths,
                 classes=["k", "d", "c", "d"], cls="split")


def cmds2(rows, headers=None, widths=(24, 26, 24, 26)):
    """Idem keys2, pour des commandes."""
    half = (len(rows) + 1) // 2
    left, right = rows[:half], rows[half:]
    right += [("", "")] * (len(left) - len(right))
    body = [(mono(a) if a else "", b, mono(c) if c else "", d)
            for (a, b), (c, d) in zip(left, right)]
    return table(headers, body, widths=widths,
                 classes=["c", "d", "c", "d"], cls="split")


def card(name, key=None, role="", rows=None, tags=None, widths=W_KEY, headers=None):
    t = ""
    if rows:
        t = table(headers or ("", ""), [(kbd(k) if k else "", d) for k, d in rows],
                  widths=widths, classes=["k", "d"])
    right = ""
    if key:
        right = kbd(key)
    elif tags:
        right = " ".join(f'<span class="tag">{esc(x)}</span>' for x in tags)
    return (
        f'<div class="card"><div class="hd"><span class="nm">{esc(name)}</span>'
        f'<span>{right}</span></div>'
        f'<div class="role">{role}</div>{t}</div>'
    )


def note(kind, label, text):
    return f'<div class="{kind}"><span class="lbl">{esc(label)}</span>{text}</div>'


def steps(items):
    return "<ol class=steps>" + "".join(f"<li>{i}</li>" for i in items) + "</ol>"


def flow(items):
    """items = [(etape, titre, texte), ...] — chaine horizontale."""
    cells = "".join(
        f'<div><span class="st">{esc(s)}</span>'
        f'<div class="tx"><b>{esc(t)}</b>{x}</div></div>'
        for s, t, x in items)
    return f'<div class="flow">{cells}</div>'


def grid(items, cols=2):
    """Colonnes cote a cote qui se coupent proprement entre deux pages.
    Voir le commentaire `table.lay` dans le CSS pour le pourquoi."""
    w = round(100 / cols, 4)
    cg = "<colgroup>" + f'<col style="width:{w}%">' * cols + "</colgroup>"
    tds = "".join(f"<td>{it}</td>" for it in items)
    return f'<table class="lay c{cols}">{cg}<tbody><tr>{tds}</tr></tbody></table>'


def idx(groups, wide=False):
    out = []
    for title, rows in groups:
        body = "".join(
            f'<div class="row"><span class="kk">{esc(k)}</span>'
            f'<span class="vv">{esc(v)}</span></div>' for k, v in rows)
        out.append(f'<div class="grp"><h4>{esc(title)}</h4>{body}</div>')
    cls = "idx w2" if wide else "idx"
    return f'<div class="{cls}">{"".join(out)}</div>'


def pre(text, dark=False):
    return f'<pre class="{"dark" if dark else ""}">{text}</pre>'
