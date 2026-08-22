;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

;; ── Retires le 2026-08-22 (declarations sans aucun usage) ───────────
;; Verifie par comptage des mentions dans config.el avant suppression :
;;   general                 0  — Doom l'embarque deja, `map!' est bati dessus
;;   carbon-now-sh           0
;;   color-identifiers-mode  0
;;   kaolin-themes           0
;;   spacemacs-theme         0
;;   zoom                    0  — le commentaire de config.el disait deja
;;                                « zoom removed », seule la ligne restait.
;;                                `zoom-window' (utilise, SPC w z) est conserve.
;;   dirvish                 0  — MAIS pour une autre raison que les autres,
;;                                et ma premiere analyse etait fausse : dirvish
;;                                n'est pas mort. Le module `:emacs dired' de
;;                                doom+ le declare deja, l'epingle sur un fork
;;                                (latiagertrutis/dirvish, commit 300b6b2) et
;;                                appelle lui-meme (dirvish-override-dired-mode)
;;                                — config.el:81 du module. La ligne ici etait
;;                                donc une declaration EN DOUBLE, sans :recipe
;;                                ni :pin : au mieux redondante, au pire de quoi
;;                                degrader le fork epingle vers l'upstream MELPA.
;;                                Verifie apres coup : le depot est bien reste
;;                                sur le fork, dirvish fonctionne normalement.
;; Les six premieres coutaient un clone, une compilation par `doom sync' et une
;; entree de load-path. `doom purge' a recupere 98 Mo (743 -> 645 Mo).
;;
;; NB `general' : la ligne est retiree, mais le PAQUET reste installe — Doom
;; en depend lui-meme pour `map!'. C'etait la declaration qui etait superflue,
;; pas le paquet.

;; python related packages
(package! numpydoc)
;; navigation related packages
(package! zoom-window)
(package! discover-my-major)
;; (package! all-the-icons-ivy-rich)
;; (package! ivy-rich)
(package! iedit)
(package! comment-dwim-2)
;; (package! blamer)
;; (package! beacon)
;; org related packages
;; (unpin! org-roam)
;; (package! org-fragtog)
;; (package! org-ql)
;; (package! org-fancy-priorities)
;; (package! org-edna)
;; (package! org-modern)
(package! focus)
(package! info-colors)
;; smooth-scrolling removed: redundant with (pixel-scroll-precision-mode 1)
;; already enabled in config.el, and the two fight each other on Emacs 29+.
;; (package! smooth-scrolling)
;; (package! ialign)
(package! puni)
(package! kind-icon)
;; (package! multi-vterm)
;; (package! fountain-mode)
;; (package! olivetti)
;; visualization related packages
;; (package! svg-tag-mode)
;; coding
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! cape) ; sources de completion CAPF (files, dabbrev, etc.)
;; evil mode packages
(package! evil-escape)
(package! just-mode)

;; ── evil-textobj-tree-sitter (nº 7) ─────────────────────────────────
;; Le chainon manquant entre tree-sitter et Evil. combobulate fait de la
;; NAVIGATION et de la MANIPULATION structurelle ; il ne fournit pas
;; d'objets textuels Evil. Ce paquet ajoute `af'/`if' (fonction),
;; `ac'/`ic' (classe), `aa'/`ia' (argument)... en s'appuyant sur les
;; requetes tree-sitter officielles de nvim-treesitter-textobjects.
;; Config : voir config.el.
(package! evil-textobj-tree-sitter)

;; ── outli (nº 8) ────────────────────────────────────────────────────
;; Transforme les commentaires de section (`;; ── titre ──') en structure
;; outline repliable. Ecrit par jdtsmith — le meme auteur qu'eglot-booster
;; et indent-bars, tous deux deja utilises ici.
(package! outli)

;; ── casual (nº 12) ──────────────────────────────────────────────────
;; Menus transient (comme magit) pour dired, isearch, ibuffer, calc,
;; re-builder, info, bookmarks. L'interet n'est pas la vitesse mais la
;; DECOUVRABILITE : on n'ouvre pas la doc, on appuie sur une touche.
;; `casual' est le paquet consolide (ses :old-names listent tous les
;; anciens casual-dired / casual-isearch / casual-calc...).
(package! casual)

;; ── LeetCode ────────────────────────────────────────────────────────
(package! leetcode) ;; add leetcode challenges for practice 

;; ══════════════════════════════════════════════════════════════════════
;;  Modern IDE — added 2026-08-21
;; ══════════════════════════════════════════════════════════════════════

;; vundo — the undo tree, on demand, on top of Emacs' native undo.
;; Replaces undo-tree (dropped from init.el via `undo' without +tree):
;; undo-tree keeps a tree in memory permanently and crawls on large
;; buffers; vundo only DRAWS the history when you open it.
(package! vundo)

;; indent-bars — indentation guides drawn with a stipple (a font texture)
;; rather than with overlays, unlike highlight-indent-guides. That is what
;; keeps it usable on large files.
(package! indent-bars)

;; dape — native DAP debugger, with no dependency on lsp-mode. This is why
;; :tools debugger stays disabled: that module pulls in dap-mode AND
;; realgud for the same service.
(package! dape)

;; eglot-booster — wires the emacs-lsp-booster binary between eglot and the
;; server: JSON arrives already converted to elisp bytecode, so Emacs never
;; parses it. Without the binary on PATH the package simply does nothing.
(package! eglot-booster
  :recipe (:host github :repo "jdtsmith/eglot-booster"))

;; llama — dependance de `transient' (0.13.7+). straight la telecharge et la
;; compile parce que transient la declare dans Package-Requires, mais Doom
;; n'ajoute au load-path que les paquets declares par `package!' : sa boucle
;; d'activation appelle `straight--get-dependencies' avec un symbole alors que
;; le cache de straight est indexe par chaine, donc elle ne remonte jamais les
;; dependances transitives. Sans declaration explicite, llama existe sur disque
;; mais reste invisible.
;;
;; D'habitude ca ne se voit pas : rien ne charge transient pendant `doom sync'.
;; Mais combobulate-autoloads.el fait un `require' immediat de combobulate-ui,
;; qui require transient, qui require llama -> "Cannot open load file: llama"
;; et le sync s'arrete. Le module magit de doom+ declare deja transient et
;; cond-let pour la meme raison ; llama y manque.
;;
;; Pas de :pin volontairement : elle suit transient, comme aujourd'hui.
(package! llama)

;; combobulate — structured navigation and editing driven by the tree-sitter
;; parse tree. Not on MELPA; it lives on GitHub only.
;;
;; Why it matters here specifically: `puni' (already installed) reasons about
;; delimiters. In Python there are none — blocks are indentation — so puni has
;; almost nothing to grab. combobulate reads the real syntax tree, so
;; "next sibling", "parent", "drag this block down" work in Python, YAML and
;; JSON exactly as they do in a lisp.
(package! combobulate
  :recipe (:host github :repo "mickeynp/combobulate"))
