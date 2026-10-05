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

;; ── Removed 2026-08-22 (declared but never used) ────────────────────
;; Checked by counting mentions in config.el before removing:
;;   general                 0  (Doom already ships it, `map!' is built on it)
;;   carbon-now-sh           0
;;   color-identifiers-mode  0
;;   kaolin-themes           0
;;   spacemacs-theme         0
;;   zoom                    0  (config.el already said "zoom removed",
;;                                only the line was left. `zoom-window'
;;                                is used, SPC w z, and stays.)
;;   dirvish                 0  (different reason, and my first take was
;;                                wrong: dirvish isn't dead. doom+'s
;;                                `:emacs dired' module already declares it,
;;                                pins it to a fork (latiagertrutis/dirvish,
;;                                commit 300b6b2) and calls
;;                                (dirvish-override-dired-mode) itself, see
;;                                the module's config.el:81. So the line here
;;                                was a DUPLICATE declaration with no :recipe
;;                                or :pin: redundant at best, at worst it
;;                                could swap the pinned fork for MELPA
;;                                upstream. Checked after: still on the
;;                                fork, dirvish works fine.)
;; The first six each cost a clone, a build on every `doom sync' and a
;; load-path entry. `doom purge' got back 98 MB (743 -> 645 MB).
;;
;; About `general': the line is gone but the PACKAGE stays installed, Doom
;; itself needs it for `map!'. The declaration was the useless part, not
;; the package.

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
(package! cape) ; CAPF completion sources (files, dabbrev, etc.)
;; evil mode packages
(package! evil-escape)
(package! just-mode)

;; ── evil-textobj-tree-sitter (nº 7) ─────────────────────────────────
;; The missing link between tree-sitter and Evil. combobulate does
;; structural navigation and editing, but gives no Evil text objects.
;; This package adds `af'/`if' (function), `ac'/`ic' (class),
;; `aa'/`ia' (argument)... using the official tree-sitter queries from
;; nvim-treesitter-textobjects.
;; Config: see config.el.
(package! evil-textobj-tree-sitter)

;; ── outli (nº 8) ────────────────────────────────────────────────────
;; Turns section comments (`;; ── title ──') into a foldable outline.
;; Written by jdtsmith, same author as eglot-booster and indent-bars,
;; both already used here.
(package! outli)

;; ── casual (nº 12) ──────────────────────────────────────────────────
;; Transient menus (like magit) for dired, isearch, ibuffer, calc,
;; re-builder, info, bookmarks. The point isn't speed, it's being able
;; to DISCOVER stuff: no need to open the docs, just press a key.
;; `casual' is the consolidated package (its :old-names list all the
;; old casual-dired / casual-isearch / casual-calc...).
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

;; llama: a dependency of `transient' (0.13.7+). straight downloads and
;; builds it because transient lists it in Package-Requires, but Doom only
;; puts packages declared with `package!' on the load-path. Its activation
;; loop calls `straight--get-dependencies' with a symbol while straight's
;; cache is keyed by string, so transitive deps never show up. Without an
;; explicit declaration, llama is on disk but invisible.
;;
;; Usually you don't notice: nothing loads transient during `doom sync'.
;; But combobulate-autoloads.el does an immediate `require' of combobulate-ui,
;; which requires transient, which requires llama -> "Cannot open load file:
;; llama" and the sync dies. doom+'s magit module already declares transient
;; and cond-let for the same reason; llama is missing there.
;;
;; No :pin on purpose: it follows transient, like it does today.
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
