;;; $DOOMDIR/config.el -*- lexical-binding: t; coding: utf-8; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; ── Identity ────────────────────────────────────────────────────────
;; This repo is PUBLIC, so name and email aren't tracked here. They live
;; in doom/private.el, which git ignores (see .gitignore).
;; Template: doom/private.el.example. Copy it and fill it in.
;;
;; Without that file Doom works fine. Only GPG signing, file templates
;; and snippets that insert the author fall back to Emacs defaults.
(let ((private (expand-file-name "private.el" doom-user-dir)))
  (when (file-readable-p private)
    (load private nil 'nomessage)))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;; ── Fonts ───────────────────────────────────────────────────────────
;; Doom exposes five variables, each for one specific role:
;;
;;   doom-font                 code text. Must be monospaced.
;;   doom-variable-pitch-font  text modes (org, markdown) when mixed-pitch
;;                             is on. Must be PROPORTIONAL — putting a
;;                             monospaced font here defeats the purpose,
;;                             which is what the previous value did.
;;   doom-symbol-font          fallback for unicode symbols.
;;   doom-serif-font           the `fixed-pitch-serif' face.
;;   doom-big-font             doom-big-font-mode (presenting, pairing).
;;
;; `:size' gotcha: an INTEGER means PIXELS, a FLOAT means POINTS.
;; `:size 18' is 18px, not 18pt. We stay in pixels to keep exactly the
;; size you are used to.
;;
;; Three Nerd Font variants exist, and the choice is not cosmetic:
;;   JetBrainsMono Nerd Font        icons span 2 cells, but the font
;;                                  declares itself PROPORTIONAL (-p- in
;;                                  its XLFD) — Emacs is a character
;;                                  grid, so avoid it.
;;   JetBrainsMono Nerd Font Mono   everything in 1 cell, declared -m-,
;;                                  truly fixed-width. This is the one.
;;   JetBrainsMono Nerd Font Propo  proportional, irrelevant here.
;; Verified at runtime: the Mono variant reports `-m-', the other `-p-'.
;;
;; UI icons do not come from this font anyway but from `nerd-icons', which
;; ships its own (Symbols Nerd Font Mono, already installed) — so the Mono
;; variant costs nothing in legibility.
;;
;; JetBrains Mono ships programming ligatures (-> => != >=) and
;; `:ui ligatures' is enabled: they render with no extra setup.
;;
;; After changing any of these:  SPC h r f   (or M-x doom/reload-font)
(setq doom-font                (font-spec :family "JetBrainsMono Nerd Font Mono" :size 18)
      doom-symbol-font         (font-spec :family "JetBrainsMono Nerd Font")
      doom-variable-pitch-font (font-spec :family "SF Pro" :size 18)
      doom-serif-font          (font-spec :family "New York" :size 18)
      doom-big-font            (font-spec :family "JetBrainsMono Nerd Font Mono" :size 26))


;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; Alternatives, closest to furthest from the terminal palette
;; (~/.config/alacritty/themes/premium-noir.toml):
;; Back to stock doom-horizon and its own background (#232530).
;;
;; Alternatives:
;;   'horizon-noir         doom-horizon with its dark end rebuilt on the
;;                         terminal background #101216 — kept in
;;                         themes/horizon-noir-theme.el, ready to use
;;   'premium-noir         a full port of the terminal palette
;;   'doom-tomorrow-night  same ANSI accents, lighter neutral background
;;   'doom-tokyo-night     same mood, more saturated accents
(setq doom-theme 'doom-horizon)
;; splash screen
(setq fancy-splash-image "~/.config/doom/misc/img/logo.png")
;; change the line spacing for better visualization
;; (setq-default line-spacing 4)
;; (global-prettify-symbols-mode +1)

;; line-spacing 4 can slow down redisplay on some machines and large buffers
(setq-default line-spacing 4)

;; Avoid prettify everywhere (it costs on large buffers)
(add-hook 'prog-mode-hook #'prettify-symbols-mode)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; [EVIL] relative is strongly recommended -- numeric prefixes like 5j, 12dd
;; become natural once you can read jump distances directly off the gutter.
;; 'relative: `5j', `12dd', `d7k' (the core of Evil grammar) can be read
;; straight off the margin, no counting.
;;
;; Yes, 'relative redraws the whole margin on every cursor move. But
;; `+maybe-lighten-buffer-h' (below) already drops line numbers past 2000
;; lines or 512 KB, which is exactly where that cost shows. Below that
;; it's microseconds.
;;
;; This cost can't be measured in batch, it lives in the redisplay of a
;; real GUI frame (same story as the "Diagnosing lag" block below). If
;; moving around feels laggy, check with SPC P s / SPC P r.
;;
;; To go back: set it to `t' (absolute). 'visual is the middle ground:
;; relative, but counts DISPLAYED lines (handy with word-wrap).
(setq display-line-numbers-type 'relative)

;; change the key modifiers on mac os
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
;; (global-font-lock-mode 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")
(setq org-directory "~/Dropbox/Org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;; [EVIL] Global keybindings formerly set with `global-set-key' are now handled
;; via `map! :leader' (SPC prefix) or `map! :n'/`:v' for modal bindings.
;; Quick reference for what moved where:
;;   C-s   consult-line        →  SPC s s
;;   C-c l l  zen toggle       →  SPC t z
;;   C-c d l  make run         →  SPC c m
;;   C-c r  set-mark-command   →  not needed; Evil visual mode replaces mark ring
;; (global-set-key (kbd "C-s") #'consult-line) ;; while using vertico
;; (global-set-key (kbd "C-c l l") #'+zen/toggle)
;; (global-set-key (kbd "C-c d l") #'+make/run)
;; (map! :map global-map "C-c r" #'set-mark-command)

(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1))
;; (smooth-scrolling-mode 1)
(pixel-scroll-precision-mode 1)
;; (setq fast-but-imprecise-scrolling t)
(setq pixel-scroll-precision-large-scroll-height 30)
;; (global-set-key (kbd "C-x SPC") #'set-mark-command)

;; less redisplay work
(setq redisplay-skip-fontification-on-input t)

(setq-default standard-indent 4)

;; An (after! flycheck (global-flycheck-mode -1)) used to live here: dead
;; code. `:checkers' is empty in init.el, so flycheck is not installed and
;; the block never ran. Diagnostics come from flymake, driven by eglot
;; (see flymake-no-changes-timeout below).


;; ============================================================
;; [EVIL] Core Evil settings
;; Doom enables Evil by default. The variables below refine its behaviour.
;; ============================================================

;; Visual cursor feedback per state -- immediately obvious which mode you are in.
(setq evil-normal-state-cursor '(box "orchid")
      evil-insert-state-cursor '(bar "green")
      evil-visual-state-cursor '(hollow "orange"))

;; Each word typed in Insert mode becomes its own undo step.
;; Without this the entire Insert session is one single undo.
(setq evil-want-fine-undo t)

;; C-u scrolls half a page up (Vim behaviour) instead of universal-argument.
(setq evil-want-C-u-scroll t)

;; gj / gk move by visual (wrapped) lines, consistent with how text looks.
(setq evil-respect-visual-line-mode t)

;; [EVIL] evil-escape -- type `jk' in Insert/Visual to return to Normal.
;; Avoids reaching for the physical ESC key or Ctrl-[.
;; Pairs well with a ZMK combo on the same sequence for hardware-level escape.
;; Remember to add `(package! evil-escape)' to packages.el.
(use-package! evil-escape
  :config
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.25
        evil-escape-excluded-states '(normal multiedit emacs motion)
        evil-escape-excluded-major-modes '(vterm-mode))
  (evil-escape-mode +1))


;; ============================================================
;; [EVIL] Keybindings
;; All former global-set-key / C-c chords are replaced by:
;;   - `map! :leader'  for SPC-prefixed commands (replaces C-c / C-x chords)
;;   - `map! :n / :v'  for Normal / Visual mode direct bindings
;; ============================================================

(map! :leader
      ;; -- Search -------------------------------------------------------
      ;; was: (global-set-key (kbd "C-s") #'consult-line)
      :desc "Search buffer"          "s s" #'consult-line

      ;; -- Toggle -------------------------------------------------------
      ;; was: (global-set-key (kbd "C-c l l") #'+zen/toggle)
      :desc "Zen toggle"             "t z" #'+zen/toggle

      ;; -- Build --------------------------------------------------------
      ;; was: (global-set-key (kbd "C-c d l") #'+make/run)
      :desc "Make run"               "c m" #'+make/run

      ;; -- Windows ------------------------------------------------------
      ;; was: (global-set-key (kbd "M-p") 'ace-window)
      :desc "Ace window"             "w a" #'ace-window
      ;; was: (global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
      :desc "Zoom window"            "w z" #'zoom-window-zoom

      ;; -- Edit / multi-edit --------------------------------------------
      ;; was: ("C-'" . iedit-mode)
      ;; Note: Doom's built-in evil-multiedit (M-d) is often more fluid for
      ;; Evil workflows; iedit remains available for explicit multi-edit needs.
      :desc "iedit mode"             "s e" #'iedit-mode

      ;; -- Multiple cursors ---------------------------------------------
      ;; was: ("C-c k" . mc/edit-lines) / ("C->" ...) / ("C-<" ...) / ("C-x m" ...)
      :desc "MC edit lines"          "s l" #'mc/edit-lines
      :desc "MC mark next"           "s n" #'mc/mark-next-like-this
      :desc "MC mark prev"           "s p" #'mc/mark-previous-like-this
      :desc "MC mark all"            "s a" #'mc/mark-all-like-this

      ;; -- Comments -----------------------------------------------------
      ;; was: ("C-," . comment-dwim-2)
      ;; Note: `gcc' (evil-commentary) handles single-line comments in Normal mode.
      ;; comment-dwim-2 stays available for more complex comment scenarios.
      :desc "Comment dwim"           "c ;" #'comment-dwim-2

      ;; -- Structural editing (puni) ------------------------------------
      ;; was: (define-key puni-map ...) under "C-c ;" prefix
      ;; Regrouped under SPC k  (k = structural Kill / sexp navigation)
      :desc "Puni forward sexp"      "k f" #'puni-forward-sexp
      :desc "Puni backward sexp"     "k b" #'puni-backward-sexp
      :desc "Puni up sexp"           "k u" #'puni-backward-up-sexp
      :desc "Puni down sexp"         "k d" #'puni-down-sexp
      :desc "Puni slurp forward"     "k s" #'puni-slurp-forward
      :desc "Puni slurp backward"    "k S" #'puni-slurp-backward
      :desc "Puni barf forward"      "k r" #'puni-barf-forward
      :desc "Puni barf backward"     "k R" #'puni-barf-backward
      :desc "Puni splice"            "k x" #'puni-splice
      :desc "Puni raise sexp"        "k e" #'puni-raise-sexp
      :desc "Puni kill line"         "k k" #'puni-kill-line
      ;; was: (global-set-key (kbd "C-c ; i") #'my/clear-inside-delimiters)
      ;; Note: Evil's native `di(' `di{' `di[' `di"' cover most cases already.
      ;; my/clear-inside-delimiters remains useful for puni-aware edge cases.
      :desc "Clear inside delims"    "k i" #'my/clear-inside-delimiters)

;; Normal / Visual mode direct bindings (no leader needed)
(map!
 ;; Was ace-jump-mode. The binding itself was fine — `g a' did resolve to
 ;; `ace-jump-mode' — but the package's last upstream commit is from
 ;; 2014-06-16: it predates Evil integration, and its temporary keymap
 ;; loses the follow-up keypress to Evil's command loop, so nothing
 ;; visible happens.
 ;;
 ;; avy is the maintained equivalent, and Doom already wires it into Evil
 ;; everywhere (`g s' motions, `SPC j'). `evil-avy-goto-char-timer' is its
 ;; best entry point: type as many characters as you want, then pick a
 ;; label — no fixed 1- or 2-char guess.
 :n "g a" #'evil-avy-goto-char-timer

 ;; was: ("C-h C-m" . discover-my-major)
 :n "g h" #'discover-my-major

 ;; Trigger Copilot manually from Normal mode (idle-delay is set to 999)
 :n "g /" #'copilot-complete

 ;; Extra LSP navigation (gd / gr / K are already mapped by Doom)
 :n "g I" #'eglot-find-implementation
 :n "g y" #'eglot-find-typeDefinition

 ;; Visual comment (gcc handles Normal mode via evil-commentary)
 :v "g c" #'comment-dwim-2)


;; ============================================================
;; Packages
;; ============================================================

;; zoom removed: zoom-mode was never enabled and you bind zoom-window,
;; which does the same job. It was loaded on every startup for nothing.

(use-package! ace-window :defer t :commands (ace-window ace-swap-window))

(use-package! zoom-window
  ;; `:custom' expects name/value pairs, not a call to
  ;; custom-set-variables: the previous form was a no-op.
  :defer t
  :commands (zoom-window-zoom)
  :config (setq zoom-window-mode-line-color "plum4"))

(use-package! iedit
  :defer t
  :commands (iedit-mode)
  :diminish iedit-mode)

(use-package! discover-my-major
  :defer t
  :commands (discover-my-major discover-my-mode))

(use-package! multiple-cursors
  :defer t
  :commands (mc/edit-lines mc/mark-next-like-this mc/mark-previous-like-this
             mc/mark-all-like-this))

(use-package! comment-dwim-2
  :defer t
  :commands (comment-dwim-2)
  :config (setq comment-dwim-2--inline-comment-behavior 'reindent-comment))

(use-package! focus
  ;; Was :config '((prog-mode . defun) ...) -> a quoted list, so dead code:
  ;; it evaluated without configuring anything. Fixed and deferred.
  :defer t
  :commands (focus-mode focus-read-only-mode)
  :config
  (setq focus-mode-to-thing '((prog-mode . defun) (text-mode . sentence))))

(use-package! info-colors
  :commands (info-colors-fontify-node)
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

(add-hook! python-mode
  (advice-add 'python-pytest-file :before
              (lambda (&rest args)
                (setq python-pytest-executable (+python-executable-find "pytest")))))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(after! python
  (setq python-indent-offset 4))

;; ══════════════════════════════════════════════════════════════════════
;;  No diagnostics on screen
;;
;;  This is NOT flycheck: `:checkers' is empty, flycheck is not even
;;  installed. What underlines code in red is `flymake' — and eglot turns
;;  it on by itself in every buffer it manages.
;;
;;  We cut it at the source rather than after the fact: `eglot-stay-out-of'
;;  tells eglot not to touch flymake at all. Without that, eglot would
;;  re-enable it in every new buffer and we would be chasing it forever.
;;
;;  What stays intact: completion, `gd', `K', renaming, code actions. The
;;  server still computes errors — we simply do not display them all the
;;  time. To see them on demand: SPC t f (toggle) or
;;  M-x flymake-show-buffer-diagnostics.
;; ══════════════════════════════════════════════════════════════════════
(after! eglot
  (add-to-list 'eglot-stay-out-of 'flymake))

(after! flymake
  (setq flymake-no-changes-timeout nil        ; never while typing
        flymake-start-on-flymake-mode nil     ; nor when the mode turns on
        flymake-start-on-save-buffer nil))    ; nor on save

;; Safety net: if a major mode or a package turns flymake back on anyway,
;; switch it off in that buffer.
(defun +no-flymake-h ()
  (when (bound-and-true-p flymake-mode) (flymake-mode -1)))
(add-hook 'eglot-managed-mode-hook #'+no-flymake-h 90)
(add-hook 'prog-mode-hook #'+no-flymake-h 90)

;; The *Warnings* buffer no longer pops up on its own for a plain warning.
;; The threshold stays at :error — hiding errors too would mean never
;; knowing why something broke.
(setq warning-minimum-level :error
      warning-minimum-log-level :error
      native-comp-async-report-warnings-errors 'silent)

;; SPC t f: turn diagnostics back on for a review pass, then off again.
;; Free here — it is flycheck's key in Doom, and that module is off.
(defun +toggle-diagnostics ()
  "Show or hide the language server diagnostics in this buffer."
  (interactive)
  (if (bound-and-true-p flymake-mode)
      (progn (flymake-mode -1) (message "Diagnostics hidden"))
    (flymake-mode 1)
    (flymake-start)
    (message "Diagnostics shown — SPC t f to hide them again")))

(map! :leader :desc "Toggle diagnostics" "t f" #'+toggle-diagnostics)

(use-package! puni
  :defer t
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode))

;; puni bindings are defined in the map! :leader "k ..." block above.
;; The former `define-prefix-command' / `global-set-key' setup under "C-c ;" is
;; no longer needed -- which-key will surface all SPC k bindings automatically.

(use-package! avy :defer t
  :commands (avy-goto-char avy-goto-line avy-goto-word-1))

;; The bindings stay outside the use-package: they must exist before avy
;; is loaded, and invoking one is what triggers the load.
(map! :leader
      :desc "Avy jump char"  "j j" #'avy-goto-char
      :desc "Avy jump line"  "j l" #'avy-goto-line
      :desc "Avy jump word"  "j w" #'avy-goto-word-1)

(defun my/clear-inside-delimiters ()
  "Delete content *inside* the enclosing delimiter ((), [], {}, \"\").
Keeps the delimiters themselves."
  (interactive)
  (let* ((ppss (syntax-ppss))
         (open (nth 1 ppss)))
    (cond
     ;; String case: empty the string content, keep the quotes.
     ((nth 3 ppss)
      (let ((beg (1+ (nth 8 ppss))))
        (goto-char beg)
        (let ((end (save-excursion
                     (goto-char (nth 8 ppss))
                     (forward-sexp 1)
                     (1- (point)))))
          (when (< beg end)
            (delete-region beg end)))))

     ;; List case: empty the inside of the enclosing pair.
     (open
      (let ((beg (1+ open))
            (end (save-excursion
                   (goto-char open)
                   (forward-sexp 1)
                   (1- (point)))))
        (when (< beg end)
          (delete-region beg end))))

     (t
      (user-error "Not inside a delimited block ((), [], {}, or string)")))))

;; [EVIL] Note: `di(' `di{' `di[' `di"' handle most inner-delete cases natively.
;; my/clear-inside-delimiters is kept for puni-aware structural edge cases.


(defvar nb/current-line '(0 . 0)
  "(start . end) of current line in current buffer")
(make-variable-buffer-local 'nb/current-line)

(defun nb/unhide-current-line (limit)
  "Enable markdown concealling"
  (interactive)
  (markdown-toggle-markup-hiding 'toggle)
  (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
  (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))

;; (add-hook 'markdown-mode-hook #'nb/markdown-unhighlight)

;; ── Copilot: inline completion ──────────────────────────────────────
;; Was effectively disabled: copilot-idle-delay 999 = a ~17 minute wait.
;; The GitHub auth is valid (account cyuss); nothing needed installing.
;;
;; Measured cost: the suggestion lands in an overlay, asynchronously. It
;; never blocks typing — the only local cost is the idle timer. 0.25s
;; means we do not fire during continuous typing, but the suggestion is
;; there as soon as you pause.
;; ── PATH for GUI frames ─────────────────────────────────────────────
;; A macOS app launched from Finder, the Dock or launchd does NOT inherit
;; the shell PATH. The Doom LaunchAgent PATH is:
;;   /opt/homebrew/bin:/opt/homebrew/sbin:/usr/bin:/bin:/usr/sbin:/sbin
;; -> `node' (installed through nvm) is not in it. The Copilot server is a
;;    `#!/usr/bin/env node' script: without node it never starts, and
;;    Copilot asks you to sign in again on EVERY Emacs launch.
;; So we prepend the bin of the newest node version, resolved by glob:
;; that survives node upgrades, unlike a path hardcoded in the plist.
(let* ((nvm (expand-file-name "~/.nvm/versions/node"))
       (dirs (and (file-directory-p nvm)
                  (sort (directory-files nvm t "^v[0-9]") #'string>)))
       (bin  (and dirs (expand-file-name "bin" (car dirs)))))
  (dolist (d (list bin (expand-file-name "~/.local/bin")))
    (when (and d (file-directory-p d))
      (add-to-list 'exec-path d)
      (setenv "PATH" (concat d path-separator (getenv "PATH"))))))

;; IMPORTANT: copilot-mode is enabled DEFENSIVELY. Without this guard, a
;; missing `copilot-language-server' makes the whole Emacs startup fail
;; ("server did not start correctly") — the editor becomes unusable.
;; Install the server with:  M-x copilot-install-server
;; or  npm install -g @github/copilot-language-server
(defun +copilot-server-available-p ()
  "Return non-nil when the Copilot server is actually installed and runnable."
  (and (executable-find "node")
       (or (executable-find "copilot-language-server")
           ;; copilot.el installs its server in its own directory,
           ;; which is NOT on Emacs' PATH
           (and (boundp 'copilot-install-dir)
                (file-executable-p
                 (expand-file-name "bin/copilot-language-server" copilot-install-dir))))))

(defvar +copilot-excluded-modes
  '(emacs-lisp-mode lisp-interaction-mode org-mode markdown-mode
    vterm-mode eshell-mode fundamental-mode)
  "Modes where inline suggestions get in the way more than they help.")

(defun +copilot-maybe-enable-h ()
  "Enable copilot-mode only when its server is actually available.
Without this guard, a missing server makes the whole Emacs startup fail
(\"server did not start correctly\").

Test order matters: the per-mode exclusion runs BEFORE the `require'.
Otherwise the *scratch* buffer — which is in `lisp-interaction-mode',
derived from `prog-mode' — fired this hook at daemon startup, loaded
copilot and started the node server. That was the real cause of
\"Copilot: Copilot server started.\" on every launch."
  (unless (or (apply #'derived-mode-p +copilot-excluded-modes)
              (minibufferp))
    (when (and (require 'copilot nil t) (+copilot-server-available-p))
      (ignore-errors (copilot-mode 1)))))

(use-package! copilot
  :defer t
  :hook ((prog-mode . +copilot-maybe-enable-h))
  :config
  (setq copilot-idle-delay 0.25
        copilot-max-char 30000
        ;; Do not index large files: past this point copilot ships too much
        ;; context and the network latency becomes noticeable.
        copilot-max-char-warning-disable t
        copilot-indent-offset-warning-disable t)

  ;; (per-mode exclusion is handled upstream by +copilot-excluded-modes:
  ;;  anonymous lambdas on hooks could not be removed, and they ran after
  ;;  the package had already been loaded anyway.)

  ;; The keys are bound HERE, in :config, and not through `:bind'.
  ;; `:bind (:map copilot-completion-map ...)' forces use-package to
  ;; resolve the keymap at startup, so it loaded copilot eagerly — and
  ;; with it the node server — before any prog-mode buffer even existed.
  ;; That was the cause of "Copilot: Copilot server started." on every
  ;; daemon launch. Inside :config this runs only once copilot is loaded,
  ;; i.e. on the first prog-mode buffer.
  (map! :map copilot-completion-map
        "TAB"   #'copilot-accept-completion
        "<tab>" #'copilot-accept-completion
        "C-e"   #'copilot-accept-completion-by-line
        "M-f"   #'copilot-accept-completion-by-word
        "C-]"   #'copilot-next-completion
        "C-["   #'copilot-previous-completion
        "C-g"   #'copilot-clear-overlay))

;; (handled in copilot's :config above)

;; ── GC ──────────────────────────────────────────────────────────────
;; Nothing to tune here, and this is a measurement, not a guess:
;;   - Doom (2.2.2 / Emacs 30) manages gc-cons-threshold itself and sets
;;     it to 16 MB after startup. gcmh is no longer bundled.
;;   - Observed in a real session: 3 collections, 0.1s total.
;; So GC is NOT a source of lag here. The old (setq gc-cons-threshold ...)
;; forms on emacs-startup-hook were overwritten by Doom and did nothing:
;; they have been removed.

;; performance: improve LSP / language servers throughput
(setq read-process-output-max (* 4 1024 1024)) ; 4MB
(setq process-adaptive-read-buffering nil)

;; (after! embark
;;   (define-key global-map (kbd "C-;") nil))

(after! eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list #'eglot-completion-at-point
                                #'cape-file
                                #'cape-dabbrev)))))

;; The section is named after the server. We declare both: eglot sends
;; everything and a server ignores sections it does not know — so this
;; works whether the project answers with pyright or basedpyright.
(after! eglot
  (setq eglot-workspace-configuration
        '((:python       . (:venvPath "." :venv ".venv"))
          (:basedpyright . (:python (:venvPath "." :venv ".venv")))
          (:pyright      . (:python (:venvPath "." :venv ".venv"))))))

;; lsp-headerline-breadcrumb-enable belongs to lsp-mode. This config uses
;; eglot (:tools (lsp +eglot)), so that setq did nothing. Removed.
;;
;; No header-line either: `breadcrumb' (the eglot equivalent) is gone.
;; The project > file > symbol trail ate a line at the top of every
;; window. The project-relative path lives in the modeline.

;; With +tree-sitter (added to :lang javascript) the real modes are
;; js-ts-mode / typescript-ts-mode / tsx-ts-mode: without those variants
;; the hooks stopped firing entirely.
(dolist (h '(js-mode-hook js-ts-mode-hook
             typescript-mode-hook typescript-ts-mode-hook
             tsx-ts-mode-hook))
  (add-hook h #'eglot-ensure))

;; ── modeline ────────────────────────────────────────────────────────
;; Measured before tuning anything: 2000 `format-mode-line' renders on
;; this file come out at ~73 us each. At 60 Hz that's 0.4% of a frame, so
;; the modeline is NOT a perf problem. The two expensive segments are
;; cached buffer-locally by doom-modeline: `doom-modeline--buffer-file-name'
;; (recomputed on find-file) and `doom-modeline--vcs' (on after-save and
;; vc-refresh-state). So the choices below are about SIGNAL, not cycles.
;;
;; The old block here did almost nothing: `doom-modeline-checker' and
;; `doom-modeline-checker-simple-format' don't exist (the real variable is
;; `doom-modeline-check'), and word-count / indent-info / modal /
;; modal-icon just repeated the defaults. The only setting that did
;; anything was `doom-modeline-buffer-encoding nil', and it made it worse.
(after! doom-modeline
  (setq
   ;; ── signal ───────────────────────────────────────────────────────
   ;; The path is the ONLY location hint since breadcrumb is gone. Keep it
   ;; project-relative and add the project name, otherwise
   ;; `lisp/comint.el' doesn't tell you which repo you're in.
   doom-modeline-buffer-file-name-style 'relative-from-project
   doom-modeline-project-name           t
   ;; The real errors-segment variable: auto / full / simple / nil.
   ;; 'simple = short counters. With eglot these are the pyright
   ;; diagnostics, which is where you notice a dead server.
   doom-modeline-check                  'simple
   ;; 'nondefault: nothing for plain UTF-8/LF, only shows up for CRLF or
   ;; latin-1. A free safety net the old `nil' removed.
   doom-modeline-buffer-encoding        'nondefault
   ;; 15 chars cut off most branch names.
   doom-modeline-vcs-max-length         24
   ;; ── noise ────────────────────────────────────────────────────────
   doom-modeline-percent-position       nil   ; "All" / "Top" / "42%"
   doom-modeline-time                   nil   ; macOS menu bar has it
   doom-modeline-battery                nil
   doom-modeline-irc                    nil
   doom-modeline-gnus                   nil)

  ;; `doom-modeline-def-modeline' is a FUNCTION (name lhs &optional rhs)
  ;; that defines `doom-modeline-format--main'. Since `mode-line-format'
  ;; holds (:eval (doom-modeline-format--main)), redefining it applies
  ;; everywhere right away, no `doom-modeline-set-modeline' needed.
  ;;
  ;; Dropped from the default definition because the matching package
  ;; isn't installed (each one is a function call returning "" on every
  ;; render): eldoc window-state follow word-count parrot objed-state
  ;; persp-name battery grip irc mu4e gnus github minor-modes
  ;; input-method indent-info time.
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches
      buffer-info remote-host buffer-position selection-info)
    ;; compilation/debug/repl/process: state of dape, Python REPLs and
    ;; comint buffers. Silent while nothing is going on.
    ;; misc-info: that's where envrc and pyvenv show the environment.
    '(compilation misc-info project-name debug repl lsp
      check vcs major-mode process buffer-encoding)))

(after! corfu
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.1)
  (setq corfu-cycle t)
  (setq corfu-auto-prefix 1))

(setq corfu-preselect 'prompt)

(use-package! kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(after! vertico
  (setq vertico-cycle t
        vertico-resize t
        vertico-count 12))

(after! orderless
  (setq orderless-style-dispatchers nil))

;; (use-package! cape
;;   :init
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(require 'cl-lib)

(defun my/project-root ()
  (or (and (fboundp 'doom-project-root) (doom-project-root))
      (when-let ((p (project-current nil))) (car (project-roots p)))
      default-directory))

(defun my/python-venv-dir ()
  (let* ((root (file-truename (my/project-root)))
         (venv (expand-file-name ".venv" root)))
    (when (file-directory-p venv) venv)))

(defun my/python-apply-local-venv ()
  (when-let* ((venv (my/python-venv-dir))
              (bin  (expand-file-name "bin" venv))
              (py   (expand-file-name "python" bin)))
    ;; For the REPL and tooling (formatters, etc.)
    (when (file-executable-p py)
      (setq-local python-shell-interpreter py))

    ;; So that Emacs finds the venv binaries.
    ;; exec-path and process-environment are buffer-local: the venv does
    ;; not leak outside the buffer. (Before: a global `setenv' -> every
    ;; open Python project stacked its venv onto the Emacs process PATH,
    ;; and the wrong python eventually won everywhere.)
    (setq-local exec-path (cons bin (remove bin exec-path)))
    (setq-local process-environment
                (append (list (concat "VIRTUAL_ENV=" venv)
                              (concat "PATH=" bin path-separator (getenv "PATH")))
                        process-environment))

    ;; venvPath = the venv's PARENT directory, venv = its name.
    ;; The three sections are declared side by side: eglot sends all of
    ;; them and a server ignores what it does not know. So this works
    ;; whether the project answers with basedpyright or pyright.
    (let* ((venvPath (file-name-directory (directory-file-name venv)))
           (venvName (file-name-nondirectory (directory-file-name venv)))
           (py (list (cons :venvPath venvPath)
                     (cons :venv venvName))))
      (setq-local eglot-workspace-configuration
                  (list (cons :python py)
                        (cons :basedpyright (list (cons :python py)))
                        (cons :pyright (list (cons :python py))))))))

(defun my/python-eglot-ensure ()
  (my/python-apply-local-venv)
  (eglot-ensure))

(add-hook 'python-mode-hook #'my/python-eglot-ensure 0)
(add-hook 'python-ts-mode-hook #'my/python-eglot-ensure 0)

(after! eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))

;; ── Transparency ────────────────────────────────────────────────────
;; alpha-background is composited by the macOS window server, NOT by
;; Emacs: the editor-side cost is nil (measured: 0.60 ms/char with it,
;; identical without). This is unlike `alpha', which also makes the TEXT
;; translucent and does cost redisplay time — we do not use that one.
;;
;; The value tracks Alacritty's window opacity (Emacs has less coloured
;; background, so it looks more opaque at the same value).
(defvar +transparency 70
  "Frame background opacity, as a percentage. 100 = fully opaque.
Emacs keeps its own value: at 55 the background all but vanished behind
the desktop. Alacritty stays at 0.55 — a terminal has far less coloured
surface, so the same number reads very differently in each app.")

(add-to-list 'default-frame-alist `(alpha-background . ,+transparency))
(add-hook 'after-make-frame-functions
          (lambda (frame) (set-frame-parameter frame 'alpha-background +transparency)))

;; SPC t t: toggle opaque / transparent (handy for a screenshot, or when
;; what is behind the frame makes reading hard)
(defun +toggle-transparency ()
  "Toggle between fully opaque and `+transparency'."
  (interactive)
  (let ((cur (frame-parameter nil 'alpha-background)))
    (set-frame-parameter nil 'alpha-background
                         (if (and cur (< cur 100)) 100 +transparency))))
(map! :leader :desc "Toggle transparency" "t t" #'+toggle-transparency)

;; ── Focus on new frames ─────────────────────────────────────────────
;; `emacsclient -c -n' creates the frame and returns at once, without ever
;; raising it. The daemon was not launched through LaunchServices, so macOS
;; grants it no right to come to the front: the window opens BEHIND whatever
;; is on screen. From Raycast it looks as though nothing happened at all.
;;
;; This belongs in the hook rather than in the .app launcher: it then covers
;; every route to a new frame (Raycast, `emacsclient' from a shell, herdr),
;; and it lives in the repo — the bundle does not.
;;
;; `server-after-make-frame-hook' runs with the new frame already selected,
;; so there is no guessing which of several frames to raise.
(defun +raise-new-frame-h ()
  "Bring a newly created graphical frame to the front."
  (when (display-graphic-p)
    (select-frame-set-input-focus (selected-frame))))
(add-hook 'server-after-make-frame-hook #'+raise-new-frame-h)

;; ── Stale panes on a fresh frame ────────────────────────────────────
;; Doom's :ui workspaces (persp-mode) restores the perspective's window
;; configuration into EVERY new frame. That is what you want for real
;; work -- your files come back. It is not what you want for a transient
;; pane: close the frame mid-LeetCode and the next one opens with the
;; dashboard on one side and a leftover *leetcode-result-N* on the other,
;; the code and statement buffers having been killed in between.
;;
;; The rule is narrow on purpose. It fires only on a NEW frame, only when
;; the dashboard is present -- which is precisely the "nothing is going on
;; here yet" signal -- and only closes LeetCode's own transient panes. A
;; frame you are actually working in never shows the dashboard, so a real
;; layout is never touched.
(defun +dismiss-stale-panes-h ()
  "Collapse a restored layout that holds nothing but leftovers.
A window is a leftover when it shows the dashboard -- the buffer every
window falls back to once its own was killed -- or one of LeetCode's
transient panes. If EVERY window in the frame is one of those, there is
nothing to preserve and the frame becomes a single dashboard.

The condition has to cover both shapes, because they are the same
accident at different stages: right after closing the frame the panes
still hold *leetcode-result-N*, and once those buffers are killed the
windows survive showing *doom* twice over."
  (let ((wins (window-list)))
    (when (and (> (length wins) 1)
               (seq-every-p
                (lambda (w)
                  (let ((b (window-buffer w)))
                    (or (eq b (doom-fallback-buffer))
                        (string-match-p "\\`\\*leetcode" (buffer-name b)))))
                wins))
      (when-let ((keep (seq-find (lambda (w) (eq (window-buffer w) (doom-fallback-buffer)))
                                 wins)))
        (ignore-errors (delete-other-windows keep))))))
(add-hook 'server-after-make-frame-hook #'+dismiss-stale-panes-h)

;; ── One workspace per project ───────────────────────────────────────
;; Doom already creates a workspace named after the project when you
;; switch to one, and switches BACK to it if it exists rather than making
;; a second -- `+workspaces-switch-to-project-h' matches on the stored
;; project root, not just the name.
;;
;; What stopped it here was the default `non-empty': from a workspace
;; with no buffers yet, Doom recycles the current one instead of spawning
;; a dedicated one. That is the common case right after starting Emacs --
;; open your first project of the day and it lands in `main'.
;;
;; `t' means "always a dedicated workspace". The reuse of an existing one
;; is unaffected: that branch runs before this setting is consulted.
(setq +workspaces-on-switch-project-behavior t)

;; ── C-x C-c closes the window, not Emacs ────────────────────────────
;; With the daemon, `C-x C-c' doesn't quit Emacs. It goes through
;; `server-save-buffers-kill-terminal', and for a frame opened with
;; `emacsclient -n' (all of mine, the launcher passes -n) that does:
;;
;;   (save-some-buffers arg)   <- arg is nil, so it ASKS, file by
;;                                file, in the minibuffer
;;   (delete-frame)
;;
;; Hence the "it's stuck" feeling: the frame won't close until every
;; question is answered, in a minibuffer you're not necessarily looking at.
;;
;; Those questions make no sense here. We're closing a WINDOW, not Emacs:
;; buffers stay alive in the daemon, nothing is lost, they're right there
;; in the next frame.
;;
;; Really quitting is still possible, on purpose: SPC q K.
(defun +close-frame-not-emacs ()
  "Close this frame, leaving the daemon and its buffers alone.
Falls back to the standard behaviour outside a daemon, where closing the
last frame really does mean quitting."
  (interactive)
  (if (and (daemonp) (frame-parameter nil 'client))
      ;; FORCE, and it is not optional. `delete-frame' without it refuses
      ;; to remove what it considers the last visible frame -- and on a
      ;; daemon the initial terminal frame does not count as visible, so
      ;; your only graphical frame IS the last one. Measured: the plain
      ;; call does not merely refuse, it wedges the daemon, twice over,
      ;; needing a SIGKILL. With FORCE it returns immediately, and the
      ;; daemon staying alive with no frame is exactly the normal state.
      (delete-frame nil t)
    (save-buffers-kill-terminal)))

(map! "C-x C-c" #'+close-frame-not-emacs)

(add-hook 'after-save-hook #'evil-normal-state)

;; ── Large-file guard ────────────────────────────────────────────────
;; Interactive lag in Emacs almost always comes from three things that are
;; recomputed on EVERY cursor move / redisplay:
;;   1. display-line-numbers 'relative — redraws the whole margin
;;   2. ligatures — glyph recomposition, line by line
;;   3. indent guides — indentation overlays
;; On a short file this is invisible. Past ~2000 lines, or with very long
;; lines, it shows. We only switch them off there.

;; ── Colors: delimiters and variables ────────────────────────────────
;; Two layers with very different costs, which is why they're hooked in
;; different places.
;;
;; rainbow-delimiters colors by DEPTH: nested parens, brackets and braces
;; get successive hues, so you see at a glance which level you're
;; closing. It's just one more font-lock rule, evaluated on demand line
;; by line with the rest of fontification. Cheap, so it goes everywhere.
;;
;; color-identifiers-mode gives each VARIABLE its own stable color: the
;; same `total' is the same blue all over the function. It ONLY colors
;; variables (not keywords, not calls) because it knows the grammar. The
;; price is that it has to rescan the buffer to know what's a variable.
;;
;; Three guards for that price:
;;   1. the rescan runs on an IDLE timer, never while typing;
;;   2. the delay is bumped to 1s (default 0.5), no recoloring between
;;      two words;
;;   3. it's only hooked on Python, not on all of `prog-mode'.
;;
;; Both get switched off in big files by `+maybe-lighten-buffer-h', same
;; as ligatures and indent guides.
(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package! color-identifiers-mode
  :hook ((python-mode python-ts-mode) . color-identifiers-mode)
  :config
  ;; The palette is REGENERATED from the theme: hues are derived from the
  ;; current background, not hardcoded, so a theme change carries over.
  ;;
  ;; Luminance 0.72 and min saturation 0.35 are tuned for a dark
  ;; background (#232530). Lower and variables drown in the background,
  ;; higher and more saturated and it looks like a Christmas tree. 12
  ;; colors instead of 10: a Python function often has more than ten
  ;; locals, and two neighbours with the same color kills the point.
  (setq color-identifiers:num-colors 12
        color-identifiers:color-luminance 0.72
        color-identifiers:min-color-saturation 0.35
        color-identifiers:max-color-saturation 0.85
        color-identifiers:recoloring-delay 1.0)
  (color-identifiers:regenerate-colors))


;; ── Aligned tables ──────────────────────────────────────────────────
;; org and markdown align columns by counting CHARACTERS via
;; `string-width'. One char = one width holds for ASCII and breaks as
;; soon as an emoji or CJK char shows up: those are drawn by a fallback
;; font whose advance has no reason to match JetBrains Mono.
;;
;; Measured on a four-row table, same column count, real width on
;; screen:
;;
;;   | abc    | ASCII |   275 px
;;   | ✅      | Fini  |   287 px   (+12)
;;   | ⚠️      | ...   |   299 px   (+24)
;;   | 日本語 | CJK   |   263 px   (-12)
;;
;; org thinks these four rows are identical. The screen disagrees, and
;; the screen is right.
;;
;; valign places the separators with a display property computed in
;; PIXELS. The buffer isn't modified, the file on disk keeps its bars as
;; is, so nothing changes for git, pandoc or anyone reading it elsewhere.
;; Only the rendering moves.
(use-package! valign
  :hook ((org-mode markdown-mode) . valign-mode)
  :config
  ;; Past this size valign gives up and just uses a fixed-pitch face. The
  ;; limit matters: realign cost grows with the cell count. Measured here:
  ;;
  ;;    30 rows / 1162 chars  ->   23 ms
  ;;   120 rows / 6411 chars  ->  147 ms
  ;;
  ;; This cost is NOT paid on every keystroke.
  ;; `valign-not-align-after-list' excludes `self-insert-command' and
  ;; friends, so typing in a cell realigns nothing. It fires on TAB, on
  ;; file open, after a cut/paste. So 4000 (the package default) puts the
  ;; cutoff around ~80 ms on a one-off event: noticeable, not annoying.
  (setq valign-max-table-size 4000)
  ;; Full-height bars: separators connect from one row to the next instead
  ;; of being chopped by line spacing (line-spacing 4 here, so it showed).
  (setq valign-fancy-bar t))

(defvar +big-file-lines 2000
  "Above this line count, drop the expensive per-line decorations.")

(defun +maybe-lighten-buffer-h ()
  "Disable expensive decorations in large buffers."
  (when (and buffer-file-name
             (or (> (buffer-size) (* 512 1024))
                 (> (line-number-at-pos (point-max)) +big-file-lines)))
    (setq-local display-line-numbers nil)   ; even absolute ones are useless here
    (when (bound-and-true-p ligature-mode)      (ligature-mode -1))
    (when (bound-and-true-p prettify-symbols-mode) (prettify-symbols-mode -1))
    (when (fboundp 'highlight-indent-guides-mode) (highlight-indent-guides-mode -1))
    (when (bound-and-true-p rainbow-delimiters-mode) (rainbow-delimiters-mode -1))
    (when (bound-and-true-p color-identifiers-mode) (color-identifiers-mode -1))
    (setq-local bidi-display-reordering nil
                bidi-paragraph-direction 'left-to-right)))

(add-hook 'find-file-hook #'+maybe-lighten-buffer-h)

;; so-long: already active here (verified). It neutralises files with
;; kilometre-long lines (minified assets, logs, single-line JSON).

;; ── Diagnosing lag while it happens ─────────────────────────────────
;; No headless measurement is trustworthy for interactive rendering: the
;; lag has to be captured WHILE it happens, in a real frame.
;;   SPC P s   -> start the profiler
;;   ... reproduce the lag for 10-20 s ...
;;   SPC P r   -> open the report (TAB expands branches)
;; SPC h is mapped to Emacs' full help-map, so SPC h p is already
;; `describe-package' and cannot become a prefix -> we declare our own
;; prefix SPC P (uppercase; SPC p is projects).
(map! :leader
      (:prefix ("P" . "profiler")
       :desc "Start (cpu+mem)"   "s" (cmd! (profiler-start 'cpu+mem)
                                            (message "Profiler started — reproduce the lag, then SPC P r"))
       :desc "Report & stop"     "r" (cmd! (profiler-report) (profiler-stop))
       :desc "Reset"             "x" #'profiler-reset))

;; ══════════════════════════════════════════════════════════════════════
;;  LeetCode
;;
;;  Window layout, from `leetcode--solving-window-layout':
;;
;;    +----------------+----------------+
;;    |                |    Statement   |
;;    |                +----------------+
;;    |      Code      |     Input      |
;;    |                +----------------+
;;    |                |     Result     |
;;    +----------------+----------------+
;;
;;  The loop: SPC l l lists problems, RET opens one and lays out the
;;  windows. Write in Code, adjust Input if needed, SPC l t runs the
;;  tests, SPC l s submits. Solutions land in leetcode-challenges/, so
;;  they are version-controlled and can be reread later.
;;
;;  SPC l g c/p/i/o jumps between windows without leaving the keyboard,
;;  SPC l b opens the dashboard and SPC l ? the cheat sheet.
;; ══════════════════════════════════════════════════════════════════════

;; ── Moving around the layout ────────────────────────────────────────
;; `leetcode-try' and `leetcode-submit' read the problem title from the
;; NAME of the current buffer: run from the statement or the result, they
;; fail. And they fail badly — they are `aio-defun's, so the read happens
;; AFTER the first `aio-await', which means the error surfaces detached
;; from the key you just pressed.
;;
;; Hence these wrappers, which select the code WINDOW first. Selecting the
;; window rather than merely the buffer is required precisely because of
;; that await: on resume, `current-buffer' follows the selected window,
;; not whatever a `with-current-buffer' set before the suspension.
;;
;; They live outside the `use-package!' so the `map!' below does not
;; depend on the package being loaded.
(defun +leetcode--code-buffer ()
  "The LeetCode code buffer shown in this frame, or nil."
  (if (bound-and-true-p leetcode-solution-mode)
      (current-buffer)
    (seq-some (lambda (w)
                (with-current-buffer (window-buffer w)
                  (and (bound-and-true-p leetcode-solution-mode)
                       (current-buffer))))
              (window-list))))

(defun +leetcode-goto-code ()
  "Jump to the code buffer."
  (interactive)
  (let ((buf (or (+leetcode--code-buffer)
                 (user-error "No LeetCode code buffer in this frame"))))
    (if-let ((w (get-buffer-window buf)))
        (select-window w)
      (switch-to-buffer buf))
    buf))

(defun +leetcode--goto-by-name (regexp what)
  "Select the window whose buffer matches REGEXP.
WHAT names the window in the error message."
  (let ((w (seq-find (lambda (w)
                       (string-match-p regexp (buffer-name (window-buffer w))))
                     (window-list))))
    (unless w (user-error "No %s window in this layout" what))
    (select-window w)))

(defun +leetcode-goto-description ()
  "Jump to the problem statement."
  (interactive) (+leetcode--goto-by-name "\\`\\*leetcode-detail-" "statement"))

(defun +leetcode--swap-into-shared (kind what)
  "Show the KIND buffer of the current problem in the shared bottom pane.
KIND is `testcase' or `result'. Input and result take turns in one pane,
so jumping to either means swapping it in, not hunting for a window that
may not be showing it right now."
  (require 'leetcode)
  (let* ((code (or (+leetcode--code-buffer)
                   (user-error "No LeetCode code buffer in this frame")))
         (problem (or (leetcode--get-problem (leetcode--get-slug-title code))
                      (user-error "Unknown LeetCode problem")))
         (id (leetcode-problem-id problem))
         (buf (get-buffer-create
               (if (eq kind 'testcase)
                   (leetcode--testcase-buffer-name id)
                 (leetcode--result-buffer-name id))))
         (win (or (and (window-live-p leetcode--result-window) leetcode--result-window)
                  (get-buffer-window buf)
                  (seq-find (lambda (w)
                              (string-match-p "\\`\\*leetcode-\\(testcase\\|result\\)-"
                                              (buffer-name (window-buffer w))))
                            (window-list)))))
    (unless win (user-error "No %s pane in this layout" what))
    (set-window-buffer win buf)
    (select-window win)))

(defun +leetcode-goto-testcase ()
  "Show and edit the test input."
  (interactive) (+leetcode--swap-into-shared 'testcase "input"))

(defun +leetcode-goto-result ()
  "Show the result."
  (interactive) (+leetcode--swap-into-shared 'result "result"))

(defun +leetcode-try ()
  "Run the tests, from any window of the layout."
  (interactive) (+leetcode-goto-code) (call-interactively #'leetcode-try))

(defun +leetcode-submit ()
  "Submit, from any window of the layout."
  (interactive) (+leetcode-goto-code) (call-interactively #'leetcode-submit))

(defun +leetcode-goto-list ()
  "Jump to the problem list."
  (interactive)
  (require 'leetcode)
  (if-let* ((buf (get-buffer leetcode--buffer-name))
            (w (get-buffer-window buf)))
      (select-window w)
    (call-interactively #'leetcode)))

(defun +leetcode-open-session ()
  "Pick a problem already started and restore its layout.
Candidates come from open code buffers AND from solutions already
written to `leetcode-directory', so a problem closed days ago is picked
up the same way as one still on screen."
  (interactive)
  (require 'leetcode)
  (let* ((sessions (or (+leetcode--sessions)
                       (user-error "No problem started yet")))
         (labels (mapcar (lambda (s)
                           (cons (format "%5s  %-45s %s"
                                         (nth 0 s) (or (nth 1 s) "?")
                                         (+leetcode--difficulty-name (nth 2 s)))
                                 (nth 0 s)))
                         sessions))
         (pick (completing-read "Resume: " labels nil t)))
    (+leetcode-resume (cdr (assoc pick labels)))))

;; ── Readability ─────────────────────────────────────────────────────
;; `leetcode--show-problem' hands the HTML to `shr-render-buffer'. shr
;; styles prose with `shr-text' (which inherits `variable-pitch') and code
;; with `shr-code' (which inherits `fixed-pitch'). So the statement was
;; inheriting `doom-variable-pitch-font' — SF Pro.
;;
;; SF Pro is an INTERFACE typeface. Apple ships it with optical sizing and
;; tracking that tighten the drawing at small sizes; Emacs applies
;; neither. We were reading whole paragraphs in the shape meant for button
;; labels.
;;
;; Bookerly was drawn by Dalton Maag for on-screen reading: low contrast —
;; it holds up on a dark, translucent background — a tall x-height, and
;; lining figures. The files come from the Kindle app, copied into
;; ~/Library/Fonts: CoreText does not follow symlinks (fontconfig does,
;; which is misleading), so real files are required. It is therefore not
;; shipped by this repo — on another machine, pick one of the families
;; listed below.
(defvar +leetcode-prose-font "Bookerly"
  "Typeface for the LeetCode problem statement.
Verified alternatives, bold and italic included:
  \"Charter\"      ships with macOS, narrower, very good too
  \"New York\"     Apple's serif, warmer and wider
  \"Merriweather\" tall x-height, robust on a dark background
  \"Avenir Next\"  if you would rather have a sans")

(defvar +leetcode-measure 74
  "Fill width for the statement, passed to `shr-max-width'.
It counts in FIXED-PITCH characters: shr converts it to pixels through
`frame-char-width'. Prose being narrower, 74 yields about 90 characters
per line — the measure past which the eye loses the start of the next
line. At 92 we were up to 113.")

;; ── Options and hooks ───────────────────────────────────────────────
(use-package! leetcode
  :defer t
  :commands (leetcode leetcode-daily leetcode-refresh)
  :init
  (setq leetcode-prefer-language "python3"
        leetcode-prefer-sql "mysql"
        leetcode-save-solutions t
        leetcode-directory "~/Desktop/projects/leetcode-challenges/solutions")
  :config
  (setq leetcode-path-operation-alist
        '(("python3" . python-ts-mode)
          ("go"      . go-mode)
          ("rust"    . rust-mode)))

  ;; ── Reading the session from the browser ──────────────────────────
  ;; leetcode.el shells out to `my_cookies', which walks Chrome,
  ;; Chromium, Brave, Firefox, Edge, Vivaldi and Opera, and stops at the
  ;; first one that answers. Arc is in none of that list. Here it was
  ;; handing over a Chrome session that had expired months earlier;
  ;; LeetCode answered "user is not authenticated" and the failure looked
  ;; like a bug in Emacs.
  ;;
  ;; `workflow-tools/leetcode-cookies' reads Arc first, reports an EXPIRED
  ;; session instead of skipping past it, and gives every browser a time
  ;; budget — a loader stuck on a Keychain prompt would otherwise freeze
  ;; Emacs, since this is called through `shell-command-to-string'.
  (defadvice! +leetcode-cookies-path-a ()
    "Prefer our Arc-aware reader over the package's `my_cookies'."
    :override #'leetcode--my-cookies-path
    (or (executable-find "leetcode-cookies")
        (executable-find (expand-file-name
                          "workflow-tools/leetcode-cookies" "~/.config"))
        (executable-find (format "%s/bin/my_cookies" leetcode-python-environment))
        (executable-find "my_cookies")))

  ;; The reader explains itself on stderr -- which browser, and whether
  ;; the session is merely expired. `shell-command-to-string', which the
  ;; package uses, keeps stdout only, so that explanation was thrown away
  ;; and you were left with LeetCode's own "user is not authenticated".
  ;;
  ;; A `user-error' would not help either: this runs inside `leetcode--login',
  ;; an `aio-defun', so the signal would be swallowed by the promise the
  ;; same way the polling bug was. A warning buffer cannot be missed, and
  ;; the problem does need you to go and do something.
  (defadvice! +leetcode-cookie-get-all-a ()
    "Read browser cookies, surfacing the reader's diagnosis when it fails."
    :override #'leetcode--cookie-get-all
    (let* ((tool (leetcode--my-cookies-path))
           (errfile (make-temp-file "leetcode-cookies-")))
      (unwind-protect
          (with-temp-buffer
            (let ((code (if tool (call-process tool nil (list t errfile) nil) 127))
                  (out (buffer-string)))
              (if (zerop code)
                  (mapcar (lambda (l) (s-split-up-to " " l 1 'OMIT-NULLS))
                          (split-string out "\n" t))
                (display-warning
                 'leetcode
                 (concat (string-trim
                          (with-temp-buffer (insert-file-contents errfile)
                                            (buffer-string)))
                         (unless tool "\n  No cookie reader found on PATH."))
                 ;; :error and not :warning -- Doom pins
                 ;; `warning-minimum-level' to :error, so a :warning here
                 ;; is dropped without even being logged. And it is an
                 ;; error in substance: without a session, test and
                 ;; submit cannot work at all.
                 :error)
                nil)))
        (delete-file errfile))))

  ;; ── Where the statement opens ─────────────────────────────────────
  ;; `shr-render-buffer' does `pop-to-buffer "*html*"'. Doom has a
  ;; catch-all rule on "^\\*" that drops every starred buffer into a
  ;; drawer at the bottom, 16% of the frame tall: the statement came out
  ;; eight lines high.
  ;;
  ;; `leetcode--show-problem' also calls `leetcode--maybe-focus', which
  ;; runs `delete-other-windows' when `leetcode-focus' is t. Mid-solve
  ;; that DESTROYS the four-window layout we just built. We neutralise the
  ;; option here only: the list still opens on its own, since `leetcode'
  ;; calls `leetcode--maybe-focus' outside this advice.
  ;;
  ;; And `shr-render-buffer' fills in a TEMPORARY buffer: the width it
  ;; used was that of whatever window happened to be current, not of the
  ;; statement window, which does not exist yet. Hence lines of differing
  ;; lengths depending on where the problem was opened from.
  (defvar +leetcode-detail-width 0.55
    "Share of the frame width the statement takes beside the problem list.")

  (defvar +leetcode-solving-detail-width 0.4
    "Share of the frame width the right-hand column takes while solving.
The code gets the rest. Code is the pane you type in and the one whose
lines run long; the statement only needs its measure, which
`+leetcode-measure' already caps.")

  (defvar +leetcode--detail-window nil
    "Window to put the statement in once the solving layout is up.
A global rather than a dynamic binding: `aio' coroutines suspend, and a
`let' does not survive the suspension.")

  (defvar +leetcode-detail-float t
    "Open a statement read from the problem list in a floating frame.")

  (defvar +leetcode--floating-render nil
    "Bound while rendering a statement that is about to be floated.
shr needs a window to render into, but that window is temporary: we hand
it the current one and put the previous buffer back afterwards, so no
stray split is left behind next to the list.")

  (defun +leetcode--place-detail (buf _alist)
    "Put statement BUF in its dedicated window, otherwise to the right."
    (cond
     (+leetcode--floating-render
      (set-window-buffer (selected-window) buf)
      (selected-window))
     ((window-live-p +leetcode--detail-window)
      (set-window-buffer +leetcode--detail-window buf)
      (select-window +leetcode--detail-window)
      +leetcode--detail-window)
     ((seq-find (lambda (w) (string-match-p "\\`\\*leetcode-detail-"
                                            (buffer-name (window-buffer w))))
                (window-list))
      (let ((w (seq-find (lambda (w) (string-match-p "\\`\\*leetcode-detail-"
                                                     (buffer-name (window-buffer w))))
                         (window-list))))
        (set-window-buffer w buf) (select-window w) w))
     (t
      (display-buffer-in-direction
       buf `((direction . right) (window-width . ,+leetcode-detail-width))))))

  (defun +leetcode--float-other-statements (keep)
    "Close every floating statement other than KEEP.
Each problem gets its own detail buffer, so each one used to get its own
child frame -- all centred, all stacked at the same spot. Opening three
problems looked like three descriptions piled inside one window. Only one
statement floats at a time.

`posframe-delete-frame' and not `posframe-delete': the latter also kills
the buffer, and `leetcode--show-problem' needs the old statement buffers
to stay around."
    (dolist (b (buffer-list))
      (let ((name (buffer-name b)))
        (when (and (string-match-p "\\`\\*leetcode-detail-" name)
                   (not (eq b keep)))
          (ignore-errors (posframe-delete-frame b))))))

  (defun +leetcode--point-on-solve (frame buf)
    "Put point on the \"Solve it\" button of BUF, in FRAME's window.
A statement you just opened is a question -- solve it or not -- so the
cursor starts on the answer. The window keeps its own point, separate
from the buffer's, so setting it in the buffer alone would not show."
    (with-current-buffer buf
      (goto-char (point-min))
      (when (search-forward "Solve it" nil t)
        (goto-char (match-beginning 0))))
    (when (frame-live-p frame)
      (set-window-point (frame-selected-window frame)
                        (with-current-buffer buf (point)))))

  (defun +leetcode--leave-float ()
    "Step out of any floating statement, back onto the real frame.
The \"Solve it\" text button carries its own keymap, and a text-property
keymap wins over the major mode's: RET on that button runs `push-button',
never our command. So the escape hatch cannot live in a key binding -- it
has to sit on `leetcode--start-coding' itself, which is where every route
converges, mouse click included.

Without it the whole solving layout was built INSIDE the child frame:
`delete-other-windows' and the splits applied there, the statement stayed
up, and it read as the problem being displayed twice with no workspace in
sight."
    (when-let ((parent (frame-parent (selected-frame))))
      (select-frame-set-input-focus parent))
    (+leetcode--float-other-statements nil))

  (defadvice! +leetcode-start-coding-a (&rest _)
    "Never build the solving layout inside a floating frame."
    :before #'leetcode--start-coding
    (+leetcode--leave-float))

  (defun +leetcode--float-buffer (buf &optional width height)
    "Show BUF in a centred child frame and give it the keyboard."
    (+leetcode--float-other-statements buf)
    (let ((frame (posframe-show
                  buf
                  :poshandler #'posframe-poshandler-frame-center
                  :width (or width 92)
                  :height (or height (min 40 (- (frame-height) 6)))
                  ;; The internal border IS the padding. Left uncoloured
                  ;; it takes the frame background, so it reads as empty
                  ;; space on all four sides rather than as a ring. At 2px
                  ;; the text sat against the edge and looked cramped.
                  ;;
                  ;; No :border-width here, tempting as it is for a
                  ;; hairline: posframe maps it onto the SAME
                  ;; internal-border-width, so asking for a 1px outline
                  ;; silently reset the padding to 1px. The card separates
                  ;; from the desktop on its own -- it is opaque while the
                  ;; main frame is translucent.
                  :internal-border-width 20
                  :background-color (face-attribute 'default :background nil t)
                  ;; Fringes carry the last few pixels between padding and
                  ;; the first character. Buffer margins cannot: posframe
                  ;; resets them when it takes the buffer over.
                  :left-fringe 12
                  :right-fringe 12
                  :accept-focus t
                  :hidehandler nil)))
      ;; posframe leaves the keyboard on the parent frame, so the buffer's
      ;; own keymap would never see a key.
      (select-frame-set-input-focus frame)
      (+leetcode--point-on-solve frame buf)
      frame))

  (defun +leetcode--unfloat (buf)
    "Hide the child frame showing BUF and hand the keyboard back."
    (when (fboundp 'posframe-hide) (posframe-hide buf))
    (when-let ((parent (frame-parent (selected-frame))))
      (select-frame-set-input-focus parent)))

  (defun +leetcode-detail-quit ()
    "Close the statement: unfloat it, or bury the window."
    (interactive)
    (if (frame-parent (selected-frame))
        (+leetcode--unfloat (current-buffer))
      (quit-window)))

  (defun +leetcode-detail-solve ()
    "Solve the problem this statement belongs to.
The floating statement is a reading step: you look at it and then decide.
RET takes the decision -- it closes the frame and lays out the four
windows, exactly like the dashboard."
    (interactive)
    (let ((id (and (string-match "\\`\\*leetcode-detail-\\([0-9]+\\)\\*\\'" (buffer-name))
                   (match-string 1 (buffer-name)))))
      (unless id (user-error "Not a LeetCode statement buffer"))
      (+leetcode--leave-float)
      (+leetcode-resume id)))

  (defadvice! +leetcode-show-problem-a (fn &rest args)
    "Render the statement at a fixed measure; float it when read from the list."
    :around #'leetcode--show-problem
    ;; Never render into a child frame. If one holds the keyboard -- the
    ;; floating statement opened a moment ago, or the dashboard -- step
    ;; back to the real frame first: a child frame is transient, and
    ;; anything drawn there goes away with it, leaving the split it
    ;; borrowed behind on the parent.
    (when-let ((parent (frame-parent (selected-frame))))
      (select-frame-set-input-focus parent))
    ;; Upstream writes the header with `(number-to-string likes)' and no
    ;; guard. The list query does not return likes or dislikes, so on a
    ;; problem whose full detail was never fetched they are nil and the
    ;; call signals `wrong-type-argument numberp nil' -- inside an `aio'
    ;; coroutine, so it vanishes and the statement simply never opens.
    ;; `cl-struct-slot-value' and not `(setf (leetcode-problem-likes ...))':
    ;; the accessor's setf expander is generated by `cl-defstruct' and, in
    ;; interpreted code, resolving it needs `cl-macs' already loaded. The
    ;; first call from inside the coroutine hit
    ;; "Symbol's function definition is void: (setf leetcode-problem-likes)".
    ;; The generic slot accessor carries its own expander and always works.
    ;; Plain record operations, and deliberately so. Everything
    ;; `cl-defstruct' generates -- the predicate, the accessors, the setf
    ;; expander behind `cl-struct-slot-value' -- carries a compiler macro,
    ;; and none of those macros exist yet when config.el is READ. Each one
    ;; logged "Optimization failure for cl-typep: Unknown type
    ;; leetcode-problem" at every daemon start before quietly falling back
    ;; to a runtime call. Measured: two warnings per start, gone once the
    ;; last accessor left this block.
    ;;
    ;; `cl-struct-slot-offset' is an ordinary function, and a cl-struct is
    ;; a record, so aref/aset reach the slot with nothing to expand. The
    ;; offset is looked up by NAME, so a reordering upstream cannot make
    ;; this write into the wrong field.
    (let ((problem (car args)))
      (when (recordp problem)
        (dolist (slot '(likes dislikes))
          (let ((i (ignore-errors (cl-struct-slot-offset 'leetcode-problem slot))))
            (when (and i (not (numberp (aref problem i))))
              (aset problem i 0))))))
    (let* ((win (selected-window))
           (prev (window-buffer win))
           (from-list (with-current-buffer prev
                        (derived-mode-p 'leetcode--problems-mode)))
           (float (and from-list +leetcode-detail-float
                       (require 'posframe nil t)
                       (posframe-workable-p))))
      (let ((shr-width nil)
            (shr-max-width +leetcode-measure)
            (leetcode-focus nil)
            (+leetcode--floating-render float)
            (display-buffer-alist
             (cons (list "\\`\\*html\\*\\'" #'+leetcode--place-detail)
                   display-buffer-alist)))
        (apply fn args))
      (when float
        ;; The statement buffer comes from the WINDOW, not from
        ;; `current-buffer': `leetcode--show-problem' ends inside a
        ;; `with-current-buffer', which restores the buffer that was
        ;; current before -- the problem list. Reading it there floated
        ;; the list and left the statement behind in a split.
        (let ((detail (window-buffer win)))
          ;; Give the list its window back before floating: the render
          ;; borrowed it, and leaving the statement there would be the
          ;; very thing the floating frame is meant to avoid.
          (when (window-live-p win) (set-window-buffer win prev))
          (+leetcode--float-buffer detail)))))

  ;; `face-remap-set-base' rather than `face-remap-add-relative': by
  ;; replacing `shr-text''s base with a RELATIVE height and no `:inherit',
  ;; we also repair the heading hierarchy. `shr-h1' carries `:height 1.3',
  ;; but in the face list `(shr-text shr-h1)' it is `shr-text' that wins,
  ;; and its height inherited from `variable-pitch-text' is ABSOLUTE: it
  ;; was overriding that 1.3. Measured: the title came out at 20px,
  ;; exactly like the body.
  ;;
  ;; The remap is buffer-local: org and markdown keep SF Pro.
  ;;
  ;; `+word-wrap-mode' (from :ui word-wrap) rather than `visual-line-mode'
  ;; alone: it adds `adaptive-wrap', so the "Constraints" items resume
  ;; under their bullet instead of returning to the margin. shr already
  ;; breaks lines at render time; this mode is the safety net for when the
  ;; window is narrower than the measure — otherwise lines overflow.
  (defun +leetcode--code-tint ()
    "A background one step off the default, for code inside the statement.
Mixed from the theme's own colours: hardcoding a grey would break on the
next theme change, and read wrong in the other polarity."
    (let* ((bg (face-attribute 'default :background nil t))
           (fg (face-attribute 'default :foreground nil t)))
      (if (and (stringp bg) (stringp fg) (color-defined-p bg) (color-defined-p fg))
          (apply #'color-rgb-to-hex
                 (append (cl-mapcar (lambda (b f) (+ b (* 0.08 (- f b))))
                                    (color-name-to-rgb bg)
                                    (color-name-to-rgb fg))
                         '(2)))
        'unspecified)))

  (defun +leetcode-detail-h ()
    "Make the statement readable: reading face, wrapping, no line numbers."
    (face-remap-set-base 'shr-text
                         (list :family +leetcode-prose-font :height 1.15))
    ;; `shr-code' dresses BOTH inline <code> and the <pre> example blocks.
    ;; A faint background turns `nums[i]' and the Input/Output samples into
    ;; objects you can find while skimming, instead of grey text sitting in
    ;; grey text. The tint is derived from the theme, not hardcoded, so it
    ;; follows a theme change.
    (face-remap-set-base 'shr-code
                         (list :inherit 'fixed-pitch
                               :background (+leetcode--code-tint)))
    ;; Breathing room. Statement text runs right up to the window edge
    ;; otherwise, and the first character sits against the fringe.
    (setq-local left-margin-width 2)
    (setq-local right-margin-width 2)
    (setq-local line-spacing 6)
    (setq-local truncate-lines nil)
    (+word-wrap-mode +1)
    (display-line-numbers-mode -1))
  (add-hook 'leetcode--problem-detail-mode-hook #'+leetcode-detail-h)

  ;; ── Polling for the result ────────────────────────────────────────
  ;; Upstream bug, and it makes Test and Submit unusable. In
  ;; `leetcode--api-check-submission' the PENDING/STARTED branch of the
  ;; `pcase' is written with one pair of parentheses too many:
  ;;
  ;;   ((or "PENDING" "STARTED") ((aio-await ...) (aio-await ...)))
  ;;
  ;; So the body is not two successive forms but a SINGLE one whose
  ;; function position is the list `(aio-await ...)'. At runtime:
  ;; `invalid-function'. Verified by reproducing the exact shape.
  ;;
  ;; LeetCode always answers PENDING on the first poll, so the branch is
  ;; always taken — and the error, raised inside an `aio' coroutine nobody
  ;; awaits, is swallowed by the promise. Nothing in *Messages*, nothing
  ;; in the echo area: the result buffer sits on "Waiting for result..."
  ;; forever.
  ;;
  ;; The recursive call targets the original name: the advice redirects it
  ;; back here, so the whole loop runs through the fixed version.
  (defvar +leetcode-poll-interval 0.5
    "Seconds between two polls of the submission result.
The package uses 0.2 — five requests a second at LeetCode for as long as
the run takes. 0.5 is imperceptible and stays out of the way.")

  (aio-defun +leetcode--api-check-submission (interpret-id problem on-success)
    "Fixed copy of `leetcode--api-check-submission'."
    (let* ((title-slug (leetcode-problem-title-slug problem))
           (problem-id (leetcode-problem-id problem))
           (url-request-method "GET")
           (url-request-extra-headers
            `(,@(aio-await (leetcode--common-extra-headers))
              ,(leetcode--referer (format leetcode--url-problems title-slug))))
           (response (aio-await (aio-url-retrieve
                                 (format leetcode--url-check-submission interpret-id))))
           (response-status (car response))
           (response-buffer (cdr response)))
      (if-let ((error-info (plist-get response-status :error)))
          (progn
            (switch-to-buffer response-buffer)
            (leetcode--warn "LeetCode check submission ERROR: %S" error-info))
        (let ((result (leetcode--parse-buffer response-buffer)))
          ;; `url-retrieve' hands us a fresh buffer every time and never
          ;; reclaims it. At one poll every half second that piles up fast
          ;; -- 36 of them were sitting in the buffer list. Parsed is
          ;; parsed; the buffer has nothing left to give.
          (when (buffer-live-p response-buffer) (kill-buffer response-buffer))
          (let-alist result
            (pcase .state
              ((or "PENDING" "STARTED")
               (aio-await (aio-sleep +leetcode-poll-interval))
               (aio-await (leetcode--api-check-submission interpret-id problem on-success)))
              ("SUCCESS" (funcall on-success problem-id result))))))))
  (advice-add 'leetcode--api-check-submission
              :override #'+leetcode--api-check-submission)

  ;; ── Opening a problem cold ────────────────────────────────────────
  ;; `leetcode-solve-problem' shows the statement and starts coding, but
  ;; fetches nothing first -- it assumes you already opened the problem,
  ;; which is true when you press "c" in the list and false for every
  ;; other route, SPC l o included. The statement then renders from a
  ;; half-empty struct.
  (aio-defun +leetcode--solve-problem (problem-id)
    "Fetch what the statement needs, then open the problem."
    (let ((problem (leetcode--get-problem-by-id problem-id)))
      (unless problem
        (user-error "Unknown LeetCode problem: %s (load the list with SPC l l)"
                    problem-id))
      (aio-await (leetcode--ensure-question-content problem))
      (aio-await (leetcode--ensure-question-snippets problem))
      (aio-await (leetcode--ensure-question-testcases problem))
      (leetcode--show-problem problem)
      (leetcode--start-coding problem)))
  (advice-add 'leetcode-solve-problem :override #'+leetcode--solve-problem)

  ;; ── Making silent failures audible ────────────────────────────────
  ;; Every command here is an `aio-defun', and an error raised inside one
  ;; is captured by its promise. Nobody awaits these promises, so the
  ;; error is simply lost: no message, no *Backtrace*, nothing in
  ;; *Messages*. Three separate bugs in this file hid behind that -- the
  ;; polling parens, the nil likes above, the swallowed relayout error.
  ;; Attaching a listener costs nothing and turns silence into a message.
  ;; `url-retrieve' never reclaims its response buffers, and leetcode.el
  ;; makes one request per page of the list plus one per statement
  ;; fetched. They pile up as hidden " *http leetcode.com:443*" buffers --
  ;; 27 of them after one session. The polling loop kills its own now;
  ;; these are the rest.
  ;;
  ;; Only buffers whose process is gone, and only at entry points where
  ;; nothing else is in flight: a continuation still holding its response
  ;; buffer must not have it pulled away.
  (defun +leetcode--reap-http-buffers ()
    "Kill finished LeetCode HTTP response buffers."
    (dolist (b (buffer-list))
      (when (and (string-prefix-p " *http leetcode" (buffer-name b))
                 (null (get-buffer-process b)))
        (ignore-errors (kill-buffer b)))))

  (dolist (cmd '(leetcode leetcode-refresh-fetch leetcode-try leetcode-submit))
    (advice-add cmd :before #'+leetcode--reap-http-buffers))

  (defun +leetcode--surface-errors (promise)
    "Report a failure carried by PROMISE instead of losing it."
    (when (aio-promise-p promise)
      (aio-listen promise
                  (lambda (value)
                    (condition-case err (funcall value)
                      (error (message "LeetCode: %s" (error-message-string err)))))))
    promise)

  (dolist (cmd '(leetcode leetcode-daily leetcode-refresh-fetch
                 leetcode-show-problem leetcode-solve-problem
                 leetcode-try leetcode-submit))
    (advice-add cmd :filter-return #'+leetcode--surface-errors))

  ;; ── Laying the windows out without wrecking them ──────────────────
  ;; `leetcode-try' starts with `leetcode-restore-layout'. Upstream
  ;; rebuilds the layout EVERY TIME — `delete-other-windows' then three
  ;; splits — even when it is already up: every test loses your scroll
  ;; positions.
  ;;
  ;; Worse, it has this bug: `desc-buf' is bound BEFORE the
  ;; `(unless desc-buf (aio-await (leetcode-show-problem ...)))' and never
  ;; read again. The first time you test a problem whose statement is not
  ;; open yet, `desc-buf' is still nil at `(display-buffer desc-buf ...)'
  ;; — and `display-buffer' with nil displays the CURRENT BUFFER. That is
  ;; the solution file landing in the statement window.
  ;;
  ;; We override the package's window layout rather than write our own:
  ;; the tree keeps exactly the same SHAPE — code on the left, a column of
  ;; three on the right — only the proportions change. The package's
  ;; display functions (`leetcode--display-detail' and its siblings)
  ;; re-navigate that tree through `window-left-child'; keeping the shape
  ;; keeps them landing right, and `leetcode--start-coding' gets the same
  ;; proportions for free.
  (defvar +leetcode-detail-share 0.55
    "Share of the right-hand column's height given to the statement.
The package gives a third to each of the three right-hand buffers. The
statement is the only one you actually read; input and result fit in a
few lines.")

  (defun +leetcode--layout-intact-p (code-buf problem-id)
    "Is the layout already up FOR CODE-BUF and PROBLEM-ID?
The check is on buffer identity, not mere presence: switching from one
problem to another leaves all four windows in place but showing the
PREVIOUS problem. A check that settled for a name prefix believed the
layout was fine and left the old statement, input and result sitting
beside the new code."
    (let ((names (mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))))
      (and (member (buffer-name code-buf) names)
           (member (leetcode--detail-buffer-name problem-id) names)
           ;; Either of the two is fine: they share a pane, and which one
           ;; is up depends on whether you last ran a test or went to edit
           ;; the input.
           (or (member (leetcode--result-buffer-name problem-id) names)
               (member (leetcode--testcase-buffer-name problem-id) names)))))

  ;; Input and result SHARE one pane. The package gives each of the three
  ;; right-hand buffers a third of the column, which left the result on a
  ;; handful of lines -- the one buffer you actually need to read after a
  ;; run. And the input had stopped earning its own pane: the result now
  ;; prints the input of every case next to its output, so keeping it
  ;; permanently on screen was showing the same thing twice.
  ;;
  ;; It is a shared pane and not a removed buffer, because the input is
  ;; still yours to EDIT: SPC l g i swaps it in, SPC l g o swaps the result
  ;; back, and a run brings the result up on its own.
  (defadvice! +leetcode-solving-window-layout-a ()
    "Lay out Code | (Statement / Input+Result), code wide."
    :override #'leetcode--solving-window-layout
    (delete-other-windows)
    (let* ((w-desc (split-window-horizontally
                    (- (round (* (window-total-width) +leetcode-solving-detail-width)))))
           (w-bottom (with-selected-window w-desc
                       (split-window-below
                        (round (* (window-total-height w-desc) +leetcode-detail-share))))))
      (setq leetcode--description-window w-desc
            ;; Same window under both names: everything upstream that
            ;; targets one or the other lands in the shared pane.
            leetcode--testcase-window    w-bottom
            leetcode--result-window      w-bottom
            +leetcode--detail-window     w-desc)
      w-desc))

  ;; The package's display functions re-walk the window tree with
  ;; `window-left-child' and `window-next-sibling', counting on exactly
  ;; three children on the right. With two, the second `window-next-sibling'
  ;; returns nil and `set-window-buffer' with nil writes into the SELECTED
  ;; window -- the result would land in your code pane. We aim at the
  ;; windows we kept instead, which is what the layout function set them for.
  (defadvice! +leetcode-display-detail-a (buffer &optional _alist)
    :override #'leetcode--display-detail
    (when (window-live-p leetcode--description-window)
      (set-window-buffer leetcode--description-window buffer)
      leetcode--description-window))

  (defadvice! +leetcode-display-testcase-a (buffer &optional _alist)
    :override #'leetcode--display-testcase
    (when (window-live-p leetcode--testcase-window)
      (set-window-buffer leetcode--testcase-window buffer)
      leetcode--testcase-window))

  (defadvice! +leetcode-display-result-a (buffer &optional _alist)
    :override #'leetcode--display-result
    (when (window-live-p leetcode--result-window)
      (set-window-buffer leetcode--result-window buffer)
      leetcode--result-window))

  (defadvice! +leetcode-restore-layout-a ()
    "Restore the layout, without rebuilding one that already holds."
    :override #'leetcode-restore-layout
    (interactive)
    ;; Same guard: this reads the window list of the current frame, and a
    ;; child frame's window list is not the workspace.
    (+leetcode--leave-float)
    (let* ((code-buf (or (+leetcode--code-buffer)
                         (user-error "No LeetCode code buffer in this frame")))
           (slug (leetcode--get-slug-title code-buf))
           (problem (or (leetcode--get-problem slug)
                        (user-error "Unknown LeetCode problem: %s" slug)))
           (problem-id (leetcode-problem-id problem))
           (desc-buf (get-buffer (leetcode--detail-buffer-name problem-id)))
           (testcase-buf (get-buffer-create (leetcode--testcase-buffer-name problem-id)))
           (result-buf (get-buffer-create (leetcode--result-buffer-name problem-id))))
      (with-current-buffer result-buf
        (erase-buffer)
        (insert "Waiting for result..."))
      (if (+leetcode--layout-intact-p code-buf problem-id)
          ;; Already up for THIS problem: break nothing, but the shared
          ;; pane may be showing the INPUT -- you went to edit it, which
          ;; is the usual reason to run a test right after. A run is
          ;; starting, so bring the result up. Without this the output was
          ;; rendered into a buffer nobody could see, and testing looked
          ;; like it did nothing at all.
          (progn
            (setq +leetcode--detail-window
                  (get-buffer-window (leetcode--detail-buffer-name problem-id)))
            (when-let ((w (or (get-buffer-window (leetcode--testcase-buffer-name problem-id))
                              (get-buffer-window (leetcode--result-buffer-name problem-id)))))
              (set-window-buffer w result-buf)
              (setq leetcode--result-window w
                    leetcode--testcase-window w)))
        (select-window (or (get-buffer-window code-buf) (selected-window)))
        (switch-to-buffer code-buf)
        (+leetcode-solving-window-layout-a)
        ;; The shared pane shows the result: a run is about to start.
        (set-window-buffer leetcode--result-window result-buf)
        (ignore testcase-buf)
        ;; The statement: place it if it exists, otherwise ask for it and
        ;; `+leetcode--place-detail' will drop it into that same window.
        (if desc-buf
            (set-window-buffer +leetcode--detail-window desc-buf)
          (leetcode-show-problem problem-id)))
      (select-window (get-buffer-window code-buf))))

  ;; Input and result: plain text you read and edit. No line numbers, no
  ;; truncation.
  (defun +leetcode--plain-setup (buf)
    (with-current-buffer buf
      (setq-local truncate-lines nil)
      (setq-local line-spacing 2)
      (visual-line-mode +1)
      (display-line-numbers-mode -1)))

  (defadvice! +leetcode-plain-buffers-a (&rest _)
    "Tidy up the input and result buffers, and show the input first."
    :after #'leetcode--start-coding
    (dolist (b (buffer-list))
      (when (string-match-p "\\`\\*leetcode-\\(testcase\\|result\\)-" (buffer-name b))
        (+leetcode--plain-setup b)))
    ;; The statement, explicitly. `leetcode--solving-window-layout' splits
    ;; the CURRENT window, and the new windows inherit its buffer -- which
    ;; is how upstream ends up with the statement top-right: you were
    ;; looking at it when you pressed Solve it. Coming out of a floating
    ;; statement that no longer holds: the window being split is the
    ;; problem list, so the list was inherited into the statement pane.
    (when (window-live-p leetcode--description-window)
      (when-let* ((code (+leetcode--code-buffer))
                  (slug (ignore-errors (leetcode--get-slug-title code)))
                  (problem (leetcode--get-problem slug))
                  (detail (get-buffer (leetcode--detail-buffer-name
                                       (leetcode-problem-id problem)))))
        (set-window-buffer leetcode--description-window detail)))
    ;; `leetcode--start-coding' fills the shared pane with the result
    ;; buffer, which is empty until you run something. On a problem you
    ;; are only opening, the sample input is the useful thing to see.
    (when (window-live-p leetcode--result-window)
      (let ((shown (window-buffer leetcode--result-window)))
        (when (and (string-match-p "\\`\\*leetcode-result-" (buffer-name shown))
                   (zerop (buffer-size shown)))
          (when-let ((input (get-buffer
                             (replace-regexp-in-string
                              "-result-" "-testcase-" (buffer-name shown)))))
            (set-window-buffer leetcode--result-window input))))))

  ;; The problem list is a table: truncation is wanted, line numbers add
  ;; nothing.
  (add-hook! 'leetcode--problems-mode-hook
    (defun +leetcode-list-h ()
      (setq-local truncate-lines t)
      (display-line-numbers-mode -1)))


  ;; ── Test results ──────────────────────────────────────────────────
  ;; Upstream dumps two unlabelled lists one after the other -- "Code
  ;; Answer" then "Expected Code Answer" -- and leaves you to line them up
  ;; by eye. With three or four cases that is exactly the moment you stop
  ;; reading and go check on the website instead.
  ;;
  ;; Here each case is one block: what went in, what came out, and the
  ;; expected value ONLY when it differs. A wrong case is marked; a right
  ;; one stays quiet. The inputs come from the test-input buffer, whose
  ;; lines hold one parameter each -- so we chunk them by
  ;; (lines / number of cases). When that does not divide evenly the
  ;; problem takes a shape we cannot infer, and we simply drop the input
  ;; column rather than print a plausible-looking lie.
  (defface +leetcode-result-pass '((t :inherit success :weight bold))
    "Verdict of a passing run.")
  (defface +leetcode-result-fail '((t :inherit error :weight bold))
    "Verdict of a failing run.")
  (defface +leetcode-result-label '((t :inherit shadow))
    "Field labels in the result buffer.")

  (defun +leetcode--testcase-raw (problem-id)
    "The whole test input as one string, or nil when there is none."
    (when-let* ((buf (get-buffer (leetcode--testcase-buffer-name problem-id)))
                (text (string-trim (with-current-buffer buf (buffer-string))))
                ((not (string-empty-p text))))
      text))

  (defun +leetcode--testcase-inputs (problem-id cases)
    "Split the test input into CASES groups, or nil if it does not divide.
The input buffer holds one parameter per line, so a case is
(lines / cases) of them. When that does not divide, the problem takes a
shape we cannot infer -- the caller then falls back to printing the whole
block once, rather than pairing inputs with the wrong outputs."
    (when-let* ((text (+leetcode--testcase-raw problem-id))
                (lines (split-string text "\n" t))
                ((> cases 0))
                ((zerop (% (length lines) cases))))
      (let ((per (/ (length lines) cases)) out)
        (dotimes (i cases (nreverse out))
          (push (string-join (seq-subseq lines (* i per) (* (1+ i) per)) ", ") out)))))

  (defun +leetcode--result-field (label value &optional face)
    (insert (propertize (format "     %-8s " label) 'face '+leetcode-result-label)
            (if face (propertize (format "%s" value) 'face face) (format "%s" value))
            "\n"))

  (defadvice! +leetcode-show-testcases-result-a (problem-id result)
    "Lay the test run out one case per block."
    :override #'leetcode--show-testcases-result
    (let-alist result
      (with-current-buffer (get-buffer (leetcode--result-buffer-name problem-id))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (cond
           ;; 10 = the code ran; every case has an answer to compare.
           ((eq .status_code 10)
            (let* ((got .code_answer)
                   (want .expected_code_answer)
                   (n (length got))
                   (ok (equal got want))
                   (inputs (+leetcode--testcase-inputs problem-id n))
                   (wrong (cl-count-if (lambda (i) (not (equal (aref got i) (aref want i))))
                                       (number-sequence 0 (1- n)))))
              (insert "\n  "
                      (if ok (propertize "✓  Passed" 'face '+leetcode-result-pass)
                        (propertize "✗  Failed" 'face '+leetcode-result-fail))
                      (propertize (format "     %d/%d cases" (- n wrong) n)
                                  'face '+leetcode-result-label)
                      (if (and .status_runtime (not (string-empty-p .status_runtime)))
                          (propertize (format "     %s" .status_runtime)
                                      'face '+leetcode-result-label)
                        "")
                      "\n\n")
              ;; Chunking failed -- print what was sent, once, rather than
              ;; leave you guessing which input produced which output.
              (unless inputs
                (when-let ((raw (+leetcode--testcase-raw problem-id)))
                  (insert (propertize "  input\n" 'face '+leetcode-result-label))
                  (dolist (l (split-string raw "\n" t))
                    (insert "     " l "\n"))
                  (insert "\n")))
              (dotimes (i n)
                (let* ((g (aref got i)) (w (aref want i)) (good (equal g w)))
                  (insert "  "
                          (propertize (format "%d" (1+ i)) 'face 'bold) "  "
                          (if good (propertize "✓" 'face '+leetcode-result-pass)
                            (propertize "✗" 'face '+leetcode-result-fail))
                          "\n")
                  (when inputs (+leetcode--result-field "input" (nth i inputs)))
                  (+leetcode--result-field "output" g
                                           (unless good '+leetcode-result-fail))
                  (unless good (+leetcode--result-field "expected" w '+leetcode-result-pass))
                  (let ((so (and .std_output_list (> (length .std_output_list) i)
                                 (aref .std_output_list i))))
                    (when (and so (not (string-empty-p so)))
                      (+leetcode--result-field "stdout" (string-trim so))))
                  (insert "\n")))))
           ;; 12 = memory limit, 14 = time limit: one offending case.
           ((or (eq .status_code 12) (eq .status_code 14))
            (insert "\n  " (propertize (format "✗  %s" .status_msg)
                                       'face '+leetcode-result-fail)
                    (propertize (format "     %s/%s cases" .total_correct .total_testcases)
                                'face '+leetcode-result-label)
                    "\n\n")
            (+leetcode--result-field "input" .last_testcase)
            (+leetcode--result-field "expected" .expected_output)
            (unless (string-empty-p .std_output)
              (+leetcode--result-field "stdout" (string-trim .std_output))))
           ;; 15 = runtime error, 20 = compile error: the trace is the answer.
           ;; The input still matters -- a crash is usually about one case.
           ((memq .status_code '(15 20))
            (insert "\n  " (propertize (format "✗  %s" .status_msg)
                                       'face '+leetcode-result-fail)
                    "\n\n")
            (when-let ((raw (+leetcode--testcase-raw problem-id)))
              (+leetcode--result-field "input" (string-replace "\n" ", " raw))
              (insert "\n"))
            (insert (or .full_runtime_error .full_compile_error "") "\n"))
           (t
            (insert "\n  " (propertize (format "%s" (or .status_msg "?"))
                                       'face '+leetcode-result-fail) "\n")))
          ;; This pane is shared with the input buffer, so say how to get
          ;; back to it -- otherwise the input looks like it disappeared.
          (insert (propertize "\n  SPC l g i  edit the input\n"
                              'face '+leetcode-result-label))
          (goto-char (point-min))))))

  ;; ── Floating dashboard ────────────────────────────────────────────
  ;; A child frame rather than a window: the dashboard is something you
  ;; glance at and dismiss, and opening it should not disturb the four
  ;; windows you are working in. `posframe' is already pulled in by Doom.
  (defvar +leetcode-dashboard-float t
    "Show the dashboard in a floating child frame rather than a window.")

  (defun +leetcode-dashboard-quit ()
    "Close the dashboard, floating or not."
    (interactive)
    (if (and (fboundp 'posframe-hide) (posframe-workable-p))
        (progn (posframe-hide +leetcode-dashboard-buffer)
               ;; Focus went to the child frame; hand it back to the real one.
               (when-let ((p (frame-parent (selected-frame))))
                 (select-frame-set-input-focus p)))
      (quit-window)))

  ;; ── Filtering the problem list ────────────────────────────────────
  ;; `leetcode--filter' looks each row's problem back up by id, twice:
  ;;
  ;;   (leetcode-problem-tags       (leetcode--get-problem-by-id (aref row 1)))
  ;;   (leetcode-problem-difficulty (leetcode--get-problem-by-id (aref row 1)))
  ;;
  ;; When that lookup comes back nil -- and it does, on [Load More] with a
  ;; filter active -- the accessor is handed nil and signals
  ;; `wrong-type-argument leetcode-problem, nil'. The error lands in the
  ;; middle of a redisplay, so the list is left half-drawn.
  ;;
  ;; I could not reproduce it here across six pages and both filters, so
  ;; this does not chase the trigger: it removes the class.
  ;;
  ;;   - difficulty needs no lookup at all. The row already carries it in
  ;;     column 4 ("Easy" / "Medium" / "Hard"), which is where
  ;;     `leetcode--problems-rows' put it a moment earlier. One whole
  ;;     lookup goes away.
  ;;   - tags still need the problem, so that one is guarded. A row whose
  ;;     problem cannot be resolved is dropped from the filtered view
  ;;     instead of taking the whole refresh down with it.
  ;;
  ;; Behaviour is identical whenever the lookup succeeds, which is every
  ;; case that works today.
  (defadvice! +leetcode-filter-a (rows)
    "Filter ROWS, surviving a row whose problem cannot be looked up."
    :override #'leetcode--filter
    (seq-filter
     (lambda (row)
       (and
        (if leetcode--filter-regex
            (string-match-p leetcode--filter-regex (aref row 2))
          t)
        (if leetcode--filter-tag
            (when-let ((p (leetcode--get-problem-by-id (aref row 1))))
              (member leetcode--filter-tag (leetcode-problem-tags p)))
          t)
        (if leetcode--filter-difficulty
            (string-equal-ignore-case (aref row 4) leetcode--filter-difficulty)
          t)))
     rows))

  ;; ── Dashboard ─────────────────────────────────────────────────────
  ;; The numbers come from `leetcode--problems', filled by
  ;; `leetcode-refresh-fetch': each problem carries a `status' ("SOLVED"
  ;; once accepted) and a `difficulty'. Until the list has been fetched
  ;; once there is nothing to count — we say so rather than show
  ;; misleading zeros.
  (defvar +leetcode-dashboard-buffer "*leetcode-dashboard*"
    "Name of the dashboard buffer.")

  (defface +leetcode-dash-heading
    '((t :inherit font-lock-keyword-face :weight bold))
    "Section heading in the LeetCode dashboard.")

  (defface +leetcode-dash-dim
    '((t :inherit shadow))
    "Secondary text in the LeetCode dashboard.")

  (defun +leetcode--difficulty-face (d)
    (pcase (downcase (or d ""))
      ("easy"   'leetcode-easy-face)
      ("medium" 'leetcode-medium-face)
      ("hard"   'leetcode-hard-face)
      (_        '+leetcode-dash-dim)))

  (defun +leetcode--difficulty-name (d)
    (pcase (downcase (or d ""))
      ("easy" "Easy") ("medium" "Medium") ("hard" "Hard") (_ "?")))

  (defconst +leetcode--eighths ["" "▏" "▎" "▍" "▌" "▋" "▊" "▉"]
    "Partial blocks, from one eighth to seven eighths.")

  (defun +leetcode--bar (done total width)
    "Progress bar WIDTH cells wide.
LeetCode completion rates per difficulty run to single-digit percents:
rounded to the cell, the bar would be empty or nearly so and would say
nothing. So we go down to the eighth of a cell, and guarantee a mark as
soon as DONE clears zero — \"a little\" and \"none\" have to read apart at
a glance."
    (let* ((ratio (if (and total (> total 0)) (/ (float done) total) 0))
           (exact (* ratio width))
           (full (floor exact))
           (rest (floor (* 8 (- exact full))))
           (partial (aref +leetcode--eighths rest))
           (filled (concat (make-string full ?█) partial))
           (filled (if (and (string-empty-p filled) (> done 0)) "▏" filled))
           (empty (max 0 (- width (string-width filled)))))
      (concat (propertize filled 'face 'success)
              (propertize (make-string empty ?░) 'face '+leetcode-dash-dim))))

  ;; ── Real numbers, not "out of what we happened to load" ───────────
  ;; The old panel counted over `leetcode--problems', which holds only the
  ;; pages fetched so far. Fresh, that meant "11 / 100" -- a ratio out of
  ;; an arbitrary window, presented as if it were the catalogue.
  ;;
  ;; LeetCode answers both figures directly, so we ask. The same call also
  ;; carries what no local count could know: your percentile per
  ;; difficulty, the current streak, and the submission calendar.
  (defvar +leetcode--profile nil
    "Cached profile payload, refreshed by `g' in the dashboard.")

  (defconst +leetcode--graphql-profile
    "query dashboardProfile($username: String!) {
       allQuestionsCount { difficulty count }
       matchedUser(username: $username) {
         profile { ranking }
         problemsSolvedBeatsStats { difficulty percentage }
         submitStatsGlobal { acSubmissionNum { difficulty count } }
         userCalendar { streak totalActiveDays submissionCalendar }
       }
     }")

  (defun +leetcode--fetch-profile ()
    "Fetch the profile block, or nil. Never signals: the dashboard degrades."
    (ignore-errors
      (with-timeout (6 nil)
        (let* ((user (leetcode-user-username leetcode--user))
               (url-request-method "POST")
               (url-request-extra-headers
                (list leetcode--User-Agent leetcode--Content-Type))
               (url-request-data
                (leetcode--graphql-payload
                 "dashboardProfile" +leetcode--graphql-profile
                 (list (cons "username" user))))
               (resp (aio-wait-for (aio-url-retrieve leetcode--url-graphql)))
               (buf (cdr resp)))
          (unwind-protect
              (with-current-buffer buf
                (goto-char url-http-end-of-headers)
                (json-read))
            (when (buffer-live-p buf) (kill-buffer buf)))))))

  (defun +leetcode--alist-count (vec difficulty)
    "COUNT for DIFFICULTY inside VEC, a vector of {difficulty,count} alists."
    (catch 'hit
      (dotimes (i (length vec) 0)
        (let-alist (aref vec i)
          (when (equal .difficulty difficulty) (throw 'hit (or .count 0)))))))

  (defun +leetcode--alist-pct (vec difficulty)
    "PERCENTAGE for DIFFICULTY, or nil when LeetCode has none to give."
    (catch 'hit
      (dotimes (i (length vec) nil)
        (let-alist (aref vec i)
          (when (equal .difficulty difficulty) (throw 'hit .percentage))))))

  (defun +leetcode--calendar-days (raw days)
    "Submissions per day over the last DAYS, oldest first."
    (let ((cal (ignore-errors
                 (json-parse-string (or raw "{}") :object-type 'alist)))
          (today (time-to-days (current-time)))
          out)
      ;; `push' prepends, and we walk i from today backwards, so the list
      ;; comes out oldest-first already. An `nreverse' here put today at
      ;; the LEFT -- the spike from yesterday appeared three weeks ago.
      (dotimes (i days out)
        (let* ((day (- today i))
               (n (seq-reduce
                   (lambda (acc c)
                     (+ acc (if (= day (time-to-days
                                        (seconds-to-time
                                         (string-to-number (symbol-name (car c))))))
                                (cdr c) 0)))
                   cal 0)))
          (push n out)))))

  (defun +leetcode--calendar-total (raw)
    "Every submission the calendar knows about (LeetCode keeps one year)."
    (let ((cal (ignore-errors (json-parse-string (or raw "{}") :object-type 'alist))))
      (seq-reduce (lambda (acc c) (+ acc (cdr c))) cal 0)))

  (defun +leetcode--calendar-weeks (raw n)
    "The last N seven-day blocks, most recent first.
Each element is (LABEL DAYS TOTAL), DAYS running oldest to newest inside
the block. Rolling blocks ending today, not calendar weeks: \"this week\"
should mean the last seven days, not \"since Monday\" -- on a Monday
morning the calendar version is empty and says nothing."
    (let ((days (+leetcode--calendar-days raw (* 7 n)))
          out)
      (dotimes (w n (nreverse out))
        (let* ((end (- (length days) (* 7 w)))
               (chunk (seq-subseq days (max 0 (- end 7)) end)))
          (push (list (pcase w
                        (0 "This week")
                        (1 "Last week")
                        (_ (format "%d weeks ago" w)))
                      chunk
                      (apply #'+ chunk))
                out)))))

  (defconst +leetcode--spark ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"]
    "Sparkline ramp, one glyph per eighth of the tallest bar.")

  (defun +leetcode--sparkline (counts)
    "Render COUNTS as a sparkline scaled to its own maximum."
    (let ((peak (apply #'max 1 counts)))
      (mapconcat
       (lambda (n)
         (if (zerop n)
             (propertize "·" 'face '+leetcode-dash-dim)
           (propertize (aref +leetcode--spark
                             (min 7 (floor (* 7.99 (/ (float n) peak)))))
                       'face 'success)))
       counts "")))

  (defun +leetcode--top-tags (n)
    "The N tags you have solved most, from the problems loaded so far."
    (let ((h (make-hash-table :test #'equal)))
      (dolist (p (leetcode-problems-problems leetcode--problems))
        (when (equal (leetcode-problem-status p) "SOLVED")
          (dolist (tag (leetcode-problem-tags p))
            (puthash tag (1+ (gethash tag h 0)) h))))
      (seq-take (sort (let (out) (maphash (lambda (k v) (push (cons k v) out)) h) out)
                      (lambda (a b) (> (cdr a) (cdr b))))
                n)))

  (defun +leetcode--stats ()
    "Count problems by difficulty: ((DIFFICULTY SOLVED TOTAL) ...)."
    (let ((tbl (list (list "easy" 0 0) (list "medium" 0 0) (list "hard" 0 0))))
      (dolist (p (leetcode-problems-problems leetcode--problems) tbl)
        (when-let ((row (assoc (downcase (or (leetcode-problem-difficulty p) "")) tbl)))
          (cl-incf (nth 2 row))
          (when (equal (leetcode-problem-status p) "SOLVED")
            (cl-incf (nth 1 row)))))))

  (defun +leetcode--verdict (id)
    "LeetCode's own verdict for problem ID: `solved', `attempted', `todo'.
Returns nil when the problem is not among the pages fetched, which is a
different thing from \"not attempted\" and is shown differently."
    (when-let ((p (leetcode--get-problem-by-id id)))
      (pcase (leetcode-problem-status p)
        ("SOLVED"    'solved)
        ("ATTEMPTED" 'attempted)
        (_           'todo))))

  (defun +leetcode--sessions ()
    "Problems started: ((ID TITLE DIFFICULTY STATE VERDICT) ...), by ID.
STATE is `open' when a code buffer is still alive, `file' when only the
solution on disk remains. VERDICT is what LeetCode says about it."
    (let ((h (make-hash-table :test #'equal)))
      ;; Disk first, buffers second: an open buffer wins.
      (when (file-directory-p leetcode-directory)
        (dolist (f (directory-files leetcode-directory nil "\\`[0-9]+_"))
          (let* ((id (car (split-string f "_")))
                 (p (leetcode--get-problem-by-id id)))
            (puthash id (list id
                              (if p (leetcode-problem-title p)
                                (file-name-base (string-join (cdr (split-string f "_")) "_")))
                              (and p (leetcode-problem-difficulty p))
                              'file
                              (+leetcode--verdict id))
                     h))))
      (dolist (b (buffer-list))
        (when (buffer-local-value 'leetcode-solution-mode b)
          (when-let* ((slug (ignore-errors (leetcode--get-slug-title b)))
                      (p (leetcode--get-problem slug)))
            (puthash (leetcode-problem-id p)
                     (list (leetcode-problem-id p) (leetcode-problem-title p)
                           (leetcode-problem-difficulty p) 'open
                           (+leetcode--verdict (leetcode-problem-id p)))
                     h))))
      (sort (hash-table-values h)
            (lambda (a b) (< (string-to-number (car a)) (string-to-number (car b)))))))

  (defun +leetcode-resume (id)
    "Resume problem ID: its file and its four windows."
    (interactive)
    (unless leetcode--lang (setq leetcode--lang leetcode-prefer-language))
    (let* ((p (leetcode--get-problem-by-id id))
           (name (and p (leetcode--get-code-buffer-name (leetcode-problem-title p))))
           (buf (and name (get-buffer name))))
      (if buf
          ;; Already open: keep the buffer exactly as it is — undo
          ;; history, point, unsaved edits — and only lay the windows out
          ;; around it.
          (progn (pop-to-buffer buf) (leetcode-restore-layout))
        ;; Cold: `leetcode-solve-problem' fetches statement and snippets,
        ;; opens the file and lays out the windows.
        (leetcode-solve-problem id))))

  (defun +leetcode-dashboard-resume ()
    "Resume the problem on the current line."
    (interactive)
    (if-let ((id (get-text-property (point) '+leetcode-id)))
        (+leetcode-resume id)
      (user-error "No problem on this line")))

  (defface +leetcode-dash-title '((t :height 1.6 :weight bold))
    "The word LeetCode at the top of the dashboard.")

  (defun +leetcode--rule (label width)
    "A section heading followed by a hairline out to WIDTH."
    (concat "  " (propertize label 'face '+leetcode-dash-heading) "  "
            (propertize (make-string (max 0 (- width (length label) 4)) ?─)
                        'face '+leetcode-dash-dim)
            "\n\n"))

  (defun +leetcode--dashboard-render ()
    "Write the dashboard contents into the current buffer."
    (let* ((inhibit-read-only t)
           (W 66)
           (user (or (leetcode-user-username leetcode--user) ""))
           (prof (cdr (assq 'data (or +leetcode--profile '())))))
      (erase-buffer)
      (insert "\n  " (propertize "LeetCode" 'face '+leetcode-dash-title)
              (if (string-empty-p user) ""
                (concat "   " (propertize user 'face '+leetcode-dash-dim)))
              "\n\n")

      (if (null prof)
          (insert "  " (propertize "No data yet." 'face '+leetcode-dash-dim)
                  "\n  Sign in and open the list once (SPC l l), then press g.\n\n")
        (let-alist prof
          ;; ── Progress ───────────────────────────────────────────────
          (insert (+leetcode--rule "Progress" W))
          ;; Two different questions, so two different devices.
          ;;
          ;; The PERCENTAGE answers "how far through the catalogue" -- and
          ;; early on it is honestly tiny: 11 of 4042 is 0.3 %. A bar drawn
          ;; from that ratio is a flat line for every row, which is why the
          ;; first version said nothing.
          ;;
          ;; The BAR therefore answers a question you can actually read at
          ;; this stage: how your solves are spread across difficulties. It
          ;; is scaled to your own best row, so Easy fills it and Medium
          ;; shows as the fraction of Easy that it is.
          (let* ((counts (mapcar (lambda (d)
                                   (+leetcode--alist-count
                                    .matchedUser.submitStatsGlobal.acSubmissionNum d))
                                 '("Easy" "Medium" "Hard")))
                 (peak (apply #'max 1 counts)))
            (dolist (d '("All" "Easy" "Medium" "Hard"))
              (let* ((done (+leetcode--alist-count
                            .matchedUser.submitStatsGlobal.acSubmissionNum d))
                     (tot  (+leetcode--alist-count .allQuestionsCount d))
                     (beat (+leetcode--alist-pct
                            .matchedUser.problemsSolvedBeatsStats d))
                     (name (if (equal d "All") "Total" d)))
                (insert (string-trim-right
                         (format "  %s %5d / %-5d %6.1f %%   %s  %s"
                                (propertize (format "%-7s" name)
                                            'face (if (equal d "All") 'bold
                                                    (+leetcode--difficulty-face d)))
                                done tot
                                (if (> tot 0) (* 100.0 (/ (float done) tot)) 0.0)
                                ;; No bar on Total: it is the sum of the
                                ;; three below, so a fourth bar would only
                                ;; repeat them.
                                (if (equal d "All")
                                    (make-string 20 ?\s)
                                  (+leetcode--bar done peak 20))
                                ;; LeetCode's own wording. "top 71 %" is the
                                ;; same number worn the other way round, and
                                ;; it flatters -- it would not match what the
                                ;; site shows you.
                                (if (numberp beat)
                                    (propertize (format "beats %.0f %%" beat)
                                                'face '+leetcode-dash-dim)
                                  "")))
                        ;; Trimmed, not padded: without a percentile the
                        ;; row ended in two stray spaces.
                        "\n"))))
          (insert "  " (propertize "bars compare your own difficulties; the percent is the catalogue"
                                   'face '+leetcode-dash-dim)
                  "\n\n")

          ;; ── Activity ───────────────────────────────────────────────
          (insert (+leetcode--rule "Activity" W))
          ;; One row per week, seven cells each, the week's total on the
          ;; right. The single 21-cell sparkline that was here before could
          ;; not answer "how much did I do last week" -- you had to count
          ;; glyphs and guess where one week ended.
          (let* ((raw .matchedUser.userCalendar.submissionCalendar)
                 (weeks (+leetcode--calendar-weeks raw 4))
                 (streak (or .matchedUser.userCalendar.streak 0)))
            (insert (format "  %-13s %-16s %s %s\n"
                            (propertize "Streak" 'face '+leetcode-dash-dim)
                            (propertize (format "%d day%s" streak (if (= 1 streak) "" "s"))
                                        'face (if (> streak 0) 'success '+leetcode-dash-dim))
                            (propertize "Active days  " 'face '+leetcode-dash-dim)
                            (format "%d" (or .matchedUser.userCalendar.totalActiveDays 0))))
            (insert "\n")
            ;; No M T W S header: these are ROLLING seven-day blocks ending
            ;; today, so a Monday column would be a lie six days out of
            ;; seven. The arrow says what the axis is instead.
            (insert (format "  %-13s %s\n"
                            ""
                            (propertize "older ─────────→ today"
                                        'face '+leetcode-dash-dim)))
            (dolist (wk weeks)
              (insert (format "  %-13s %s   %s\n"
                              (propertize (nth 0 wk) 'face '+leetcode-dash-dim)
                              ;; The glyph height is the raw count, not a
                              ;; ratio: one bar means one submission in every
                              ;; row. A per-week scale would make a quiet week
                              ;; look as busy as a loud one.
                              (mapconcat (lambda (n)
                                           (if (zerop n)
                                               (propertize " · " 'face '+leetcode-dash-dim)
                                             (propertize
                                              (format " %s " (aref +leetcode--spark
                                                                   (min 7 (1- (max 1 n)))))
                                              'face 'success)))
                                         (nth 1 wk) "")
                              (if (zerop (nth 2 wk))
                                  (propertize "0" 'face '+leetcode-dash-dim)
                                (propertize (number-to-string (nth 2 wk)) 'face 'success)))))
            ;; Aligned under the weekly totals, not floating in the middle
            ;; of the row: it is the same quantity over a longer window, so
            ;; it belongs in the same column.
            (insert (format "  %-13s %-21s   %s\n"
                            (propertize "Past year" 'face '+leetcode-dash-dim)
                            ""
                            (propertize (number-to-string
                                         (+leetcode--calendar-total raw))
                                        'face '+leetcode-dash-dim))))
          (insert "\n")

          ;; ── Strengths ──────────────────────────────────────────────
          ;; Computed locally, so it only knows the pages already fetched.
          ;; Saying so is the difference between a statistic and a guess.
          (let ((tags (+leetcode--top-tags 5))
                (loaded (length (leetcode-problems-problems leetcode--problems))))
            (when tags
              (insert (+leetcode--rule "Strengths" W))
              (let ((peak (cdar tags)))
                (dolist (tg tags)
                  (insert (format "  %-20s %3d  %s\n"
                                  (truncate-string-to-width (car tg) 20) (cdr tg)
                                  (propertize (make-string
                                               (max 1 (round (* 18 (/ (float (cdr tg)) peak))))
                                               ?▬)
                                              'face 'success)))))
              (insert "  " (propertize (format "from %d problems loaded — G in the list fetches more"
                                               loaded)
                                       'face '+leetcode-dash-dim)
                      "\n\n")))))

      ;; ── Problems started ─────────────────────────────────────────
      (insert (+leetcode--rule "Problems started" W))
      (let ((sessions (+leetcode--sessions)))
        (if (null sessions)
            (insert "  " (propertize "None yet.\n" 'face '+leetcode-dash-dim))
          (dolist (s sessions)
            (let* ((verdict (nth 4 s))
                   ;; The glyph is LeetCode's verdict, not the state of your
                   ;; buffer -- that is the question you actually ask of this
                   ;; list. The buffer state stays, dimmed, at the end.
                   (mark (pcase verdict
                           ('solved    (propertize "✓" 'face 'success))
                           ('attempted (propertize "●" 'face 'leetcode-medium-face))
                           ('todo      (propertize "○" 'face '+leetcode-dash-dim))
                           (_          (propertize "·" 'face '+leetcode-dash-dim))))
                   (label (pcase verdict
                            ('solved    (propertize "accepted"  'face 'success))
                            ('attempted (propertize "attempted" 'face 'leetcode-medium-face))
                            ('todo      (propertize "not sent"  'face '+leetcode-dash-dim))
                            (_          (propertize "unknown"   'face '+leetcode-dash-dim))))
                   (line (format "  %s %4s  %-30s %-7s %-10s %s\n"
                                 mark
                                 (nth 0 s)
                                 (truncate-string-to-width (or (nth 1 s) "?") 30 nil nil "…")
                                 (propertize (+leetcode--difficulty-name (nth 2 s))
                                             'face (+leetcode--difficulty-face (nth 2 s)))
                                 label
                                 (propertize (if (eq (nth 3 s) 'open) "open" "")
                                             'face '+leetcode-dash-dim))))
              (insert (propertize (string-trim-right line) '+leetcode-id (nth 0 s))
                      "\n")))))

      (insert "\n  " (propertize "RET resume   l list   d daily   g refresh   ? help   q close"
                                 'face '+leetcode-dash-dim)
              "\n")
      (goto-char (point-min))))

  (defvar +leetcode-dashboard-mode-map
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map)
      (define-key map (kbd "RET") #'+leetcode-dashboard-resume)
      (define-key map "g" #'+leetcode-dashboard-refresh)
      (define-key map "l" #'leetcode)
      (define-key map "d" #'leetcode-daily)
      (define-key map "?" #'+leetcode-cheatsheet)
      (define-key map "q" #'+leetcode-dashboard-quit)
      map)
    "Keymap for the LeetCode dashboard.")

  (define-derived-mode +leetcode-dashboard-mode special-mode "LC Dashboard"
    "LeetCode dashboard."
    (setq-local truncate-lines t)
    (setq-local line-spacing 3)
    (display-line-numbers-mode -1)
    (hl-line-mode +1)
    ;; Same reason as elsewhere in this package: modes derived from
    ;; `special-mode' are driven from the keyboard, and evil has to see
    ;; the map.
    (when (featurep 'evil)
      (setq evil-normal-state-local-map +leetcode-dashboard-mode-map)))

  (defun +leetcode-dashboard-refresh ()
    "Re-fetch the profile, then redraw."
    (interactive)
    (setq +leetcode--profile (+leetcode--fetch-profile))
    (with-current-buffer (get-buffer-create +leetcode-dashboard-buffer)
      (+leetcode--dashboard-render)))

  (defun +leetcode-dashboard ()
    "Open the LeetCode dashboard, floating unless `+leetcode-dashboard-float' is nil."
    (interactive)
    (unless +leetcode--profile
      (setq +leetcode--profile (+leetcode--fetch-profile)))
    (with-current-buffer (get-buffer-create +leetcode-dashboard-buffer)
      (unless (derived-mode-p '+leetcode-dashboard-mode) (+leetcode-dashboard-mode))
      (+leetcode--dashboard-render))
    (if (and +leetcode-dashboard-float
             (require 'posframe nil t)
             (posframe-workable-p))
        (let ((frame (posframe-show
                      +leetcode-dashboard-buffer
                      :poshandler #'posframe-poshandler-frame-center
                      :width 84
                      :height (min 34 (- (frame-height) 6))
                      :internal-border-width 2
                      :internal-border-color (face-attribute 'font-lock-comment-face
                                                             :foreground nil t)
                      :background-color (face-attribute 'default :background nil t)
                      :accept-focus t
                      :hidehandler nil)))
          ;; posframe leaves focus on the parent, so the keymap would not
          ;; get the keys. RET has to reach the dashboard for it to be of
          ;; any use.
          (select-frame-set-input-focus frame))
      (switch-to-buffer +leetcode-dashboard-buffer)))

  ;; ── Cheat sheet ───────────────────────────────────────────────────
  (defvar +leetcode-cheatsheet
    '(("Get in"
       ("SPC l l" "Problem list")
       ("SPC l b" "Dashboard")
       ("SPC l d" "Daily problem")
       ("SPC l o" "Resume a problem you started"))
      ("Solve"
       ("SPC l t" "Run the tests against the input")
       ("SPC l s" "Submit")
       ("SPC l w" "Lay the four windows out again"))
      ("Move around"
       ("SPC l g c" "Code")
       ("SPC l g p" "Statement (problem)")
       ("SPC l g i" "Input")
       ("SPC l g o" "Output (result)")
       ("SPC l g l" "Problem list"))
      ("Housekeeping"
       ("SPC l r" "Refresh the list")
       ("SPC l R" "Refetch from LeetCode")
       ("SPC l q" "Close everything")
       ("SPC l ?" "This cheat sheet"))
      ("In the statement"
       ("c" "Go to the code") ("i" "Go to the input") ("o" "Go to the result")
       ("t" "Test") ("s" "Submit") ("q" "Close"))
      ("In the code"
       ("SPC m l t" "Test") ("SPC m l s" "Submit")
       ("SPC m l w" "Lay the windows out again")
       ("C-c C-t / C-c C-s" "Test / submit (the package's own keys)"))
      ("In the list"
       ("RET" "Open the statement") ("c" "Solve") ("s" "Filter by title")
       ("d" "Filter by difficulty") ("z" "Refresh") ("q" "Close")))
    "LeetCode cheat sheet: sections of (KEY DESCRIPTION).")

  (defun +leetcode-cheatsheet ()
    "Show the LeetCode key bindings."
    (interactive)
    (with-current-buffer (get-buffer-create "*leetcode-help*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (setq-local line-spacing 3)
        (display-line-numbers-mode -1)
        (when (featurep 'evil) (setq evil-normal-state-local-map special-mode-map))
        (insert "\n  " (propertize "LeetCode — cheat sheet"
                                   'face '(:height 1.4 :weight bold)) "\n")
        (dolist (section +leetcode-cheatsheet)
          (insert "\n  " (propertize (car section) 'face '+leetcode-dash-heading) "\n")
          (dolist (row (cdr section))
            (insert (format "    %-22s %s\n"
                            (propertize (car row) 'face 'help-key-binding)
                            (cadr row)))))
        (insert "\n  " (propertize "q to close" 'face '+leetcode-dash-dim) "\n")
        (goto-char (point-min))))
    (switch-to-buffer "*leetcode-help*"))

  ;; ── Local keys ────────────────────────────────────────────────────
  ;; Bound DIRECTLY in the keymaps, with no state prefix:
  ;; `leetcode--set-evil-local-map' installs these maps as-is into
  ;; `evil-normal-state-local-map', so a plain binding is live in normal
  ;; state as well as everywhere else.
  (map! :map leetcode--problems-mode-map
        "q" #'quit-window
        "r" #'leetcode-refresh)

  (map! :map leetcode--problem-detail-mode-map
        "RET" #'+leetcode-detail-solve
        "q" #'+leetcode-detail-quit
        "c" #'+leetcode-goto-code
        "i" #'+leetcode-goto-testcase
        "o" #'+leetcode-goto-result
        "t" #'+leetcode-try
        "s" #'+leetcode-submit)

  ;; In the code buffer, Doom's localleader — under an "l" prefix rather
  ;; than at the root. `leetcode-solution-mode' is a MINOR mode: its
  ;; bindings come ahead of the major mode's. Put straight on SPC m t, it
  ;; masked python-mode's pytest menu, silently, and precisely where a
  ;; Python developer types "t" for "test". Under SPC m l nothing is
  ;; covered.
  (map! :map leetcode-solution-mode-map
        :localleader
        (:prefix ("l" . "leetcode")
         :desc "Test"              "t" #'+leetcode-try
         :desc "Submit"            "s" #'+leetcode-submit
         :desc "Lay windows out"   "w" #'leetcode-restore-layout
         :desc "Statement"         "p" #'+leetcode-goto-description
         :desc "Test input"        "i" #'+leetcode-goto-testcase
         :desc "Result"            "o" #'+leetcode-goto-result)))

(map! :leader
      (:prefix ("l" . "leetcode")
       ;; Get in
       :desc "Problem list"        "l" #'leetcode
       :desc "Dashboard"           "b" #'+leetcode-dashboard
       :desc "Daily problem"       "d" #'leetcode-daily
       :desc "Resume a problem"    "o" #'+leetcode-open-session
       ;; Solve
       :desc "Test"                "t" #'+leetcode-try
       :desc "Submit"              "s" #'+leetcode-submit
       :desc "Lay windows out"     "w" #'leetcode-restore-layout
       ;; Housekeeping
       :desc "Refresh"             "r" #'leetcode-refresh
       :desc "Refetch from LeetCode" "R" #'leetcode-refresh-fetch
       :desc "Close everything"    "q" #'leetcode-quit
       :desc "Cheat sheet"         "?" #'+leetcode-cheatsheet
       (:prefix ("g" . "go to")
        :desc "Code"      "c" #'+leetcode-goto-code
        :desc "Statement" "p" #'+leetcode-goto-description
        :desc "Input"     "i" #'+leetcode-goto-testcase
        :desc "Result"    "o" #'+leetcode-goto-result
        :desc "List"      "l" #'+leetcode-goto-list)))

;; ── LeetCode gets its own workspace ─────────────────────────────────
;; LeetCode opens four windows and keeps a pile of transient buffers
;; alive. Dropped into whatever workspace you happened to be in, it
;; buries the layout you were working in; `SPC TAB d' would then be the
;; only way back, and your files would be mixed in with *leetcode-*
;; buffers in every buffer list.
;;
;; The four commands below are the ONLY ways in -- every other LeetCode
;; key acts on a session that is already open, so it is already in the
;; right workspace and needs no advice.
;;
;; `:before' rather than a wrapper command: the bindings, `M-x', and the
;; calls LeetCode makes internally all go through the same path, so there
;; is no second entry point left uncovered.
(defvar +leetcode-workspace-name "leetcode"
  "Name of the workspace LeetCode is confined to.")

(defun +leetcode-ensure-workspace (&rest _)
  "Switch to the LeetCode workspace, creating it only if absent.
Does nothing when already there -- re-entering would otherwise reset the
window configuration of a session in progress."
  (when (and (bound-and-true-p persp-mode)
             (not (equal (+workspace-current-name) +leetcode-workspace-name)))
    (+workspace-switch +leetcode-workspace-name t)
    (+workspace/display)))

(dolist (cmd '(leetcode
               leetcode-daily
               +leetcode-dashboard
               +leetcode-open-session))
  (advice-add cmd :before #'+leetcode-ensure-workspace))


;; ══════════════════════════════════════════════════════════════════════
;;  Modern IDE setup (2026-08-21)
;; ══════════════════════════════════════════════════════════════════════

;; ── Fallback font ───────────────────────────────────────────────────
;; `doom doctor' asks for Symbola: it is Emacs' last-resort font when no
;; active font can draw a character. With no fallback, those characters
;; cause severe slowdowns and can crash Emacs.
;; Symbola no longer exists as a Homebrew cask; JetBrainsMono Nerd Font
;; covers the essentials (it bundles Nerd Fonts plus broad Unicode
;; coverage) and is already `doom-symbol-font'. We declare it explicitly
;; as the fallback for the `symbol' and `mathematical' charsets, and leave
;; emoji to Apple Color Emoji, which renders them in colour.
;;
;; This must run once per frame: in daemon mode the first frame does not
;; exist yet when this file is loaded.
(defun +set-symbol-fallback-fonts-h (&optional frame)
  "Register the fallback fonts for symbols and emoji."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (dolist (charset '(symbol mathematical))
        (set-fontset-font t charset
                          (font-spec :family "JetBrainsMono Nerd Font")
                          nil 'append))
      (set-fontset-font t 'emoji
                        (font-spec :family "Apple Color Emoji")
                        nil 'prepend))))
(add-hook 'after-make-frame-functions #'+set-symbol-fallback-fonts-h)
(add-hook 'doom-after-init-hook #'+set-symbol-fallback-fonts-h)

;; ── vundo: the undo tree, on demand ─────────────────────────────────
;; `:emacs undo' without +tree now uses undo-fu, which builds on Emacs'
;; native undo. undo-tree kept a tree in memory permanently — the cost
;; shows on large buffers, and its serialised history was corruption
;; prone. vundo only DRAWS the history when you open it.
(use-package! vundo
  :defer t
  :commands (vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols   ; otherwise plain ASCII
        vundo-compact-display t)
  (map! :map vundo-mode-map
        :n "l" #'vundo-forward
        :n "h" #'vundo-backward
        :n "j" #'vundo-next
        :n "k" #'vundo-previous
        :n "q" #'vundo-quit
        :n "RET" #'vundo-confirm))

(map! :leader :desc "Undo history (vundo)" "o u" #'vundo)

;; ── indent-bars: stipple indentation guides ─────────────────────────
;; Draws the guides with a font texture instead of one overlay per line —
;; that is what makes it viable on large files, where
;; highlight-indent-guides collapsed (you already switched it off past
;; 2000 lines in +maybe-lighten-buffer-h).
;;
;; Doom's `:ui indent-guides' stays enabled: it already drives per-mode
;; activation. We only swap the rendering engine.
(use-package! indent-bars
  :defer t
  :hook ((python-mode python-ts-mode yaml-mode yaml-ts-mode
          json-mode json-ts-mode sh-mode bash-ts-mode) . indent-bars-mode)
  :config
  (setq indent-bars-treesit-support t
        ;; Subtle: we want peripheral help, not a grid.
        indent-bars-width-frac 0.15
        indent-bars-pad-frac 0.2
        indent-bars-color '(highlight :face-bg t :blend 0.22)
        indent-bars-highlight-current-depth '(:blend 0.6)
        ;; No bar on column 0: it doubles up with the line-number margin.
        indent-bars-starting-column nil
        indent-bars-display-on-blank-lines t)
  ;; Languages where indentation IS the syntax benefit from tree-sitter
  ;; support: the bar follows the real block, not the column.
  (setq indent-bars-treesit-wrap
        '((python argument_list parameters list list_comprehension
                  dictionary dictionary_comprehension parenthesized_expression
                  subscript))))

;; ── dape: the debugger, without the weight ──────────────────────────
;; Doom's `:tools debugger' pulls in dap-mode AND realgud for the same
;; service. dape speaks DAP directly, does not depend on lsp-mode, and
;; ships preconfigured adapters — including `debugpy' for Python.
;;
;; Key point for this workflow: the `debugpy' config simply runs
;; "python -m debugpy.adapter". And `my/python-apply-local-venv' already
;; sets `exec-path' and `PATH' BUFFER-LOCALLY to the project's .venv. So
;; "python" resolves to the right interpreter on its own, with nothing to
;; declare. debugpy just has to be in the project venv:
;;     uv add --dev debugpy        (or: uv pip install debugpy)
;; Otherwise dape says so plainly: "module debugpy is not installed".
(use-package! dape
  :defer t
  :commands (dape dape-breakpoint-toggle dape-breakpoint-remove-all)
  :init
  ;; Doom already reserves SPC d as the "debugger" prefix and leaves it
  ;; empty while no debugging module is active: we fill it.
  (setq dape-key-prefix nil)
  :config
  (setq dape-buffer-window-arrangement 'right  ; stack on the right, code on the left
        dape-info-hide-mode-line nil
        dape-inlay-hints t                     ; inline variable values
        dape-cwd-function #'my/project-root
        ;; Do not keep the compilation buffer around when it succeeds.
        dape-repl-use-shorthand t)

  ;; Breakpoints survive an Emacs restart.
  (dape-breakpoint-global-mode)
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  (add-hook 'doom-after-init-hook #'dape-breakpoint-load)

  ;; Pulse the current line on every step: without it you lose your place.
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  ;; Close the debug windows cleanly when the session ends.
  (add-hook 'dape-stopped-hook #'dape-info)
  (add-hook 'dape-on-stopped-hooks #'dape-info nil t))

(map! :leader
      (:prefix ("d" . "debugger")
       :desc "Start / continue"        "d" #'dape
       :desc "Toggle breakpoint"       "b" #'dape-breakpoint-toggle
       :desc "Log breakpoint"          "l" #'dape-breakpoint-log
       :desc "Conditional breakpoint"  "c" #'dape-breakpoint-expression
       :desc "Remove all breakpoints"  "B" #'dape-breakpoint-remove-all
       :desc "Step over"               "n" #'dape-next
       :desc "Step into"               "i" #'dape-step-in
       :desc "Step out"                "o" #'dape-step-out
       :desc "Continue"                "r" #'dape-continue
       :desc "Restart session"         "R" #'dape-restart
       :desc "Quit session"            "q" #'dape-quit
       :desc "Stack / info panes"      "s" #'dape-info
       :desc "REPL"                   "e" #'dape-repl
       :desc "Evaluate expression"     "E" #'dape-evaluate-expression))

;; ── eglot-booster: LSP JSON, pre-chewed ─────────────────────────────
;; emacs-lsp-booster sits between eglot and the server: it reads the
;; server's JSON and hands Emacs elisp bytecode instead. Emacs no longer
;; parses JSON in the completion loop — which is where most of the latency
;; went on a large project.
;;
;; STATUS: ENABLED, but something else had to be fixed first.
;;
;; First verdict, wrong: "incompatible". The server died instantly as soon
;; as the mode was on, while the exact same chain worked perfectly from
;; the command line.
;;
;; The real cause was elsewhere, and it is visible in the daemon log:
;; "File watching not possible, no file descriptor left".
;; `launchctl limit maxfiles' is 256 on macOS, and that is the limit
;; launchd passes to its children — zsh raises it for its own processes,
;; launchd does not. eglot + basedpyright register one file watcher per
;; project directory; on a decent-sized repo the 256 descriptors run out,
;; the server dies, and eglot gives up reconnecting. The booster was not
;; at fault: it merely added one more process, which crossed the threshold
;; sooner.
;;
;; Fixed in the doom-emacs LaunchAgent plist (~/Library/LaunchAgents):
;;   SoftResourceLimits > NumberOfFiles = 16384
;; Emacs now sees ~10000 descriptors instead of 256.
;;
;; Verified after the fix: the server's actual command line is
;;   emacs-lsp-booster --json-false-value :json-false -- \
;;     basedpyright-langserver --stdio
;; the advice on jsonrpc--json-read is installed, and the session holds
;; without a single descriptor error.
;;
;; Binary built as native arm64 into ~/.local/bin: the GitHub releases
;; only ship x86_64, and Rosetta would have eaten part of the gain.
(use-package! eglot-booster
  :after eglot
  :config
  (eglot-booster-mode)
  (setq eglot-booster-io-only nil))

;; ── basedpyright ────────────────────────────────────────────────────
;; The maintained fork of pyright: faster, stricter inference, and no
;; longer shipped through a pyenv shim (the old `pyright-langserver'
;; resolved to ~/.pyenv/shims/, adding indirection on every server start).
;; Installed with `uv tool install basedpyright' -> ~/.local/bin.
(after! eglot
  (setq eglot-server-programs
        (cl-remove-if (lambda (e)
                        (and (consp (car e))
                             (memq 'python-mode (car e))))
                      eglot-server-programs))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("basedpyright-langserver" "--stdio"))))

;; ── Animated half-page scrolling ────────────────────────────────────
;; No extra package. Emacs 29+ ships `pixel-scroll-precision-mode' (already
;; on above) and, with it, `pixel-scroll-precision-interpolate' — the same
;; pixel-accurate engine the trackpad uses. We simply route Evil's C-d /
;; C-u and PageUp / PageDown through it.
;;
;; Why this is safe for performance, and it is worth being precise: the
;; interpolation only runs while a scroll COMMAND is executing. It adds
;; nothing to redisplay, nothing to a cursor move, nothing to typing. The
;; only cost is the duration of the animation itself, and that duration is
;; a setting — 0.10s here, short enough to read as momentum rather than as
;; a wait.
;;
;; To turn it off: set `+smooth-scroll-time' to 0, or comment out the two
;; advices. Nothing else depends on this block.
(defvar +smooth-scroll-time 0.10
  "Duration of the half-page scroll animation, in seconds. 0 disables it.")

(setq pixel-scroll-precision-interpolate-page t
      pixel-scroll-precision-use-momentum t)

(defun +smooth-scroll-half (direction)
  "Scroll half a window height with interpolation.
DIRECTION is -1 to move the view down, +1 to move it up."
  (let ((pixel-scroll-precision-interpolation-total-time +smooth-scroll-time))
    (pixel-scroll-precision-interpolate
     (* direction (/ (window-text-height nil t) 2))
     nil 1)))

(defun +smooth-scroll-down-a (&rest _)
  "Animate `evil-scroll-down' instead of jumping."
  (+smooth-scroll-half -1))

(defun +smooth-scroll-up-a (&rest _)
  "Animate `evil-scroll-up' instead of jumping."
  (+smooth-scroll-half 1))

(after! evil
  ;; :override and not :before — otherwise the view would jump first and
  ;; then animate from the wrong place.
  (unless (zerop +smooth-scroll-time)
    (advice-add 'evil-scroll-down :override #'+smooth-scroll-down-a)
    (advice-add 'evil-scroll-up   :override #'+smooth-scroll-up-a)))

;; ── avy: jump anywhere on screen ────────────────────────────────────
;; avy is the maintained successor of ace-jump-mode (same idea, written by
;; the same community after ace-jump was abandoned in 2014). Doom already
;; wires it into Evil through the `g s' motions; what follows only tunes it.
;;
;; The defaults were already sane — home-row keys, `at-full' overlay style —
;; so there are exactly three things worth changing.
(after! avy
  ;; 1. THE important one. By default avy only labels candidates in the
  ;;    CURRENT window. With splits that is half the screen wasted: you see
  ;;    the target in the other window and cannot jump to it. Setting this
  ;;    makes `g a' reach anywhere on screen, and jumping moves the focus
  ;;    to that window — which is usually what you wanted anyway.
  (setq avy-all-windows t
        ;; ... but not across every frame: in daemon mode that would label
        ;; windows you cannot even see.
        avy-all-windows-alt nil

        ;; 2. One candidate, no label to read: go straight there.
        avy-single-candidate-jump t

        ;; 3. `g a' waits for you to stop typing before showing labels.
        ;;    0.5s is long enough to feel like a hesitation; 0.35 keeps the
        ;;    rhythm without firing between two fast keystrokes.
        avy-timeout-seconds 0.35))

;; The actions available WHILE the labels are on screen — press one of these
;; instead of a label, then pick the target. This is the part most people
;; never discover, and it is where avy stops being "a jump" and becomes an
;; operator:
;;   x  kill the target line          X  kill the target region
;;   t  teleport the line here        m  move the line here
;;   y  copy the target line          Y  copy the target region
;;   i  ignore (dismiss)              z  zap up to the target
;; They are avy's defaults; nothing to configure, only to remember.

(map! :leader
      (:prefix ("j" . "jump")
       :desc "Character (timed)"  "j" #'evil-avy-goto-char-timer
       :desc "Character (2 keys)" "c" #'evil-avy-goto-char-2
       :desc "Line"               "l" #'avy-goto-line
       :desc "Word"               "w" #'avy-goto-word-1
       :desc "Word (any window)"  "W" #'avy-goto-word-0
       :desc "End of word"        "e" #'avy-goto-word-0-below
       :desc "Whitespace end"     "SPC" #'avy-goto-whitespace-end
       :desc "Resume last jump"   "r" #'avy-resume
       :desc "Pop mark (go back)" "b" #'avy-pop-mark))

;; ── combobulate: move by syntax, not by characters ──────────────────
;; Enabled only on the modes whose tree-sitter grammar is actually compiled.
;; A `*-ts-mode' without its grammar never activates, so a hook there would
;; be dead weight — and until today only `python' was installed on this
;; machine, despite `+tree-sitter' being requested for javascript and web.
;;
;; Its own prefix `C-c o' is kept rather than moved under SPC: it is what
;; the package documents, which-key surfaces it, and SPC is already dense.
;; The hook goes through a wrapper rather than calling `combobulate-mode'
;; directly, and that detail is load-bearing. The package's autoload points
;; at `combobulate-setup', which defines the minor mode but NOT the
;; per-language procedures — those live in combobulate-python.el and
;; friends, pulled in by `combobulate.el'. Hooking the mode alone loaded
;; the shell and no language, so the mode refused to enable and `C-c o'
;; stayed unbound. Requiring the main feature first fixes it.
(defun +combobulate-maybe-enable-h ()
  "Load combobulate fully, then enable it in this buffer."
  (when (require 'combobulate nil t)
    (combobulate-mode 1)))

(use-package! combobulate
  :defer t
  :commands (combobulate combobulate-mode)
  :init
  (setq combobulate-key-prefix "C-c o")
  (dolist (h '(python-ts-mode-hook
               js-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook
               json-ts-mode-hook yaml-ts-mode-hook toml-ts-mode-hook
               css-ts-mode-hook html-ts-mode-hook))
    (add-hook h #'+combobulate-maybe-enable-h)))


;; ══════════════════════════════════════════════════════════════════════
;;  Improvements (2026-08-22)
;;  Each block carries the number of the recommendation it came from.
;; ══════════════════════════════════════════════════════════════════════

;; ── nº 4 · apheleia: format on save ─────────────────────────────────
;; Enabled by `(format +onsave)' in init.el. apheleia runs ASYNC and
;; applies a DIFF instead of replacing the buffer, so neither point nor
;; scroll position moves. That's what made format-all & co unbearable.
;;
;; Three settings here, each one guards something specific.
(after! apheleia
  ;; 1. Python -> ruff, not black.
  ;;    apheleia sends python to `black' by default. I format with ruff
  ;;    (alias `ruffc' = ruff check --fix + ruff format). The two are
  ;;    ALMOST identical, and almost means files would flip-flop between
  ;;    two formatters. `ruff-isort' sorts imports, then `ruff' formats.
  ;;
  ;;    Measured before enabling (ruff format --diff):
  ;;      active project    2 / 235 files would change
  ;;      other project     5 /  18
  ;;      old project      85 / 147   (never ran ruff)
  ;;    So basically no churn on the active project.
  (setf (alist-get 'python-mode    apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))

  ;; 2. NO formatting for shell scripts, on purpose. Doom adds
  ;;    `(sh-mode . shfmt)' to the alist. shfmt isn't even installed here,
  ;;    but the scripts in ~/.config/workflow-tools are hand-aligned with
  ;;    careful comment columns and shfmt would wreck them.
  ;;    nil = "no formatter", that's the documented value.
  (setf (alist-get 'sh-mode       apheleia-mode-alist) nil)
  (setf (alist-get 'bash-ts-mode  apheleia-mode-alist) nil)

  ;; 3. Same for TOML: the config.toml files (herdr, aerospace, alacritty)
  ;;    are hand-aligned. `taplo' isn't installed today, but the day it
  ;;    gets installed for something else it would quietly reformat them.
  (setf (alist-get 'conf-toml-mode apheleia-mode-alist) nil)
  (setf (alist-get 'toml-ts-mode   apheleia-mode-alist) nil))

;; JS/TS/web/JSON go through prettier, already installed. Checked before
;; enabling: on the active project 0 / 173 JS/TS files are off, so
;; formatting there is a non-event.
;;
;; Skip it once: C-u C-x C-s (prefix arg on save).
;; Skip a whole mode: add it to `+format-on-save-disabled-modes'
;; (Doom already puts LaTeX and SQL there).

;; ── nº 6 · diff-hl: git diff in the gutter ──────────────────────────
;; Enabled by `(vc-gutter +pretty +diff-hl)' in init.el.
;;
;; Doom ALREADY wires the keys when the module is on, no need to redo them:
;;   SPC g ]   next hunk           SPC g r   revert hunk
;;   SPC g [   previous hunk       SPC g s   stage hunk
;;   SPC t d   toggle diff-hl in this buffer
;;
;; We only add vim-gitgutter style navigation (checked free: Doom defines
;; no `] d' / `[ d') and a popup preview of the hunk.
(map! :when (modulep! :ui vc-gutter)
      :n "] d" #'+vc-gutter/next-hunk
      :n "[ d" #'+vc-gutter/previous-hunk
      :n "g H" #'diff-hl-show-hunk)

;; Cost: diff-hl queries git async since Emacs 28. Measured on my repos:
;; `git status' takes 0.06s on the biggest (519 files) and 0.01s
;; elsewhere. Not noticeable.

;; ── nº 7 · evil-textobj-tree-sitter: syntax text objects ────────────
;; The missing link between tree-sitter and Evil. combobulate does
;; navigation and editing, it does NOT provide Evil text objects.
;;
;; Checked before installing: the package handles Emacs 30's NATIVE
;; `treesit' (variable `evil-textobj-tree-sitter--can-use-builtin-treesit')
;; and ships a treesit-queries/ dir, so it works with python-ts-mode,
;; js-ts-mode & co, not just the old tree-sitter.el.
;;
;; What you get. `a' = with the wrapper, `i' = just the inside, like
;; everywhere else in Evil:
;;
;;   af / if   function        vaf  daf  caf  yaf ...
;;   ac / ic   class
;;   aa / ia   argument (parameter)
;;   al / il   loop
;;   ai / ii   conditional block
;;   ak / ik   comment
;;
;; and the jumps:  ] f / [ f   next / previous function
;;                 ] c / [ c   next / previous class
(use-package! evil-textobj-tree-sitter
  :after evil
  :config
  ;; Spelled out on purpose, not sloppiness:
  ;; `evil-textobj-tree-sitter-get-textobj' is a MACRO (checked in its
  ;; autoloads, the form ends with `nil t'). A loop passing it a variable
  ;; hands over the SYMBOL, not the string, which gives a
  ;; (wrong-type-argument sequencep outer) that silently aborts loading
  ;; the rest of this file. The group name has to be a literal.
  ;;
  ;; Capture names come from the queries shipped with the package
  ;; (treesit-queries/python/textobjects.scm): function, class, parameter,
  ;; loop, conditional, comment, test, entry, each with .outer and .inner.
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
  (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
  (define-key evil-outer-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-outer-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
  (define-key evil-inner-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
  (define-key evil-outer-text-objects-map "k" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-inner-text-objects-map "k" (evil-textobj-tree-sitter-get-textobj "comment.inner"))

  ;; Function / class jumps. `] f' and `[ f' clash with nothing: Doom only
  ;; defines 5 bindings starting with `]', none on f or c (checked in
  ;; +evil-bindings.el).
  ;;
  ;; Real signature, checked in the package autoloads:
  ;;   (evil-textobj-tree-sitter-goto-textobj GROUP &optional END PREVIOUS QUERY)
  ;; PREVIOUS is the THIRD arg. Passing `t' second would jump to the END
  ;; of the current function, not to the previous one.
  (map! :n "] f" (cmd! (evil-textobj-tree-sitter-goto-textobj "function.outer"))
        :n "[ f" (cmd! (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t))
        :n "] c" (cmd! (evil-textobj-tree-sitter-goto-textobj "class.outer"))
        :n "[ c" (cmd! (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t))))

;; ── nº 8 · outli: fold my own config files ──────────────────────────
;; By jdtsmith, same author as eglot-booster and indent-bars, both
;; already used here.
;;
;; The key point: by default outli expects `;;; Title' headings (stem
;; ";;" + repeated ";"). My convention is `;; ── title ──────', which it
;; wouldn't see. So we reconfigure the stem and repeat char for the real
;; convention, across the three file families involved.
;;
;; The resulting regexp is  \(;; ─+ \)  (tested on real lines of this
;; config before writing the block):
;;   ";; ── modeline ─────────"     -> heading
;;   ";; ══════════════════════"    -> ignored  (boxes stay boxes)
;;   ";;  Modern IDE setup (2026"   -> ignored
;;   ";; plain comment"             -> ignored
(use-package! outli
  :defer t
  :hook ((emacs-lisp-mode sh-mode bash-ts-mode conf-mode
          conf-toml-mode toml-ts-mode
          python-mode python-ts-mode yaml-mode yaml-ts-mode) . outli-mode)
  :init
  (setq outli-heading-config
        '((emacs-lisp-mode ";; " ?─ t)
          (lisp-data-mode  ";; " ?─ t)
          (python-mode     "# "  ?─ t)
          (python-ts-mode  "# "  ?─ t)
          (sh-mode         "# "  ?─ t)
          (bash-ts-mode    "# "  ?─ t)
          (conf-mode       "# "  ?─ t)
          (conf-toml-mode  "# "  ?─ t)
          (toml-ts-mode    "# "  ?─ t)
          (yaml-mode       "# "  ?─ t)
          (yaml-ts-mode    "# "  ?─ t)
          ;; org already handles its own structure: `. nil' turns outli
          ;; off in modes derived from it. That's outli's own default, we
          ;; keep it since we rebuild the alist.
          (org-mode . nil)
          ;; Fallback for everything else: outli's native behaviour.
          (t (let* ((c (or comment-start "#"))
                    (space (unless (eq (aref c (1- (length c))) ?\s) " ")))
               (concat c space))
             ?*))))

;; NO TAB rebinding here, on purpose. outli turns on
;; `outline-minor-mode', and `evil-fold-list' (evil-vars.el:1841) already
;; knows it, so Evil's native fold commands just work on these headings:
;;   za  toggle      zc  close        zo  open
;;   zm  close all                 zr  open all
;; Rebinding TAB would have shadowed Doom's in all those buffers for no
;; gain.

;; ── nº 12 · casual: transient menus for the stuff you forget ────────
;; The point isn't speed, it's DISCOVERABILITY: no need to open the dired
;; docs, press one key and see everything.
;;
;; Prefix `SPC =': checked free in +evil-bindings.el (0 hits).
;; We touch neither `C-o' (= evil-jump-backward, want to keep it) nor
;; mode keymaps, to avoid any clash with evil-collection.
(map! :leader
      (:prefix ("=" . "casual")
       :desc "Menu du mode courant" "=" #'casual-editkit-main-tmenu
       :desc "dired"        "d" #'casual-dired-tmenu
       :desc "isearch"      "s" #'casual-isearch-tmenu
       :desc "ibuffer"      "b" #'casual-ibuffer-tmenu
       :desc "calc"         "c" #'casual-calc-tmenu
       :desc "Info"         "i" #'casual-info-tmenu
       :desc "re-builder"   "r" #'casual-re-builder-tmenu
       :desc "bookmarks"    "m" #'casual-bookmarks-tmenu
       :desc "make"         "k" #'casual-make-tmenu
       :desc "compile"      "C" #'casual-compile-tmenu
       :desc "ediff"        "e" #'casual-ediff-tmenu
       :desc "org"          "o" #'casual-org-tmenu
       :desc "agenda"       "a" #'casual-agenda-tmenu
       :desc "help"         "h" #'casual-help-tmenu
       :desc "man"          "M" #'casual-man-tmenu))

;; ── nº 14 · numpydoc: installed forever, never wired up ─────────────
;; `(package! numpydoc)' sat in packages.el forever and showed up NOWHERE
;; in config.el. It generates a numpy docstring skeleton from the
;; signature of the function at point.
(use-package! numpydoc
  :defer t
  :commands (numpydoc-generate)
  :config
  ;; 'prompt asks questions in the minibuffer, nil just writes the
  ;; skeleton to fill in. Skeleton it is: less intrusive, and you fill it
  ;; in the buffer with completion and copilot at hand.
  (setq numpydoc-insertion-style nil
        numpydoc-insert-examples-block nil
        numpydoc-template-short "FIXME: courte description."))

(map! :after python
      :map (python-mode-map python-ts-mode-map)
      :localleader
      :desc "Docstring numpy" "d" #'numpydoc-generate)
