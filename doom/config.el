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
;;  Workflow: SPC l l lists problems, RET on one opens the code, the
;;  description and the tests side by side. SPC l t runs the tests,
;;  SPC l s submits. Solutions are written to leetcode-challenges/ so they
;;  are version-controlled and can be reviewed later.
;; ══════════════════════════════════════════════════════════════════════
(use-package! leetcode
  :defer t
  :commands (leetcode leetcode-daily leetcode-refresh)
  :init
  (setq leetcode-prefer-language "python3"
        leetcode-prefer-sql "mysql"
        leetcode-save-solutions t
        leetcode-directory "~/Desktop/projects/leetcode-challenges/solutions")
  :config
  ;; Windows: description on the left, code on the right, results below.
  (setq leetcode-path-operation-alist
        '(("python3" . python-ts-mode)
          ("go"      . go-mode)
          ("rust"    . rust-mode)))
  (map! :map leetcode--problems-mode-map
        :n "q" #'quit-window
        :n "r" #'leetcode-refresh))

(map! :leader
      (:prefix ("l" . "leetcode")
       :desc "Problem list"  "l" #'leetcode
       :desc "Daily problem" "d" #'leetcode-daily
       :desc "Refresh"       "r" #'leetcode-refresh
       :desc "Run tests"     "t" #'leetcode-try
       :desc "Submit"        "s" #'leetcode-submit
       :desc "Quit"          "q" #'leetcode-quit))

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
