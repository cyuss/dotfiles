;;; premium-noir-theme.el --- port Emacs du theme Alacritty "Premium Noir" -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Source : ~/.config/alacritty/themes/premium-noir.toml
;; Base graphite tres sombre, accents desatures, curseur ambre.
;; Concu pour un fond transparent (Alacritty opacity 0.70).
;;
;;; Code:

(require 'doom-themes)

(defgroup premium-noir-theme nil
  "Options du theme premium-noir."
  :group 'doom-themes)

(defcustom premium-noir-brighter-modeline nil
  "Si non-nil, modeline plus contrastee."
  :group 'premium-noir-theme
  :type 'boolean)

(defcustom premium-noir-brighter-comments nil
  "Si non-nil, commentaires plus lumineux."
  :group 'premium-noir-theme
  :type 'boolean)

(defcustom premium-noir-padded-modeline nil
  "Si non-nil, ajoute 4px de padding a la modeline."
  :group 'premium-noir-theme
  :type '(or integer boolean))

(def-doom-theme premium-noir
  "Graphite sombre, accents desatures, ambre chaud. Jumeau du terminal."

  ;; name        default   256       16
  ((bg         '("#101216" "#101216" "black"        ))
   (bg-alt     '("#0b0d11" "#0b0d11" "black"        ))
   (base0      '("#0b0d11" "#0b0d11" "black"        ))
   (base1      '("#15181e" "#15181e" "brightblack"  ))
   (base2      '("#1b1e25" "#1b1e25" "brightblack"  ))
   (base3      '("#22262e" "#22262e" "brightblack"  ))
   (base4      '("#2b303b" "#2b303b" "brightblack"  ))
   (base5      '("#5b6272" "#5b6272" "brightblack"  ))
   (base6      '("#7d8494" "#7d8494" "brightblack"  ))
   (base7      '("#a9aeb9" "#a9aeb9" "brightblack"  ))
   (base8      '("#eef1f6" "#eef1f6" "white"        ))
   (fg         '("#d6dae1" "#d6dae1" "white"        ))
   (fg-alt     '("#eef1f6" "#eef1f6" "brightwhite"  ))

   (grey       base4)
   (red        '("#d2696b" "#d2696b" "red"          ))
   (orange     '("#e0956a" "#e0956a" "brightred"    ))
   (green      '("#8fb884" "#8fb884" "green"        ))
   (teal       '("#79b3b6" "#79b3b6" "brightgreen"  ))
   (yellow     '("#d8ab6d" "#d8ab6d" "yellow"       ))
   (blue       '("#7fa5cf" "#7fa5cf" "brightblue"   ))
   (dark-blue  '("#617e9e" "#617e9e" "blue"         ))
   (magenta    '("#ab8cc8" "#ab8cc8" "magenta"      ))
   (violet     '("#c4a6dd" "#c4a6dd" "brightmagenta"))
   (cyan       '("#96cbcd" "#96cbcd" "brightcyan"   ))
   (dark-cyan  '("#5c888a" "#5c888a" "cyan"         ))
   ;; accent maison : le curseur du terminal
   (amber      '("#e3b778" "#e3b778" "yellow"       ))

   ;; face categories -- obligatoires
   (highlight      amber)
   (vertical-bar   base2)
   (selection      base4)
   (builtin        magenta)
   (comments       (if premium-noir-brighter-comments base6 base5))
   (doc-comments   (doom-lighten (if premium-noir-brighter-comments base6 base5) 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       magenta)
   (methods        cyan)
   (operators      teal)
   (type           yellow)
   (strings        green)
   (variables      fg)
   (numbers        orange)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    amber)
   (vc-added       green)
   (vc-deleted     red)

   ;; categories maison
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright premium-noir-brighter-modeline)
   (-modeline-pad
    (when premium-noir-padded-modeline
      (if (integerp premium-noir-padded-modeline) premium-noir-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt base6)
   (modeline-bg (if -modeline-bright base3 base1))
   (modeline-bg-l (if -modeline-bright base3 base2))
   (modeline-bg-inactive   base0)
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))

  ;; --- Faces ------------------------------
  (
   ;; curseur ambre, comme dans Alacritty
   (cursor :background amber :foreground bg)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground amber :weight 'bold)

   (font-lock-comment-face :foreground comments :slant 'italic)
   (font-lock-doc-face :inherit 'font-lock-comment-face :foreground doc-comments :slant 'italic)

   ;;; modeline
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)
   (mode-line-buffer-id :foreground highlight :weight 'bold)

   ;;; doom-modeline
   (doom-modeline-bar :background highlight)
   (doom-modeline-buffer-path :foreground base6 :weight 'normal)
   (doom-modeline-buffer-file :foreground fg-alt :weight 'bold)
   (doom-modeline-buffer-modified :foreground amber :weight 'bold)
   (doom-modeline-project-dir :foreground teal :weight 'bold)
   (doom-modeline-info :foreground green)
   (doom-modeline-warning :foreground yellow)
   (doom-modeline-urgent :foreground red)

   ;;; solaire — panneaux legerement decroches
   (solaire-default-face :background base1)
   (solaire-hl-line-face :background base2)
   (solaire-mode-line-face
    :inherit 'mode-line :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;;; selection / recherche — memes teintes que le terminal
   (region :background base4 :distant-foreground 'unspecified)
   (isearch :background amber :foreground bg :weight 'bold)
   (lazy-highlight :background base6 :foreground bg)
   (highlight :background amber :foreground bg)

   ;;; vertico / corfu / orderless
   (vertico-current :background base3 :extend t)
   (corfu-default :background base1)
   (corfu-current :background base3)
   (corfu-border :background base4)
   (orderless-match-face-0 :foreground amber :weight 'bold)
   (orderless-match-face-1 :foreground magenta :weight 'bold)
   (orderless-match-face-2 :foreground teal :weight 'bold)
   (orderless-match-face-3 :foreground green :weight 'bold)
   (marginalia-documentation :foreground comments :slant 'italic)

   ;;; parentheses
   (rainbow-delimiters-depth-1-face :foreground fg)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground magenta)
   (rainbow-delimiters-depth-4-face :foreground teal)
   (rainbow-delimiters-depth-5-face :foreground amber)
   (rainbow-delimiters-depth-6-face :foreground green)
   (rainbow-delimiters-depth-7-face :foreground violet)
   (show-paren-match :foreground amber :background base4 :weight 'bold)

   ;;; magit / diff
   (magit-section-heading :foreground amber :weight 'bold)
   (magit-branch-local    :foreground teal)
   (magit-branch-remote   :foreground orange)
   (magit-diff-added        :foreground (doom-darken green 0.2)  :background (doom-blend green bg 0.12))
   (magit-diff-added-highlight :foreground green                 :background (doom-blend green bg 0.2))
   (magit-diff-removed      :foreground (doom-darken red 0.2)    :background (doom-blend red bg 0.12))
   (magit-diff-removed-highlight :foreground red                 :background (doom-blend red bg 0.2))
   (magit-diff-hunk-heading :background base3 :foreground base7)
   (magit-diff-hunk-heading-highlight :background base4 :foreground fg-alt)

   ;;; treemacs / dirvish
   (treemacs-root-face :foreground amber :weight 'ultra-bold :height 1.15)
   (treemacs-directory-face :foreground blue)
   (treemacs-file-face :foreground fg)
   (treemacs-git-modified-face :foreground amber)

   ;;; flymake / eglot
   (flymake-error   :underline `(:style wave :color ,red))
   (flymake-warning :underline `(:style wave :color ,yellow))
   (flymake-note    :underline `(:style wave :color ,teal))
   (eglot-highlight-symbol-face :background base3 :weight 'bold)

   ;;; markdown / org
   (markdown-code-face :background base1 :extend t)
   (markdown-header-face-1 :foreground amber   :weight 'bold :height 1.2)
   (markdown-header-face-2 :foreground magenta :weight 'bold :height 1.1)
   (markdown-header-face-3 :foreground blue    :weight 'bold)
   (org-block :background base1 :extend t)
   (org-block-begin-line :background base1 :foreground comments :extend t)
   (org-level-1 :foreground amber   :weight 'bold :height 1.2)
   (org-level-2 :foreground magenta :weight 'bold :height 1.1)
   (org-level-3 :foreground blue    :weight 'bold)
   (org-level-4 :foreground teal)
   (org-todo :foreground amber :weight 'bold)
   (org-done :foreground base5 :weight 'bold)
   (org-headline-done :foreground base5)

   ;;; hl-todo
   (hl-todo :foreground amber :weight 'bold))

  ;; --- Variables --------------------------
  ())

(provide-theme 'premium-noir)
;;; premium-noir-theme.el ends here
