;;; horizon-noir-theme.el --- doom-horizon on the terminal's background -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; doom-horizon, unchanged, except for the dark end of its palette:
;; the background and the four darkest steps now match
;; ~/.config/alacritty/themes/premium-noir.toml.
;;
;;   bg      #232530 -> #101216   (the terminal's own background)
;;   bg-alt  #1c1e26 -> #0b0d11
;;   base0-3 shifted onto the same graphite ramp, so hl-line, the
;;           modeline, the vertical bar and the region keep the exact
;;           contrast steps the theme was designed with.
;;
;; Every accent (the pinks, corals and violets that make Horizon what it
;; is) is untouched.
;;
;; Transparency is NOT set here: it belongs to the frame, not the theme.
;; `+transparency' in config.el drives `alpha-background', which macOS
;; composites for free — see the Transparency section there.
;;
;;
;; Added: January 21, 2020 (#374)
;; Author: karetsu <https://github.com/karetsu>
;; Maintainer:
;; Source: https://github.com/aodhneine/horizon-theme.el
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup horizon-noir-theme nil
  "Options for the `horizon-noir' theme."
  :group 'doom-themes)

(defcustom horizon-noir-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'horizon-noir-theme
  :type 'boolean)

(defcustom horizon-noir-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'horizon-noir-theme
  :type 'boolean)

(defcustom horizon-noir-comment-bg horizon-noir-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their legibility."
  :group 'horizon-noir-theme
  :type 'boolean)

(defcustom horizon-noir-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'horizon-noir-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme horizon-noir
  "A port of the port of the Visual Studio Code theme Horizon"

  ;; name       default    256       16
  ((bg         '("#101216" nil       nil            ))   ; = Alacritty premium-noir
   (bg-alt     '("#0b0d11" nil       nil            ))
   (base0      '("#0b0d11" "black"   "black"        ))
   (base1      '("#15181e" "#111111" "brightblack"  ))
   (base2      '("#1b1e25" "#333333" "brightblack"  ))
   (base3      '("#22262e" "#555555" "white"        ))
   (base4      '("#6a6a6a" "#6a6a6a" "white"        ))
   (base5      '("#f9cec3" "#f9cec3" "white"        ))
   (base6      '("#f9cbbe" "#f9cbbe" "white"        ))
   (base7      '("#fadad1" "#fadad1" "white"        ))
   (base8      '("#fdf0ed" "#fdf0ed" "white"        ))
   (fg-alt     '("#fdf0ed" "#fdf0ed" "brightwhite"  ))
   (fg         '("#c7c9cb" "#c7c9cb" "white"        ))

   (grey       base4)
   (red        '("#e95678" "#e95678" "red"          ))
   (orange     '("#f09383" "#f09383" "brightred"    ))
   (green      '("#09f7a0" "#09f7a0" "green"        ))
   (teal       '("#87ceeb" "#87ceeb" "brightgreen"  ))
   (yellow     '("#fab795" "#fab795" "yellow"       ))
   (blue       '("#21bfc2" "#21bfc2" "brightblue"   ))
   (dark-blue  '("#25b2bc" "#25b2bc" "blue"         ))
   (magenta    '("#6c6f93" "#6c6f93" "magenta"      ))
   (violet     '("#b877db" "#b877db" "brightmagenta"))
   (cyan       '("#59e3e3" "#59e3e3" "brightcyan"   ))
   (dark-cyan  '("#27d797" "#27d797" "cyan"   ))

   ;; additional highlighting colours for horizon
   (hor-highlight  `(,(doom-lighten (car base3) 0.05) ,@(cdr base2)))
   (hor-highlight-selected (doom-lighten base3 0.1))
   (hor-highlight-bright (doom-lighten base3 0.2))
   (hor-highlight-brighter (doom-lighten base3 0.5))

   ;; face categories -- required for all themes
   (highlight      red)
   (vertical-bar   base0)
   (selection      violet)
   (builtin        violet)
   (comments       (if horizon-noir-brighter-comments magenta hor-highlight-bright))
   (doc-comments   yellow)
   (constants      orange)
   (functions      teal)
   (keywords       violet)
   (methods        magenta)
   (operators      teal)
   (type           teal)
   (strings        yellow)
   (variables      red)
   (numbers        orange)
   (region         hor-highlight)
   (error          red)
   (warning        dark-cyan)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)


   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright horizon-noir-brighter-modeline)
   (-modeline-pad
    (when horizon-noir-padded-modeline
      (if (integerp horizon-noir-padded-modeline) horizon-noir-padded-modeline 4)))

   (modeline-fg     `(,(doom-darken (car fg) 0.2) ,@(cdr fg-alt)))
   (modeline-fg-alt `(,(doom-lighten (car bg) 0.2) ,@(cdr base3)))

   (modeline-bg (if -modeline-bright base4 base1))
   (modeline-bg-inactive base1))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :slant 'italic
    :background (if horizon-noir-comment-bg (doom-lighten bg 0.03) 'unspecified))
   (fringe :background bg)
   (link :foreground yellow :inherit 'underline)
   ((line-number &override) :foreground hor-highlight-selected)
   ((line-number-current-line &override) :foreground hor-highlight-brighter)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   (mode-line-highlight :background base1 :foreground fg)
   (tooltip :background base0 :foreground fg)

   ;;;; company
   (company-box-background    :background base0 :foreground fg)
   (company-tooltip-common    :foreground red :weight 'bold)
   (company-tooltip-selection :background hor-highlight :foreground fg)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground violet)
   (css-property             :foreground fg)
   (css-selector             :foreground red)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-highlight :foreground (doom-lighten bg 0.3))
   (doom-modeline-project-dir :foreground red :inherit 'bold )
   (doom-modeline-buffer-path :foreground red)
   (doom-modeline-buffer-file :foreground fg)
   (doom-modeline-buffer-modified :foreground violet)
   (doom-modeline-panel :background base1)
   (doom-modeline-urgent :foreground modeline-fg)
   (doom-modeline-info :foreground cyan)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; evil
   (evil-ex-search          :background hor-highlight-selected :foreground fg)
   (evil-ex-lazy-highlight  :background hor-highlight :foreground fg)
   ;;;; haskell-mode
   (haskell-type-face :foreground violet)
   (haskell-constructor-face :foreground yellow)
   (haskell-operator-face :foreground fg)
   (haskell-literate-comment-face :foreground hor-highlight-selected)
   ;;;; ivy
   (ivy-current-match       :background hor-highlight :distant-foreground nil)
   (ivy-posframe-cursor     :background red :foreground base0)
   (ivy-minibuffer-match-face-2 :foreground red :weight 'bold)
   ;;;; js2-mode
   (js2-object-property        :foreground red)
   ;;;; markdown-mode
   (markdown-markup-face           :foreground cyan)
   (markdown-link-face             :foreground orange)
   (markdown-link-title-face       :foreground yellow)
   (markdown-header-face           :foreground red :inherit 'bold)
   (markdown-header-delimiter-face :foreground red :inherit 'bold)
   (markdown-language-keyword-face :foreground orange)
   (markdown-markup-face           :foreground fg)
   (markdown-bold-face             :foreground violet)
   (markdown-table-face            :foreground fg :background base1)
   ((markdown-code-face &override) :foreground orange :background base1)
   ;;;; orderless
   (orderless-match-face-1 :weight 'bold :foreground (doom-blend red fg 0.6) :background (doom-blend red bg 0.1))
   ;;;; mic-paren
   (paren-face-match    :foreground green   :background base0 :weight 'ultra-bold)
   (paren-face-mismatch :foreground yellow :background base0   :weight 'ultra-bold)
   (paren-face-no-match :inherit 'paren-face-mismatch :weight 'ultra-bold)
   ;;;; magit
   (magit-section-heading :foreground red)
   (magit-branch-remote   :foreground orange)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground blue :background nil)
   ;;;; org <built-in>
   ((org-block &override) :background base1)
   ((org-block-begin-line &override) :background base1 :foreground comments)
   (org-hide :foreground hidden)
   (org-link :inherit 'underline :foreground yellow)
   (org-agenda-done :foreground cyan)
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-tag-bracket-face :foreground red)
   (rjsx-attr :foreground cyan :slant 'italic :weight 'medium)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   ;;;; treemacs
   (treemacs-root-face :foreground fg :weight 'bold :height 1.2)
   (doom-themes-treemacs-root-face :foreground fg :weight 'ultra-bold :height 1.2)
   (doom-themes-treemacs-file-face :foreground fg)
   (treemacs-directory-face :foreground fg)
   (treemacs-git-modified-face :foreground green)
   ;;;; web-mode
   (web-mode-html-tag-bracket-face :foreground red)
   (web-mode-html-tag-face         :foreground red)
   (web-mode-html-attr-name-face   :foreground orange)))

;;; horizon-noir-theme.el ends here
