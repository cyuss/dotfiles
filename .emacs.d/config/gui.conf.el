;; this file contains all the configuration
;; related to display settings

;; save the customization and theme settings in other file
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; visualization
(global-visual-line-mode t)

;; change the yes or no answer to y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; set font
(set-frame-font "Monaco-13")

;; change the line spacing for better visualization
(setq-default line-spacing 5)

;; prettify symbols
(global-prettify-symbols-mode +1)

;; start up buffers
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; frames configuration
(tool-bar-mode -1)
(display-time-mode 1)
(menu-bar-mode -99)
(scroll-bar-mode -1)
(display-battery-mode 1)

;; encoding system
(set-language-environment "UTF-8")
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces. Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1))
  )

;; disable flymake mode for flycheck
(flymake-mode -1)

;; misc
(show-paren-mode t)

;; define highlight-parentheses-mode
;; (define-globalized-minor-mode global-highlight-parentheses-mode
;;   highlight-parentheses-mode
;;   (lambda ()
;;     (highlight-parentheses-mode t)))
;; (global-highlight-parentheses-mode t)
(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :commands highlight-parentheses-mode
  )

;; show column number
(setq-default column-number-mode t)

(use-package all-the-icons
  :ensure t)

;; modeline settings
;; (use-package spaceline-all-the-icons
;;   :ensure t
;;   :config
;;   (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
;;   (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
;;   (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
;;   (spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
;;   (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
;;   )

;; spaceline configuration
;; (use-package spaceline
;;   :ensure t
;;   :config
;;   (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
;;   (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

;; (use-package spaceline-config
;;   :ensure spaceline
;;   :config
;;   (spaceline-helm-mode 1)
;;   (spaceline-emacs-theme))

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (set-face-foreground 'doom-modeline-evil-emacs-state "SkyBlue2")
  (set-face-foreground 'doom-modeline-evil-insert-state "chartreuse3")
  (set-face-foreground 'doom-modeline-evil-motion-state "plum3")
  (set-face-foreground 'doom-modeline-evil-normal-state "DarkGoldenrod2")
  (set-face-foreground 'doom-modeline-evil-operator-state "DarkGoldenrod2")
  (set-face-foreground 'doom-modeline-evil-visual-state "gray")
  (set-face-foreground 'doom-modeline-evil-replace-state "chocolate")

  (doom-modeline-def-segment latex-compile
    "For indications on auctex compilation"
    (if compilation-in-progress
        (propertize "Compiling..." 'face 'doom-modeline-info)
      ""))

  (doom-modeline-def-segment minor-modes
    "For indications on minor modes"
    (mapconcat (lambda (mm) mm)
               (split-string (format-mode-line minor-mode-alist))
               " "))

  ;; (doom-modeline-def-modeline 'main
  ;;                             '(workspace-number window-number bar
  ;;                               evil-state matches " " buffer-info
  ;;                               buffer-position " " selection-info
  ;;                               latex-compile)
  ;;                             '(global buffer-encoding major-mode
  ;;                               process vcs flycheck))
  (add-hook 'after-init-hook 'doom-modeline-init))

;; Highlight todo and fixme in comments
;; (package-require 'fic-ext-mode)
;; (defun add-something-to-mode-hooks (mode-list something)
;;   "helper function to add a callback to multiple hooks"
;;   (dolist (mode mode-list)
;;     (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))

;; (add-something-to-mode-hooks '(c++ tcl emacs-lisp python text markdown latex) 'fic-ext-mode)

;; add todo, fixme, etc highlighting in comments and strings
;; (use-package fic-ext-mode
;;   :ensure t
;;   :config
;;   (defun add-something-to-mode-hooks (mode-list something)
;;     "helper function to add a callback to multiple hooks"
;;     (dolist (mode mode-list)
;;       (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
;;   (add-something-to-mode-hooks '(c++ tcl emacs-lisp python text markdown latex) 'fic-ext-mode))

;; highlight and manage comment tags like TODO, BUG, FIXME, etc
(use-package comment-tags
  :ensure t
  :config
  (autoload 'comment-tags-mode "comment-tags-mode")
  (setq comment-tags-keymap-prefix (kbd "C-c t"))
  (with-eval-after-load "comment-tags"
    (setq comment-tags-keyword-faces
          `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
            ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
            ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
            ("HACK" . ,(list :weight 'bold :foreground "#E8B71A"))
            ("KLUDGE" . ,(list :weight 'bold :foreground "#E8B71A"))
            ("XXX" . ,(list :weight 'bold :foreground "#F7EAC8"))
            ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
            ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A"))))
    (setq comment-tags-comment-start-only t
          comment-tags-require-colon t
          comment-tags-case-sensitive t
          comment-tags-show-faces t
          comment-tags-lighter nil))
  (add-hook 'prog-mode-hook 'comment-tags-mode))

;; highlight multiple occurences
(use-package highlight-symbol
  :ensure t
  :bind (("M-p" . highlight-symbol-prev)
         ("M-n" . highlight-symbol-next)
         ("M-'" . highlight-symbol-query-replace))
  :init
  (defun highlight-symbol-first ()
    "Jump to the first location of symbol at point."
    (interactive)
    (push-mark)
    (eval
     `(progn
        (goto-char (point-min))
        (search-forward-regexp
         (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
         nil t)
        (beginning-of-thing 'symbol))))

  (defun highlight-symbol-last ()
    "Jump to the last location of symbol at point."
    (interactive)
    (push-mark)
    (eval
     `(progn
        (goto-char (point-max))
        (search-backward-regexp
         (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
         nil t))))

  (bind-keys ("M-P" . highlight-symbol-first)
             ("M-N" . highlight-symbol-last)))

(use-package rainbow-identifiers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
  ;; configuration
  (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face))

(use-package rainbow-delimiters
  :ensure t)

;; set theme activated only in gui mode
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

;; dashboard package
;; TODO: resize dashboard banner
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((bookmarks . 5)
			  (recents  . 3)
                          (agenda . 4)
			  (projects . 3)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

;; display a topstories of hackernews on dashboard
;; (use-package dashboard-hackernews
;;   :ensure t
;;   :config
;;   (setq dashboard-items '((hackernews . 5))))

;; ;; theme
;; (use-package spacemacs-theme
;;   :ensure t)

(use-package color-identifiers-mode
  :ensure t
  :defer t)

;; show ~ at the end of the file for empty lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
(progn
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))

;; electric indentation
(electric-indent-mode 1)

;; autopair mode
;; (require 'autopair)
;; (autopair-global-mode 1)
(use-package autopair
  ;; diminish autopair mode to make a spacy mode line
  :diminish autopair-mode
  :ensure t
  :config
  (autopair-global-mode 1)
  )

;; highlight current line
;; (global-hl-line-mode +1)
(use-package hl-line
  ;; visible current line
  :ensure t
  :diminish global-hl-line-mode
  :config
  (global-hl-line-mode))

;; show the found items in mode line
(use-package anzu
  :init (global-anzu-mode 1)
  :diminish anzu-mode
  :ensure t)

(use-package scratch
  :ensure t)

(use-package zoom-window
  :ensure t
  :config
  (global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
  (custom-set-variables '(zoom-window-mode-line-color "DarkGreen")))
