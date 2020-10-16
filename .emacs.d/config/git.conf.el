;; this file contains all the configuration
;; related to git settings

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq version-control t)
(setq delete-old-versions -1)
(setq vc-make-backup-files t)

;; magit config
(use-package magit
  :defer t
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package magit-todos
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t)

;; undo-tree - visualize your undos and branches
(use-package undo-tree
  :ensure t
  :defer t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package loc-changes
  :ensure t)

;; Diff-hl - Highlight changed lines
(use-package diff-hl
  :ensure t
  :config
  ;; (global-diff-hl-mode)
  ;; (unless (display-graphic-p)
  ;;   (setq diff-hl-side 'left)
  ;;   (diff-hl-margin-mode))
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

;; git gutter, to show diffs
;; deactivated for now cause diff-hli is used
;; (use-package git-gutter
;;   ;; show diff hunks in gutter + stage/unstage from buffer
;;   :ensure t
;;   :diminish git-gutter-mode
;;   :config
;;   (progn
;;     (bind-keys
;;      ("C-x C-g C-n" . git-gutter:next-hunk)
;;      ("C-x C-g C-p" . git-gutter:previous-hunk)
;;      ("C-x C-g C-s" . git-gutter:stage-hunk)
;;      ("C-x C-g C-r" . git-gutter:revert-hunk))
;;     (global-git-gutter-mode))
;;   )

(use-package projectile
  :ensure t
  :defer t
  :config
  ;; activate projectile mode only if available; otherwise nothing
  (setq projectile-mode-line
        '(:eval (if (projectile-project-p)
                    (projectile-mode 1)
                  ""))))


(use-package treemacs-magit
  :ensure t
  :defer t
  :after (treemacs magit))

(use-package treemacs-projectile
  :ensure t
  :defer t
  :after (treemacs projectile))
