;; this file contains all the configuration
;; related to navigation settings

;; set the keys - invert between the command and option keys for meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; if you close a buffer, it remembers where you were in the file
;; (setq-default save-place t)
;; (setq save-place-file (expand-file-name ".places" user-emacs-directory))
(save-place-mode 1)

;; every time when neotree window is opened, let it find current file and jump to node
;;(setq neo-smart-open t)
;; install the fonts from https://github.com/domtronn/all-the-icons.el/tree/master/fonts
(use-package neotree
  :ensure t
  :defer t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t))

;; treemacs configuration
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-indentation                   2
          treemacs-show-cursor                   t
          treemacs-no-png-images                 nil
          treemacs-show-hidden-files             t
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-width                         30
          treemacs-eldoc-display                 t
          treemacs-no-delete-other-windows       t)
	(treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
	(require 'treemacs-all-the-icons)
	(treemacs-load-theme "all-the-icons")))

(use-package treemacs-all-the-icons
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ;; ("C-c C-<" . mc/mark-all-like-this)
         ("C-c o" . mc/mark-all-like-this)))

(use-package ace-mc
  :ensure t
  :defer t
  :bind (("C-)" . ace-mc-add-multiple-cursors)
         ("C-M-)" . ace-mc-add-single-cursor)))

(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-=" . er/expand-region)
  )

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind (("C-c SPC" . ace-jump-mode))
  )

;; (use-package ace-window
;;   :ensure t
;;   :defer t
;;   :bind (("M-p" . ace-window))
;;   )

;; add bookmark package; open if bookmark is file; else call helm-find-files
(use-package bookmark
  :ensure t
  :defer t
  :config
  (progn
    (defun bookmark-find-from-dir-or-default (orig-fun bmk-record)
      "Calls through unless bookmark is a directory, in which
             case, calls helm-find-files."
      (let ((file (bookmark-get-filename bmk-record)))
        (if (file-directory-p file)
            (let ((default-directory file))
              (call-interactively 'helm-find-files))
          (funcall orig-fun bmk-record))))
    (advice-add `bookmark-default-handler
                :around #'bookmark-find-from-dir-or-default)))

(use-package bm
  :ensure t
  :demand t
  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)
  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)
  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  ;; save bookmarks
  (setq-default bm-buffer-persistence t)
  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)
  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)
  ;; highlight and add fringe
  (setq bm-highlight-style 'bm-highlight-line-and-fringe)
  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  :bind (("C-c j" . bm-next)
         ("C-c p" . bm-previous)
         ("C-c g" . bm-toggle))
  )

(use-package json-navigator
  :ensure t)

;; swiper configuration
(use-package swiper
  :ensure t
  :bind ("C-c i" . swiper)
  )

(use-package iedit
  :ensure t
  :diminish iedit-mode
  :bind ("C-'" . iedit-mode))

(use-package origami
  :ensure t)

(use-package ripgrep
  :ensure t)

(use-package fzf
  :ensure t)
