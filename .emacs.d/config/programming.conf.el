;; this file contains all the configuration
;; related to programming environment

;; Dockerfile & Docker-compose modes
(use-package dockerfile-mode
  :defer t
  :ensure t)
(use-package docker-compose-mode
  :defer t
  :ensure t)

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package go-mode
  :defer t
  :ensure t
  :mode ("\\.go$" . go-mode))

;; JavaScript
(use-package js2-mode
  :ensure t
  :defer t
  :config
  (use-package ac-js2
    :ensure t
    :defer t
    :config
    (add-hook 'js2-mode-hook 'ac-js2-mode)))

;; set snippets for emacs
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-minor-mode t))

;; markdown configuration
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; better solution for commenting lines
(use-package comment-dwim-2
  :ensure t
  :bind ("C-;" . comment-dwim-2)
  :config (setq comment-dwim-2--inline-comment-behavior 'reindent-comment)
  )
