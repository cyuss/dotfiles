;; my emacs configuration

;; (load "package")
;; (load "cl")
(require 'cl-lib)
(setq byte-compile-warnings '(cl-functions))
;; HACK: bug fix https://www.reddit.com/r/emacs/comments/cdf48c/failed_to_download_gnu_archive/
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

(unless (assoc-default "marmalade" package-archives)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t))
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(unless (assoc-default "melpa-stable" package-archives)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq load-prefer-newer t)
;; enable use-package
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))
;; (require 'diminish) ;; if we want to use :diminish
(use-package bind-key
  :ensure t) ;; if we want to use :bind

;; Setup diminish to keep the mode line nice
(use-package diminish
  :ensure t
  ;; :diminish auto-revert-mode
  ;; :diminish abbrev-mode
  :diminish helm-mode
  :diminish eldoc-mode
  :diminish flymake-mode
  :diminish elpy-mode
  :diminish sphinx-doc-mode
  :diminish org-indent-mode)

;; fix the $PATH environment variable
;; inherit from the shel environment
;; using it in Os X
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package restart-emacs
  :ensure t)

;; increase gc-cons threshold to decrease the load and compile time
;; (setq gc-cons-threshold 402653184
;;       gc-cons-percentage 0.6
;;       ;;gc-cons-threshold (* 1024 1024 1024) ;1G
;;       jit-lock-stealth-time 1
;;       jit-lock-chunk-size 500
;;       jit-lock-defer-time 0.5)

;; set spell check with ispell
;; (setq ispell-program-name "/usr/local/bin/ispell")
;; (ispell-change-dictionary "francais")

(use-package miniedit
  :commands minibuffer-edit
  :ensure t
  :init (miniedit-install))

;; count lines of code over emacs buffers
(use-package cloc
  :ensure t)

;; configuration for ledger mode
(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transactions 1)
  :config
  (setq ledger-binary-path "/usr/local/bin/ledger")
  (company-mode 1)
  ;; (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
  :mode ("\\.ledger\\'" "\\.journal\\'"))

;; key bindings
;; (global-set-key (kbd "C-;") 'comment-or-uncomment-region)
;; (global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(load "~/.emacs.d/mylibrary/cyuss-functions.el")
(load "~/.emacs.d/config/org.conf.el")
(load "~/.emacs.d/config/python.conf.el")
(load "~/.emacs.d/config/gui.conf.el")
(load "~/.emacs.d/config/navigation.conf.el")
(load "~/.emacs.d/config/git.conf.el")
(load "~/.emacs.d/config/doc.conf.el")
(load "~/.emacs.d/config/programming.conf.el")
(load "~/.emacs.d/config/helm.conf.el")
(load "~/.emacs.d/config/language.conf.el")
;; (load "~/.emacs.d/config/go.conf.el")
