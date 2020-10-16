;; this file contains all the configuration
;; related to documentation settings

(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

;; it shows which combos are setup while we are typing
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package devdocs
  :defer t
  :ensure t)

(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

;; autocomplete with company
(use-package company
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'org-keyword-backend)
  ;; (add-to-list 'company-backends #'company-tabnine)
  ;; (add-to-list 'company-backends 'elpy-company-backend)
  ;;(add-to-list 'company-backends 'company-anaconda)
  :bind (("C-," . company-complete-common)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  )

;; (eval-after-load 'company
;;   '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))
(use-package company-quickhelp
  :diminish
  :ensure t
  :after company
  :config (company-quickhelp-mode))

;; (use-package company-tabnine
;;   :ensure t)

;; BUG: icons are too large
;; (use-package company-box
;;   :init (setq company-box-enable-icon (display-graphic-p))
;;   :diminish
;;   :ensure t
;;   :hook (company-mode . company-box-mode)
;;   :functions (all-the-icons-faicon
;; 	      all-the-icons-material
;; 	      all-the-icons-octicon
;; 	      all-the-icons-alltheicon))

;; autocomplete
(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config
  (require 'auto-complete-config)
  (ac-config-default))

;; add autocomplete in eshell
(use-package esh-autosuggest
  :ensure t
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package general
  :ensure t)
