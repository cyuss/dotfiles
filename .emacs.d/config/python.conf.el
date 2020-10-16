;; this file contains all the configuration
;; related to python workflow


;; define the dependencies when working with python project
;; (setq elpy-modules (delq 'elpy-module-company elpy-modules))
(defun cyuss--python-workenv ()
  "define my workflow in python"
  (interactive)
  ;; (setq python-shell-interpreter "/Users/youcef/anaconda3/bin/python")
  ;; (setq jedi:server-args
  ;; 	'("--virtual-env" "~/Users/youcef/anaconda3/"))
  (linum-mode 1)
  ;;(projectile-mode 1)
  (use-package elpy
    :ensure t)
  (elpy-enable)
  (elpy-mode 1)
  (company-mode 1)
  (yas-minor-mode t)
  (use-package sphinx-doc
    :diminish sphinx-mode
    :ensure t)
  (sphinx-doc-mode t)
  (pyvenv-workon "base")
  (flymake-mode -1)
  (setq-default tab-width 4)
  (highlight-indentation-mode -1))

(add-hook 'python-mode-hook 'cyuss--python-workenv)

(use-package pyimpsort
  :ensure t
  ;; :config
  ;; (eval-after-load 'python
  ;;   '(define-key python-mode-map "\C-cC-u" #'pyimpsort-buffer))
  )

;; set virtual environments on emacs
(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "/Users/youcef/anaconda3/envs/")
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1)
  )

;; python hooks
;; (defun cyuss--python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))
;; (add-hook 'python-mode-hook
;; 	      (lambda ()
;; 		    (setq-default indent-tabs-mode nil)
;; 		    (setq-default tab-width 4)
;;             (add-hook 'python-mode-hook 'cyuss--python-mode-hook)
;;             (cyuss--python-workenv)))


;; (use-package lsp-mode
;;   ;; :ensure t
;;   :config
;;   ;; make sure we have lsp-imenu everywhere we have LSP
;;   ;; (require 'lsp-imenu)
;;   (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
;;   ;; get lsp-python-enable defined
;;   ;; NB: use either projectile-project-root or ffip-get-project-root-directory
;;   ;;     or any other function that can be used to find the root directory of a project
;;   ;; (lsp-define-stdio-client lsp-python "python"
;;   ;;                          #'projectile-project-root
;;   ;;                          '("pyls"))

;;   ;; make sure this is activated when python-mode is activated
;;   ;; lsp-python-enable is created by macro above 
;;   ;; (add-hook 'python-mode-hook #'lsp-python-enable)
;; (use-package lsp-python-ms
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;; 			 (require 'lsp-python-ms)
;; 			 (lsp))))  ; or lsp-deferred

;;   ;; lsp extras
;;   (use-package lsp-ui
;;     :ensure t
;;     :config
;;     (setq lsp-ui-sideline-ignore-duplicate t)
;;     (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;;   (use-package company-lsp
;;     :ensure t
;;     :config
;;     (push 'company-lsp company-backends))

;;   ;; NB: only required if you prefer flake8 instead of the default
;;   ;; send pyls config via lsp-after-initialize-hook -- harmless for
;;   ;; other servers due to pyls key, but would prefer only sending this
;;   ;; when pyls gets initialised (:initialize function in
;;   ;; lsp-define-stdio-client is invoked too early (before server
;;   ;; start)) -- cpbotha
;;   ;; (defun lsp-set-cfg ()
;;   ;;   (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
;;   ;;     ;; TODO: check lsp--cur-workspace here to decide per server / project
;;   ;;     (lsp--set-configuration lsp-cfg)))

;;   ;; (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)
;;   )

;; (use-package dap-mode
;;  :ensure t
;;  :disabled
;;  :after lsp-mode
;;  :commands dap-debug
;;  :config
;;  (require 'dap-python))

;; (use-package lsp-mode
;;   :disabled
;;   :commands lsp)

;; (use-package lsp-python
;;   :disabled
;;   :hook (python-mode . lsp))

;; (use-package lsp-ui
;;   :disabled
;;   :after lsp-mode
;;   :hook (lsp-mode . lsp-ui-mode))

;; (require 'lsp-mode)
;; ;; (require 'lsp-python)
;; (add-hook 'python-mode-hook #'lsp-python-enable)

;; (use-package company-lsp
;;   :disabled
;;   :after lsp-mode
;;   :config
;;   (push 'company-lsp company-backends))

;; (use-package realgud
;;   :ensure t
;;   :defer t)

;; (use-package lsp-mode
;;   ;; :hook (python-mode . lsp)
;;   :commands lsp)

;; (add-hook 'python-mode-hook #'lsp)

;; (use-package lsp-mode
;;   :config
;;   (add-hook 'python-mode-hook #'lsp)
;;   (setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) over flymake.
;;   )

;; (use-package lsp-ui
;;   :requires lsp-mode flycheck
;;   :config

;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-doc-use-childframe t
;;         lsp-ui-doc-position 'top
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-sideline-enable nil
;;         lsp-ui-flycheck-enable t
;;         lsp-ui-flycheck-list-position 'right
;;         lsp-ui-flycheck-live-reporting t
;;         lsp-ui-peek-enable t
;;         ;; lsp-ui-peek-list-width 60
;;         ;; lsp-ui-peek-peek-height 25
;; 	)

;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package company-lsp
;;   :requires company
;;   :config
;;   (push 'company-lsp company-backends)
;;    ;; Disable client-side cache because the LSP server does a better job.
;;   (setq company-transformers nil
;;         company-lsp-async t
;;         company-lsp-cache-candidates nil))
