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

(use-package poetry
  :ensure t)
