;; this file contains all the configuration
;; related to helm settings

;; (use-package helm-icons
;;   :ensure t)

;; helm configuration
(use-package helm
  :diminish helm-mode
  :ensure t
  ;; :after (helm-icons)
  :init
  (progn
    ;; (helm-icons-enable)
    (require 'helm-bm) ;; Not necessary if using ELPA package
    (setq helm-candidate-number-limit 100)
    (setq helm-idle-delay 0.0
          helm-input-idle-delay 0.01
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
		  history-delete-duplicates t
		  history-length 10)
    (helm-mode))
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-i" . helm-swoop)
         ("M-y" . helm-show-kill-ring)
         ("C-c h o" . helm-occur)
         ("M-D" . helm-buffer-run-kill-buffers)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-<tab>" . helm-select-action)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch))
  )

(use-package helm-bm
  :ensure t
  :bind ("C-c m" . helm-bm))

(use-package helm-make
  :ensure t)

;; (use-package ivy-posframe
;;   :ensure t
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   (setq ivy-posframe-display-functions-alist
;; 	'((swiper          . ivy-posframe-display-at-frame-center)
;; 	  (complete-symbol . ivy-posframe-display-at-frame-center)
;; 	  (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;; 	  (t               . ivy-posframe-display)))
;;   (ivy-posframe-mode 1))
