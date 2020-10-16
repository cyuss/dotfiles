;; this file contains all configuration
;; related to language and translation settings

;; a dictionary to define words (English)
;; (use-package define-word
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-c d") 'define-word-at-point)
;;   (global-set-key (kbd "C-c D") 'define-word)
;;   )

;; set google translate
(use-package google-translate
  :ensure t
  :config
  (setq google-translate-translation-directions-alist
        '(("de" . "en") ("en" . "de")))
  (setq google-translate-show-phonetic t)
  (global-set-key (kbd "C-c d") 'google-translate-at-point)
  )

;; a dictionary for chinese
(use-package youdao-dictionary
  :ensure t
  :config
  ;; Enable cache
  ;; (setq url-automatic-caching t)
  ;; Keybinding
  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)
  (global-set-key (kbd "C-c Y") 'youdao-dictionary-search-at-point+)
  ;; Enable Chinese word segmentation support (支持中文分词)
  ;; (setq youdao-dictionary-use-chinese-word-segmentation t)
  )
