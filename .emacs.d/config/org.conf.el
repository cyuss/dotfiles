;; this file contains all the configuration
;; related to org mode and all the packages
;; interact or use org mode

(use-package calfw
  :ensure t)

(use-package calfw-org
  :ensure t
  :config
  '(org-agenda-include-diary)
  '(holiday-islamic-holidays t)
  '(holiday-bahai-holidays nil)
  '(holiday-hebrew-holidays nil)
  (setq cfw:display-calendar-holidays t)
  )

;; a package for presentations based on org-mode
(use-package zpresent :ensure t)

;; define dependencies between tasks
(use-package org-edna
  :ensure t
  :after org-mode)

(use-package org-board
  :ensure t
  :defer t)

(setq org-agenda-repeating-timestamp-show-all nil)
(setq org-habit-show-habits-only-for-today t)
;; org mode
(use-package org
  :diminish org-indent-mode
  :ensure t
  :defer t
  :mode ("\\.org" . org-mode)
  :bind (("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb)
	 ("C-c l" . org-store-link))
  :config
  (setq org-src-window-setup 'current-window)
  (use-package org-ac :ensure t)
  (org-ac/config-default)
  ;; (org-edna-load)
  ;; (require 'org-drill) ;; better using 'use-package' configuration
  (use-package org-drill :ensure t)
  (add-to-list 'org-modules 'org-habit)
  ;; (defadvice org-agenda (around split-vertically activate)
  ;; (let ((split-width-threshold 80))  ; or whatever width makes sense for you
  ;;   ad-do-it))
  (setq org-agenda-files '("/Users/youcef/Dropbox/personal/agenda"))
  ;; (setq org-habit-show-habits-only-for-today nil) ;; show up habits only for today
  ;; (setq org-agenda-repeating-timestamp-show-all nil) ;; show up habit only when it is next scheduled, but no further repetitions
  (custom-set-variables ;; show logbook drawer for historical logs
   '(org-log-into-drawer t))
  ;; (setq org-log-done t)
  (setq org-timeline-show-empty-dates t)
  (setq org-src-fontify-natively t)
  (setq org-ellipsis "⤵")
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-log-mode-items '(closed clock state)) ;; to see done tasks in org-agenda (by pressing l)
  (setq org-default-notes-file (concat "" "~/Dropbox/personal/notes/notes.org")) ;; DONE: change the location
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 6)))
  ;; we can refile a note only to a subtree,
  ;; to change that we can uncomment the following line, or
  ;; refile to a special header called `Refiling header`
  ;; and then refactor the headers
  ;; (setq org-refile-use-outline-path 'file) ;; HACK: can refile to a file
  (add-hook 'org-mode-hook 'org-hide-block-all)
  (add-hook 'org-mode-hook 'toggle-truncate-lines)
  (setq org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t)
  (use-package org-bullets
    :ensure t
    :init
    (add-hook 'org-mode-hook 'org-bullets-mode))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  ;; add ID automatically on capture
  (add-hook 'org-capture-prepare-finalize-hook 'org-id-store-link)
  ;; customize todo sequence
  (setq org-todo-keywords
	(quote ((sequence "NEXT(n!)" "TODO(t!)" "INPROGRESS(p!)" "WAITING(w!)" "SOMEDAY(.!)" "|" "CANCELED(c@)" "REJECTED(r!)" "DONE(d!)")
		(sequence "MEETING(m!)" "PHONE(h!)" "RDV(v!)" "READ(e!)" "EVENT(z!)" "|" "CANCELED(c@)" "DONE(d!)")
		(sequence "|" "QUESTION(q!)" "IDEA(i!)" "NOTE(o!)")
		(sequence "HOLIDAYS(h!)" "DAYOFF(f!)" "TRIP(v!)" "|" "DONE(d!)"))))
  (setq org-todo-keyword-faces
	'(("SOMEDAY" . (:foreground "cyan" :weight bold))
	  ("IDEA" . (:foreground "yellow" :weight bold))
	  ("NOTE" . (:foreground "yellow" :weight bold))
	  ("QUESTION" . (:foreground "turquoise" :weight bold))
	  ("INPROGRESS" . (:foreground "pink" :weight bold))
	  ("WAITING" . (:foreground "lemon chiffon" :weight bold))
	  ("CANCELED" . (:foreground "tomato" :weight bold))
	  ("RDV" . (:foreground "magenta" :weight bold))
	  ("NEXT" . (:foreground "coral" :weight bold))
	  ("READ" . (:foreground "DarkSeaGreen1" :weight bold))
	  ;; HACK: changed the face cause of theme for TODO keyword
	  ("TODO" . (:foreground "orange" :weight bold))
	  ("MEETING" . (:foreground "moccasin" :weight bold))
	  ("EVENT" . (:foreground "moccasin" :weight bold))
	  ("PHONE" . (:foreground "moccasin" :weight bold))
	  ("HOLIDAYS" . (:foreground "peach puff" :weight bold)))
	)
  (setq org-html-checkbox-type 'unicode)
  (setq org-html-checkbox-types
	'((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
		   (off . "<span class=\"task-todo\">&#x2610;</span>")
		   (trans . "<span class=\"task-in-progress\">[-]</span>"))))
  ;; set company mode to complete org keywords
  (defun org-keyword-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'org-keyword-backend))
    (prefix (and (eq major-mode 'org-mode)
                 (cons (company-grab-line "^#\\+\\(\\w*\\)" 1)
                       t)))
    (candidates (mapcar #'upcase
                        (cl-remove-if-not
                         (lambda (c) (string-prefix-p arg c))
                         (pcomplete-completions))))
    (ignore-case t)
    (duplicates t)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (latex . t)
     (ledger . t)
     (C . t)
     (awk . t)
     ))

  ;; org-capture templates
  (setq org-capture-templates
	'(
	  ;; Templates for the TASKS keyword sequence
	  ("t" "Tasks")

	  ;; TODO     (t) Todo template
	  ("tt" "TODO" entry (file "~/Dropbox/personal/notes/notes.org")
	   "* TODO %?
  :PROPERTIES:
  :Note:
  :END:
  :LOGBOOK:
  - State \"TODO\"       from \"\"           %U
  :END:" :empty-lines 1)
	  ;; Templates for the EVENT keyword sequence
	  ("e" "Events")
	  ;; MEETING     (m) Meeting template
	  ("em" "MEETING (m) Meeting" entry (file "~/Dropbox/personal/notes/notes.org")
	   "* MEETING %?
  :PROPERTIES:
  :Note:
  :Attend:
  :Location:
  :END:
  :LOGBOOK:
  - State \"MEETING\"       from \"\"           %U
  :END:" :empty-lines 1)

	  ;; EVENT (e) Event template
	  ("ee" "EVENT (e) Event" entry (file "~/Dropbox/personal/notes/notes.org")
	   "* EVENT %?
  :PROPERTIES:
  :Note:
  :Location:
  :END:
  :LOGBOOK:
  - State \"EVENT\"       from \"\"           %U
  :END:" :empty-lines 1)

	  ;; PHONE (p) Phone template
	  ("ep" "PHONE (p) Phone" entry (file "~/Dropbox/personal/notes/notes.org")
	   "* PHONE %?
  :PROPERTIES:
  :Note:
  :Attend:
  :END:
  :LOGBOOK:
  - State \"PHONE\"       from \"\"           %U
  :END:" :empty-lines 1)

	  ;; RDV (r) RDV template
	  ("er" "RDV (r) RDV" entry (file "~/Dropbox/personal/notes/notes.org")
	   "* RDV %?
  :PROPERTIES:
  :Note:
  :Attend:
  :Location:
  :END:
  :LOGBOOK:
  - State \"RDV\"       from \"\"           %U
  :END:" :empty-lines 1)

	  ("r" "Reading")
	  ;; READ (r) Read template
	  ("rr" "READ (r) Read" entry (file "~/Dropbox/personal/notes/notes.org")
	   "* READ %?
  :PROPERTIES:
  :Note:
  :Title:
  :Link:
  :END:
  :LOGBOOK:
  - State \"READ\"       from \"\"           %U
  :END: \n%a" :empty-lines 1)
	  ;; Templates for taking notes
	  ("n" "Notes")
	  ;; NOTE (n) Note template
	  ("nn" "NOTE (n) Note" entry (file "~/Dropbox/personal/notes/notes.org")
	   "* NOTE %?
  :PROPERTIES:
  :Note:
  :END:
  :LOGBOOK:
  - State \"NOTE\"       from \"\"           %U
  :END: \n%a" :empty-lines 1)
	  ;; IDEA (i) Idea template
	  ("ni" "IDEA (i) Idea" entry (file "~/Dropbox/personal/notes/notes.org")
	   "* IDEA %?
  :PROPERTIES:
  :Note:
  :END:
  :LOGBOOK:
  - State \"IDEA\"       from \"\"           %U
  :END: \n%a" :empty-lines 1)
	  ;; QUESTION (q) Question template
	  ("nq" "QUESTION (q) Question" entry (file "~/Dropbox/personal/notes/notes.org")
	   "* QUESTION %?
  :PROPERTIES:
  :Note:
  :END:
  :LOGBOOK:
  - State \"QUESTION\"       from \"\"           %U
  :END: \n%a" :empty-lines 1)
	))
  )
