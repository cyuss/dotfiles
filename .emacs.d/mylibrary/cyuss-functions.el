;; useful functions to load

;; useful functions
(defun cyuss--delete-in-quotes () 
  "Deletes the text inside of quotes."
  (interactive)
  ;; Search for a match on the same line, don't delete across lines
  (search-backward-regexp "[\"\']" (line-beginning-position))
  (forward-char)
  (let  ((lstart (point)))
    (search-forward-regexp "[\"\']" (line-end-position))
    (backward-char)
    (kill-region lstart (point)))
  )

(defun cyuss--delete-in-parentheses () 
  "Deletes the text within parentheses."
  (interactive)
  ;; Search for a match on the same line, don't delete across lines
  (search-backward "(" (line-beginning-position))
  (forward-char)
  (let  ((lstart (point)))
    (search-forward ")" (line-end-position))
    (backward-char)
    (kill-region lstart (point)))
  )

(defun cyuss--delete-in-brackets () 
  "Deletes the text within square brackets, angle brackets, and curly brackets."
  (interactive)
  ;; Search for a match on the same line, don't delete across lines
  (search-backward-regexp "[[{<]" (line-beginning-position))
  (forward-char)
  (let ((lstart (point)))
    (search-forward-regexp "[]}>]" (line-end-position))
    (backward-char)
    (kill-region lstart (point)))
  )

(defun cyuss--rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
	(message "A buffer named '%s' already exists!" new-name)
      (progn
	(rename-file name new-name 1)
	(rename-buffer new-name)
	(set-visited-file-name new-name)
	(set-buffer-modified-p nil))))
  )

(defun cyuss--insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time)))
  )

(defun cyuss--generate-numbered-list (start end)
  "Creates a numbered list from provided start to provided end."
  (interactive "nStart num:\nnEnd num:")
  (let ((x  start))
    '(while (<= x end)
       (insert (number-to-string x) ".")
       (newline)
       (setq x (+ x 1))))
  )

(defun cyuss--search-all-buffers (regexp) 
  "Search all open buffers for a regex. Open an occur-like window."
  (interactive "sRegexp: ")
  (multi-occur-in-matching-buffers "." regexp t)
  )

(defun cyuss--make-temp-file (name)
  "Creates a temporary file in the system temp directory, for various purposes."
  (interactive "sFile name:")
  (generate-new-buffer name)
  (switch-to-buffer name)
  (write-file (concat temporary-file-directory name))
  )

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max))
  )

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max))
  )

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace)
  )

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end))
  )

(defun cyuss--org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file which do not already have one."
  (interactive)
  (org-map-entries 'org-id-get-create))

(defun cyuss--org-summarize-task-status ()
  "Count number of tasks by status.
Probably should make this a dblock someday."
  (interactive)
  (let (result)
    (org-map-entries
     (lambda ()
       (let ((todo (elt (org-heading-components) 2)))
         (if todo
             (if (assoc todo result)
                 (setcdr (assoc todo result)
                         (1+ (cdr (assoc todo result))))
               (setq result (cons (cons todo 1) result)))))))
    (message "%s" (mapconcat (lambda (x) (format "%s: %d" (car x) (cdr x)))
                             result "\n"))))

(defun cyuss--prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun cyuss--shuffle-lines-in-region (beg end)
  (interactive "r")
  (let ((list (split-string (buffer-substring beg end) "[\r\n]+")))
    (delete-region beg end)
    (insert (mapconcat 'identity (shuffle-list list) "\n"))))

;; add some shortcuts to clean buffer and region
(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun cyuss--org-global-custom-ids ()
  "Find custom ID fields in all org agenda files."
  (let ((files (org-agenda-files))
        file
        cyuss-all-org-custom-ids)
    (while (setq file (pop files))
      (with-current-buffer (org-get-agenda-file-buffer file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (re-search-forward "^[ \t]*:CUSTOM_ID:[ \t]+\\(\\S-+\\)[ \t]*$"
                                      nil t)
              (add-to-list 'cyuss-all-org-custom-ids
                           `(,(match-string-no-properties 1)
                             ,(concat file ":" (number-to-string (line-number-at-pos))))))))))
    cyuss-all-org-custom-ids))

(defun cyuss--org-goto-custom-id ()
  "Go to the location of a custom ID, selected interactively."
  (interactive)
  (let* ((all-custom-ids (cyuss--org-global-custom-ids))
         (custom-id (completing-read
                     "Custom ID: "
                     all-custom-ids)))
    (when custom-id
      (let* ((val (cadr (assoc custom-id all-custom-ids)))
             (id-parts (split-string val ":"))
             (file (car id-parts))
             (line (string-to-int (cadr id-parts))))
        (pop-to-buffer (org-get-agenda-file-buffer file))
        (goto-char (point-min))
        (forward-line line)
        (org-reveal)
        (org-up-element)))))

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "C-S-d") 'duplicate-line)

;; define my skeletons for languages
(define-skeleton deutsch-verb
  "Add a verb in deutsch"
  ""
  "** Verb                                                             :drill:\n"
  "  :PROPERTIES:\n"
  "  :DRILL_CARD_TYPE: multisided\n"
  "  :END:\n"
  "*** Allemand\n"
  (skeleton-read "German: ") "\n"
  "*** Francais\n"
  (skeleton-read "French: ") "\n"
  )
