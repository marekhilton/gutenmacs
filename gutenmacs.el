;;; Gutenberg --- Package for reading texts from the gutenburg

;;; Commentary:

;;; Code:

(require 'fuzzy-finder)
(require 'hydra)
(require 'dash)

(defconst data-dir
  (let ((dir (expand-file-name "./data/")))
    (unless (file-directory-p dir)
      (make-directory dir))
    dir))

(defconst url-index-name
  "GUTINDEX.ALL")
  

(defcustom mirror-url
  "http://mirror.csclub.uwaterloo.ca/gutenberg/"
  "Mirror URL."
  :type 'string
  :group 'gutenmacs)
(defcustom index-url
  "http://mirror.csclub.uwaterloo.ca/gutenberg/GUTINDEX.ALL"
  "URL of full gutenberg index."
  :type 'string
  :group 'gutenmacs)
(defcustom index-file
  (concat (file-name-as-directory data-dir) "index")
  "File containing index"
  :type 'string
  :group 'gutenmacs)

(defun gutenmacs-get-index ()
  (interactive)
  (url-copy-file (concat (file-name-as-directory mirror-url) url-index-name)
		 index-file t)
  (shell-command
   (format "./strip %s | sponge %s" index-file index-file)))

(defun gutenmacs ()
  (interactive)
  (gutenmacs-search-index))

(defun gutenmacs-search-index ()
  (fuzzy-finder :action (lambda (l)
			  (kill-buffer (format "*%s*" fuzzy-finder--process-name))
			  (gutenmacs-visit-index-entry
			   (gutenmacs-gen-index-url (car l))))
		:input-command (format "cat %s" index-file)))

(defun gutenmacs-expand-index (index)
  (let ((index-terms
	 (append
	  (butlast (mapcar #'char-to-string (string-to-list index)))
	  (list index))))
    (file-name-as-directory (mapconcat #'identity index-terms "/"))))

(defun gutenmacs-visit-index-entry (index-url)
  (with-current-buffer (url-retrieve-synchronously index-url)
    (gutenmacs-choose-dialog index-url (buffer-string))))


(defun gutenmacs-gen-index-url (entry)
  (let* ((index (progn (string-match "\\([0-9]+\\)" entry)
		       (message (match-string 0 entry))))
	 (expanded-index
	  (gutenmacs-expand-index index)))
    (message
     (concat (file-name-as-directory mirror-url) expanded-index))))

(global-set-key (kbd "C-c n") 'gutenmacs)

(defun gutenmacs-find-all-matches (pattern str)
  (if (string-match pattern str)
      (cons (match-string 0 str)
	    (gutenmacs-find-all-matches pattern (substring str (match-end 0))))
    '()))


(defun gutenmacs-hydra-file-ents (open-func filenames &optional n)
  (or n (setq n 0))
  (if (null filenames)
      '()
    (cons (cons (format "_%d_ %s" n (car filenames))
		(list (int-to-string n)
		      (list open-func (car filenames))))
	  (gutenmacs-hydra-file-ents open-func (cdr filenames) (+ 1 n)))))

(defun gutenmacs-choose-dialog (url html)
  (let* ((open-func `(lambda (filename)
		       (gutenmacs-open-text
			(concat ,(file-name-as-directory url) filename))))
	 (files (-distinct
		 (gutenmacs-find-all-matches "\\([0-9\-]+.txt\\)" html)))
	 (files-info (gutenmacs-hydra-file-ents open-func files))
	 (file-open-funcs (mapcar 'cdr files-info))
	 (files-labels
	  (format "\nFILES:\n%s\n\n_q_uit\n" (mapconcat 'car files-info "\n"))))
    (eval (list* 'defhydra 'gutenmacs-dialog
		 '(nil nil :foreign-keys nil :hint nil :exit t)
		 files-labels
		 '("q" nil)
		 file-open-funcs))
    (gutenmacs-dialog/body)))


(defun gutenmacs-open-text (url)
  (switch-to-buffer (url-retrieve-synchronously url t))
  (delete-trailing-whitespace)
  (fundamental-mode)
  (read-only-mode)
  (set-buffer-multibyte t)
  (rename-buffer "Gutenberg"))
  

(provide 'gutenmacs)
;;; gutenmacs.el ends here
