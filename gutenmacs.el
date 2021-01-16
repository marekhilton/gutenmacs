;;; Gutenberg --- Package for reading texts from the gutenburg

;;; Commentary:

;;; Code:

(require 'hydra)
(require 'dash)

(require 'gutenmacs-fzf)

(defconst gutenmacs-package-dir
  (file-name-directory load-file-name))
(defconst gutenmacs-package-cache
    (let ((dir (expand-file-name "~/.emacs.d/.cache/gutenmacs/")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))
(defconst gutenmacs-data-dir

  (let ((dir (concat (file-name-as-directory gutenmacs-package-cache)
		     "data/")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))
(defconst gutenmacs-strip-executable
  (concat gutenmacs-package-dir "strip"))

(defconst url-index-name
  "GUTINDEX.ALL")
  

(defcustom gutenmacs-mirror-url
  "http://mirror.csclub.uwaterloo.ca/gutenberg/"
  "Mirror URL."
  :type 'string
  :group 'gutenmacs)
(defcustom gutenmacs-index-url
  "http://mirror.csclub.uwaterloo.ca/gutenberg/GUTINDEX.ALL"
  "URL of full gutenberg index."
  :type 'string
  :group 'gutenmacs)
(defcustom gutenmacs-index-file
  (concat (file-name-as-directory gutenmacs-data-dir) "index")
  "File containing index"
  :type 'string
  :group 'gutenmacs)

;; Main interactive  gutenmacs functions

(defun gutenmacs-get-index ()
  (interactive)
  (let ((executables
	 (-filter (lambda (x) (not (executable-find x))) '("sed" "sponge"))))
    (if (null executables)
	()
      (user-error
       "Please install the following and make sure they are available on PATH: %s"
       (mapconcat 'identity executables ", "))))

  (url-copy-file (concat (file-name-as-directory gutenmacs-mirror-url) url-index-name)
		 gutenmacs-index-file t)
  (shell-command
   (format "%s %s | sponge %s"
	   gutenmacs-strip-executable
	   gutenmacs-index-file
	   gutenmacs-index-file)))

(defun gutenmacs ()
  "Main function to search through index file and subsequently
visit the index entry."
  (interactive)
  (let* ((index-entry (gutenmacs-fzf gutenmacs-index-file))
	 (entry-url (gutenmacs-gen-index-url index-entry)))
    (gutenmacs-visit-index-entry entry-url)))


;; Functions to generate index entry url

(defun gutenmacs-expand-index (index)
  "Helper function for gutenmacs-gen-index-url to turn an index
number into the directory path."
  (let ((index-terms
	 (append
	  (butlast (mapcar #'char-to-string (string-to-list index)))
	  (list index))))
    (file-name-as-directory (mapconcat #'identity index-terms "/"))))

(defun gutenmacs-gen-index-url (entry)
  "Generates url for a particular entry (line) of the index file"
  (let* ((index (progn (string-match "\\([0-9]+\\)" entry)
		       (message (match-string 0 entry))))
	 (expanded-index
	  (gutenmacs-expand-index index)))
    (message
     (concat (file-name-as-directory gutenmacs-mirror-url) expanded-index))))


;; Functions for visiting an index entry and selecting an appropriate
;; text file to open

(defun gutenmacs-visit-index-entry (index-url)
  (with-current-buffer (url-retrieve-synchronously index-url)
    (gutenmacs-choose-dialog index-url (buffer-string))))


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
