;;; gutenmacs-fzf --- fzf support for gutenmacs

;;; Commentary:

;;; Code:

(require 'term)
(require 'dash)


;; Custom defintions

(defcustom gutenmacs-fzf-exe
  "fzf"
  "Name of the fzf executable on this system."
  :type 'string
  :group 'gutenmacs)

(defcustom gutenmacs-fzf-process-name
  "gutenmacs-fzf-process"
  "Name of the fzf process used by Gutenmacs."
  :type 'string
  :group 'gutenmacs)

(defvar gutenmacs-fzf-result nil
  "Variable where the result of an fzf search is temporarily
stored for communication between callback and main functions.
Intention is to remove this at some point.")


;; gutenmacs fzf functions

(defun gutenmacs-fzf-after-term-handle-exit (proc-name msg)
  "Function called after termination of fzf process.  Used to
temporarily populate gutenmacs-fzf-result with the result of the
fzf search."
  (let* ((lines (split-string (buffer-string) "\n"))
	 (selects (-filter (lambda (s) (string-match-p "\s*>\s+" s)) lines))
	 (choice (string-trim-left (car selects) "\s*>\s+")))
    (setq gutenmacs-fzf-result choice)
    (kill-buffer)
    (setq unread-command-events (listify-key-sequence "\n"))))

(defun gutenmacs-fzf (filename)
  "Perform fzf search on contents of FILENAME."
  (unless (executable-find gutenmacs-fzf-exe)
    (user-error "Please ensure fzf is installed and PATH"))
  
  (advice-add 'term-handle-exit :after #'gutenmacs-fzf-after-term-handle-exit)
  (switch-to-buffer
   (make-term gutenmacs-fzf-process-name
	      shell-file-name nil shell-command-switch
	      (format "cat %s | %s" filename gutenmacs-fzf-exe)))
  (term-char-mode)
  (visual-line-mode 0)
  (linum-mode 0)
  (when (fboundp #'company-mode)
    (company-mode 0))
  (setq-local mode-line-format nil)
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 0)
  (setq-local term-suppress-hard-newline t)  ; for paths wider than the window
  (setq-local show-trailing-whitespace nil)
  (setq-local display-line-numbers nil)

  (setq gutenmacs-fzf-result nil)
  (while (null gutenmacs-fzf-result)
    (command-execute (read-key-sequence nil)))

  gutenmacs-fzf-result)

(provide 'gutenmacs-fzf)
;;; gutenmacs-fzf.el ends here
