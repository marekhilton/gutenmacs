;;; gutenmacs-fzf --- fzf support for gutenmacs

;;; Commentary:

;;; Code:

(require 'term)
(require 'dash)

(defconst gutenmacs-fzf-procname "gutenmacs-fzf-process")
(defvar gutenmacs-fzf-result nil)

(defun gutenmacs-fzf-after-term-handle-exit (proc-name msg)
  (let* ((lines (split-string (buffer-string) "\n"))
	 (selects (-filter (lambda (s) (string-match-p "\s*>\s+" s)) lines))
	 (choice (string-trim-left (car selects) "\s*>\s+")))
    ;(message choice)
    (setq gutenmacs-fzf-result choice)
    (kill-buffer)
    (setq unread-command-events (listify-key-sequence "\n"))))

(defun gutenmacs-fzf (filename)
  (advice-add 'term-handle-exit :after #'gutenmacs-fzf-after-term-handle-exit)
  (switch-to-buffer
   (make-term gutenmacs-fzf-procname
	      shell-file-name nil shell-command-switch
	      (format "cat %s | fzf" filename)))
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

;;; gutenmacs-fzf.el ends here
