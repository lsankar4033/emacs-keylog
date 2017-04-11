;; This code is used to log all keystrokes to disk. Keystrokes are logged with timestamp and the major mode
;; they were entered in.

(require 'cl-lib)

;;; Logging hooks
(define-minor-mode global-looking-glass-mode
  "Global minor mode for looking-glass loggin."
  nil nil nil
  (when global-looking-glass-mode
      (progn (add-hook 'pre-command-hook 'log-keys)
             (add-hook 'kill-emacs-hook 'save-log-buffer))))

;;; Logging code

;; TODO buffer-dir should be configurable
(defconst keylog-log-buffer-dir "~/.emacs.d/looking_glass/")
(defconst log-file-date-format "%Y-%m-%d %H:%M:%S")

;; NOTE this is currently created at load time of *this* file
(defvar keylog-log-buffer-name (format-time-string log-file-date-format (current-time)))

(defun log-keys ()
  (interactive)
  (let ((deactivate-mark deactivate-mark))
    (when (this-command-keys)
      (let ((major-mode-name major-mode)
	    (tt (format-time-string "%s" (current-time))))
	(with-current-buffer (get-buffer-create keylog-log-buffer-name)
	  (goto-char (point-max))
	  (if (eql this-command 'self-insert-command)
	      (let ((desc (key-description (this-command-keys))))
		(if (= 1 (length desc))
		    (insert (format "\n%s %S " tt major-mode-name) desc)
		  (insert (format "\n%s %S " tt major-mode-name) " " desc " ")))
	    (insert (format "\n%s %S " tt major-mode-name) (key-description (this-command-keys)))))))))

(defun save-log-buffer ()
  (interactive)
  (with-current-buffer (get-buffer-create keylog-log-buffer-name)
    (write-file (concat keylog-log-buffer-dir keylog-log-buffer-name ".log"))))
