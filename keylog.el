(defconst keylog-log-buffer-dir "~/.emacs.d/keylog/")
(defvar keylog-log-buffer-name (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))

; This logic is a modified version of Chris Done's script for key logging found here: http://lpaste.net/91637
(defun log-keys ()
  (interactive)
  ; TODO - determine why this deactivate-mark line is necessary
  (let ((deactivate-mark deactivate-mark))
    (when (this-command-keys)
      (let ((major-mode-name major-mode)
	    (tt (format-time-string "%s" (current-time))))
	(with-current-buffer (get-buffer-create keylog-log-buffer-name)
	  (goto-char (point-max))
		; TODO - understand this special case for self-insert-command
	  (if (eql this-command 'self-insert-command)
	      (let ((desc (key-description (this-command-keys))))
		(if (= 1 (length desc))
		    (insert (format "\n%s %S " tt major-mode-name) desc)
		  (insert (format "\n%s %S " tt major-mode-name) " " desc " ")))
	    (insert (format "\n%s %S " tt major-mode-name) (key-description (this-command-keys)))))))))

(add-hook 'pre-command-hook 'log-keys)


(defun save-log-buffer ()
  (interactive)
  (with-current-buffer (get-buffer-create keylog-log-buffer-name)
    (write-file (concat keylog-log-buffer-dir keylog-log-buffer-name ".log"))))

(add-hook 'kill-emacs-hook 'save-log-buffer)
