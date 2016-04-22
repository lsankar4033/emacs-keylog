(require 'cl-lib)

(provide 'keylog)

(defconst keylog-log-buffer-dir "~/.emacs.d/keylog/")
(defconst log-file-date-format "%Y-%m-%d %H:%M:%S")

(defvar keylog-log-buffer-name (concat "keylog-" (format-time-string log-file-date-format (current-time))))

; reminder of methods I need to convert time to/from string
;(format-time-string "%Y-%m-%d %H:%M:%S" (date-to-time "2016-03-12 12:10:10"))

; TODO Move this to a general utils file
(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

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

(defun line-seq (file)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun line->major-mode-and-key (line)
  (let ((items (split-string line " ")))
    (list (nth 1 items) (nth 2 items))))

(->> (line-seq "~/.emacs.d/keylog/keylog-2016-04-11 11:36:32.log")
     (mapcar 'line->major-mode-and-key)
     (cl-reduce +)
     )
