;; This code is used to log all keystrokes to disk. Keystrokes are logged with timestamp and the major mode
;; they were entered in.

(require 'cl-lib)

;;; Logging hooks

(define-minor-mode global-looking-glass-mode
  "Global minor mode for looking-glass loggin."
  nil nil nil
  (when global-looking-glass-mode
    (progn
      ;; TODO figure out how to not log repeat key-presses due to a held key
      (add-hook 'pre-command-hook 'log-keys)

      ;; TODO figure out a more frequent hook that can be used to trigger 'save-log-buffer
      (add-hook 'kill-emacs-hook 'save-log-buffer))))

;;; Logging code

;; TODO should be configurable and synced with CLI
(defconst keylog-log-buffer-dir "~/.emacs.d/looking_glass/")

;; NOTE this works because the python dateutil.parse method handles it automatically. be careful if changing this!
(defconst log-file-datetime-format "%Y-%m-%d %H:%M:%S %Z")

;; NOTE this is currently created at load time of *this* file
(defvar keylog-log-buffer-name (format-time-string log-file-datetime-format (current-time) "UTC0"))

(defun log-keys ()
  (interactive)
  (let ((deactivate-mark deactivate-mark))
    (when (this-command-keys)
      (let ((major-mode-name major-mode)
            ;; TODO properly handle daylight savings time. I think this creates timestamps 1 hour late
	    (tt (format-time-string "%s" (current-time) "UTC0")))
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
