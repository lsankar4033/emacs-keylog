(provide 'keylog-utils)

(defconst keylog-log-buffer-dir "~/.emacs.d/keylog/")
(defconst log-file-date-format "%Y-%m-%d %H:%M:%S")

(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))
