(require 'cl-lib)

(require 'utils)
(require 'logger)
(require 'extractor)

(provide 'keylog)

;; Logging hooks

(add-hook 'pre-command-hook 'log-keys)
(add-hook 'kill-emacs-hook 'save-log-buffer)

;; Extraction hooks

;; TODO
