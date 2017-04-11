(require 'cl-lib)

(require 'keylog-utils)

(provide 'extractor)

(define-hash-table-test 'contents-hash 'equal 'sxhash)

(defun line-seq (file)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun line->major-mode-and-key (line)
  (let ((items (split-string line " ")))
    (list (nth 1 items) (nthcdr 2 items))))

(defun aggregate-keymap (mode-and-key-lst)
  (let ((ag-map (make-hash-table :test 'contents-hash)))
    (dolist (mode-and-key mode-and-key-lst)
      (let* ((mode (first mode-and-key))
	     (key (second mode-and-key))
	     (cur-mode-map (or (gethash mode ag-map)
			       (make-hash-table :test 'contents-hash)))
	     (cur-key-count (or (gethash key cur-mode-map)
				0)))
	(puthash key (+ 1 cur-key-count) cur-mode-map)
	(puthash mode cur-mode-map ag-map)))

    ag-map))

(defun get-keymap-for-file (file)
  (->> (line-seq file)
       (mapcar 'line->major-mode-and-key)
       (aggregate-keymap)))

; This isn't quite right- really we'll need to filter the items in each map based on the timestamp as well
; TODO fix the logic here
(defun file-in-time-range-p (file start end)
  (if (< (length file) 30)
      ; Handle the case of '.' and '..' files
      nil

    (let ((file-date (date-to-time
		      ; This is pretty hardcodey- it would be good to make this DRYer with the writing logic
		      (substring file 7 26))))
      (and
       (time-less-p start file-date)
       (time-less-p file-date end)))))

(defun get-files-for-date-range (start end)
  ; list files in log file dir, filter to those with time after start and before end
  (->> (directory-files keylog-log-buffer-dir)
       (cl-remove-if-not (lambda (file) (file-in-time-range-p file start end)))
       (mapcar (lambda (file) (concat keylog-log-buffer-dir file)))))

(defun merge-keymaps (keymaps)
  (let ((full-map (make-hash-table :test 'contents-hash)))

    (dolist (keymap keymaps)

      (maphash (lambda (mode mode-map)
		 (maphash (lambda (key count)
			    ; TODO there's quite a bit of duplication of logic with 'aggregate-keymap' here
			    (let* ((cur-mode-map (or (gethash mode full-map)
						     (make-hash-table :test 'contents-hash)))
				   (cur-key-count (or (gethash key cur-mode-map)
						      0)))
			      (puthash key (+ count cur-key-count) cur-mode-map)
			      (puthash mode cur-mode-map full-map)))
			  mode-map))
	       keymap))
    full-map))

(defun get-keymap-for-date-range (start end)
  (let ((files (get-files-for-date-range start end)))
    (->> files
	 (mapcar 'get-keymap-for-file)
	 (merge-keymaps))))

(defconst keylog-view-buffer "*view-buffer*")

(defun recent-keymap-view ()
  (interactive)
  (with-current-buffer (get-buffer-create keylog-view-buffer)
    (let ((km (get-keymap-for-date-range
	       (time-subtract (current-time) (seconds-for-time (* 60 60 24 5)))
	       (current-time) )))

      ; TODO sort km by total count and display mode -> count

      ; TODO remove
      (insert "WOO")))

  (switch-to-buffer-other-window keylog-view-buffer))
