(setq compile-log nil)

(setq time-string-format "%m/%d/%y %H:%M:%S")

(defun time-string (&optional format)
  (substring
   (shell-output
    (format "date \"+%s\"" (or format time-string-format))) 0 -1))

(defvar log-file-name "a.log")

(defun log-time ()
  (let ((s (current-time-string)))
    (substring s 4 -5)))

(defun append-log-entry (&optional s)
  (cond ((file-exists-p log-file-name)
	 (set-buffer (find-file-noselect log-file-name))
	 (eob)
	 (or (bolp) (insert "\n"))
	 (insert (substring (current-time-string) 4 -5) (concat "\t" s))
	 )))
