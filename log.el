(setq log-time-string-format "%y/%m/%d %H:%M:%S")

(defun log-time-string (&optional format)
  (format-time-string log-time-string-format)
  )

(defvar log-file-name "log.log")

(defun log-append (&optional s)
  (cond ((file-exists-p log-file-name)
	 (set-buffer (find-file-noselect log-file-name))
	 (eob)
	 (or (bolp) (insert "\n"))
	 (insert (log-time-string) (concat "\t" s))
	 )))

(defun log-to-file () (interactive)
  (switch-to-buffer (find-file-noselect log-file-name))
  (eob)
  (or (bolp) (insert "\n"))
  (insert (log-time-string) "\t")
  )

