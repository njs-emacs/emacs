(defun file-contents (name &optional size)
  "Returns the contents of file with name NAME."
  (save-excursion 
    (let ((buffer (get-file-buffer name)))
      (cond 
       (buffer
	(set-buffer buffer)
	(buffer-substring-no-properties 1 (or size (point-max))))
       ((file-exists-p name)
	(setq buffer (get-buffer-create name))
	(set-buffer buffer)
	(insert-file-contents-literally name nil)
	(prog1 (buffer-substring-no-properties 1 (or size (point-max)))
	  (kill-buffer buffer)))
       ))
    )
  )

(defun file-contains-pattern (file pat &optional limit)
  (let* ((fc (file-contents file limit)))
    (string-match-string pat fc)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bulkload-1 (file)
  (cond ((file-contains-pattern file "## *nobulkload *##"))
	((load file))
	)
  )

(defun bulkload () (interactive)
  (let* ((d default-directory)
	 (files (directory-files "." nil ".el"))
	 )
    (setq load-path (cons dir load-path))
    (mapcar 'bulkload-1 files)
    (setq load-path (cdr load-path))
    )
  nil
  )

(defun bulkload-directory (dir)
  (let* ((dir (expand-file-name dir))
	 (default-directory dir)
	 (files (directory-files "." nil ".el"))
	 )
    (setq load-path (cons dir load-path))
    (mapcar 'bulkload-1 files)
    (setq load-path (cdr load-path))
    )
  nil
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro swx-other (&rest forms) `(save-window-excursion (other-window 1) ,@forms))

