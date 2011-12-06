;#~obsolete

(make-variable-buffer-local 'killed-buffer-list-flag)

(defun killed-buffer-list-flag-toggle () (interactive)
  (setq killed-buffer-list-flag (not killed-buffer-list-flag))
  (message "killed-buffer-list-flag is now %s" killed-buffer-list-flag)
  )

(defun kill-buffer-hook-push ()
  (let ((file (buffer-file-name)))
    (cond ((and file killed-buffer-list-flag)
	   (setq killed-buffer-list
	     (cons file (delete file killed-buffer-list))
	     )
	   )
	  )
    )
  )
	      
(defun killed-buffer-pop () (interactive)
  (cond (killed-buffer-list
	 (let ((file (car killed-buffer-list)))
	   (setq killed-buffer-list (cdr killed-buffer-list))
	   (find-file file)
	   )
	 )
	((message "No killed file buffers to pop"))
	)
  )

(setq killed-buffer-list nil)
(setq kill-buffer-hook (adjoin 'kill-buffer-hook-push kill-buffer-hook))

(setq buffer-ring nil)

(defun buffer-ring-next () (interactive)
  (cond ((null buffer-ring) (error "No buffers in buffer-ring")))
  (let ((file (car buffer-ring)))
    (setq buffer-ring (nconc (cdr buffer-ring) (list file)))
    (find-file file)
    )
  )

(defun buffer-ring-add () (interactive)
  (setq buffer-ring (adjoin (expand-file-name (buffer-file-name)) buffer-ring))
  )

(defun buffer-ring-delete () (interactive)
  (setq buffer-ring
    (delete (expand-file-name (buffer-file-name)) buffer-ring))
  )

