;#~:panic kill all buffers

; moved from recent.el

(set-default 'panic-immune nil)
(make-variable-buffer-local 'panic-immune)

(setq panic-immune-mode-alist `((shell-mode . t)))

(defun panic (&optional kill) (interactive "P")
  "Abandon all changes to buffers except those where local variable 'panic-immune' is
non-nil"
  (let ((list (buffer-list)))
    (mapcar '(lambda (x)
	       (set-buffer x)
	       (cond
		((not (buffer-file-name)))
		((alist-get panic-immune-mode-alist 'major-mode))
		(panic-immune nil)
		(t
		 (cond ((buffer-modified-p)
			(revert-buffer t t)
			(message "reverting %s" x)
			))
		 (and kill (kill-buffer x))
		 )
		)
	       )
	    list)
    ))

(defun kill-all () (interactive)
  (let ((list (buffer-list)))
    (mapcar
     '(lambda (x)
	(set-buffer x)
	(cond
	 (panic-immune nil)
	 ((alist-get panic-immune-mode-alist major-mode))
	 ((cond
	   ((and (buffer-file-name) (buffer-modified-p))
	    (if (y-or-n-p (format "%s modified - save ?" (buffer-file-name)))
		(save-buffer)
	      (revert-buffer t t))
	    (kill-buffer nil)
	    )
	   ((kill-buffer nil))
	   ))
	 )
	)
     list)
    ))

(defun panic-kill (&optional arg) (interactive "P")
  (cond ((or arg (y-or-n-p "Confirm ? "))
	 (panic)
	 (kill-all)
	 )
	)
  )
