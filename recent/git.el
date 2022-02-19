(defun vc-git--quick-commit (m) (interactive "sMessage: ")
  (vc-git--call nil "commit" "-m" m)
  )

(def-key c-rbracket-map (kbd "C-c") 'vc-git--quick-commit)

(defun magit-buffer-open-p (&optional directory)
  (get-buffer (format "magit: %s" (filename-directory-last)))
  )

(defun magit-refresh-if-open (&optional directory)
  (interactive "Ddirectory: ")
  (let ((buffer (magit-buffer-open-p directory)))
    (cond (buffer
	   (set-buffer buffer)
	   (magit-refresh)
	   )
	  )
    )
  )

(defvar git-stage-and-commit-refresh nil "Refresh magit after git-stage-and-commit etc")

(defun git-stage-and-commit (file message &optional arg)
  (interactive (list
		(read-from-minibuffer
		 "File: " (file-name-nondirectory (buffer-file-name)) nil nil
		 )
		(read-from-minibuffer
		 "Message: " (format "%s " (file-name-nondirectory (buffer-file-name)))
		 )
		(prefix-numeric-value current-prefix-arg)
		))
  (let* ((refresh 
	  (cond
	   (git-stage-and-commit-refresh)
	   ((eq arg 4))
	   )
	  )
	 )
    (debug)
    (git-call (format "add \"%s\"" file))
    (git-call (format "commit -m \"%s\"" message))
    (cond
     (refresh (magit-refresh-if-open))
     )
    )
  )

(define-key global-map (kbd "C-x C-v C-c") 'git-stage-and-commit)

