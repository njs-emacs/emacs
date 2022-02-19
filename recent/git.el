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

(defun git-stage-and-commit (file message)
  (interactive (list
		(read-from-minibuffer
		 "File: " (file-name-nondirectory (buffer-file-name)) nil nil
		 )
		(read-from-minibuffer
		 "Message: " (format "%s " (file-name-nondirectory (buffer-file-name)))
		 )
		))
  (git-call (format "add \"%s\"" file))
  (git-call (format "commit -m \"%s\"" message))
  (cond
   (t)
   ((magit-refresh-if-open))
   )
  )

(define-key global-map (kbd "C-x C-v C-c") 'git-stage-and-commit)

