(defun vc-git--quick-commit (m) (interactive "sMessage: ")
  (vc-git--call nil "commit" "-m" m)
  )

(def-key c-rbracket-map (kbd "C-c") 'vc-git--quick-commit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun magit-buffer-name (&optional directory)
  (setq directory
    (or directory
	(filename-directory-last (expand-file-name (concat (magit-git-dir directory) "..")))))
  (format "magit: %s" directory)
  )

(defun magit-buffer-open-p (&optional directory)
  (get-buffer (magit-buffer-name directory))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar git-stage-and-commit-refresh nil "Refresh magit after git-stage-and-commit etc")

(defun git-get-stage-target-dwim ()
  (cond
   ((eq major-mode 'magit-status-mode) (bs$))
   ((file-name-nondirectory (buffer-file-name)))
   )
  )

(defun git-stage-and-commit (file message &optional arg)
  (interactive (list
		(read-from-minibuffer
		 "File: " (git-get-stage-target-dwim) nil nil
		 )
		(read-from-minibuffer
		 "Message: " (format "%s " (git-get-stage-target-dwim))
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
    (git-call (format "add \"%s\"" file))
    (git-call (format "commit -m \"%s\"" message))
    (cond
     (refresh (magit-refresh-if-open))
     )
    )
  )

;(define-key global-map (kbd "C-x C-v C-c") 'git-stage-and-commit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun git-call (&rest args)
  (call-shell
   (format "%s %s" vc-git-program (mconcat args " "))
   )
;  (apply 'call-process vc-git-program nil nil nil args)
  )

(defun magit-call (&rest args)
  (call-shell
   (format "%s %s" vc-git-program (mconcat args " "))
   )
  (magit-refresh-if-open)
  )

(defun magit-stage-and-commit (file message &optional arg)
  (git-stage-and-commit file message arg)
  (magit-refresh-if-open)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-last-changelog ()
  (sx (eob)
      (let ((start (rsb "=begin changelog"))
	    (end (rsf "=end"))
	    )
	(goto-char end)
	(fl -1)
	(bs$)
	)
      )
  )
	    
(defun ns-git-commit-current-from-changelog ()
  (interactive)
  (save-buffer)
  (git-call (format "add \"%s\"" (buffer-file-name)))
  (git-call (format "commit -m \"%s\"" (get-last-changelog)))
  )

(def-key global-map (kps "88") 'ns-git-commit-current-from-changelog)
;(def-key global-map (kbd "C-x C-v C-c") 'ns-git-commit-current-from-changelog)
