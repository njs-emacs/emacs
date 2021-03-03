;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-cycle-agenda-files-new ()
  "Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file."
  (interactive)
  (let* ((fs (org-agenda-files t))
	 (files (append fs (list (car fs))))
	 (tcf (if buffer-file-name (file-truename buffer-file-name)))
	 file)
    (unless files (user-error "No agenda files"))
    (catch 'exit
      (dolist (file files)
	(when (equal (file-truename file) tcf)
	    (when (cdr --dolist-tail--)
	      (find-file (cadr --dolist-tail--))
	      (throw 'exit t)))
	)
      (find-file (car fs)))
    (if (buffer-base-buffer) (org-pop-to-buffer-same-window (buffer-base-buffer)))
    )
  )

