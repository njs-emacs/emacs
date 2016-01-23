(defun org-next-agenda-file ()
  "Extracted from org-cycle-agenda-files. Returns the file
that org-cycle-agenda-files would have visited."
  (let* ((fs (org-agenda-files t))
	 (files (append fs (list (car fs))))
	 (tcf (if buffer-file-name (file-truename buffer-file-name)))
	 file)
    (unless files (user-error "No agenda files"))
    (catch 'exit
      (while (setq file (pop files))
	(if (equal (file-truename file) tcf)
	    (when (car files)
	      (throw 'exit (car files)))))
      (car fs))
    )
  )

(defun org-prev-agenda-file ()
  "Like org-next-agenda-file, but previous instead of next."
  (let* ((fs (org-agenda-files t))
	 (files (append fs (list (car fs))))
	 (tcf (if buffer-file-name (file-truename buffer-file-name)))
	 file)
    (unless files (user-error "No agenda files"))
    (setq files (reverse files))
    (catch 'exit
      (while (setq file (pop files))
	(if (equal (file-truename file) tcf)
	    (when (car files)
	      (throw 'exit (car files)))))
      (car fs))
    )
  )

(file-class-linked-file-add 'org-mode '(
					(next . org-next-agenda-file)
					(prev . org-prev-agenda-file)
					)
			    )


