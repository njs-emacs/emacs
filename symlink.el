(defun chars-to-string
  (&rest list) (apply 'concat (mapcar 'char-to-string list)))

(defun delete-char-from-string (s c)
  (apply 'chars-to-string
	 (delete c (string-to-list s))
	 ))

(defun symlink-follow ()
  (cond
   ((> (length (buffer-string)) 11)
    (let* ((s (buffer-substring 1 11)))
      (cond
       ((string= s "!<symlink>")
	(let* ((s (bs 13 (sxp (bob) (eol))))
	       (file (delete-char-from-string s 0))
	       )
	  (find-alternate-file file)
	  (message "symlink to %s" file)
	  )
	)
       )
      )
    )
   )
  )

(add-hook 'find-file-hook 'symlink-follow)
