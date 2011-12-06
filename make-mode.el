(load-standard "make-mode")

(define-key makefile-mode-map [f9] 'makefile-make-at-point)

(defun makefile-make-at-point () (interactive)
  (compile (format "make -f %s %s"
		   (file-name-nondirectory (buffer-file-name))
		   (at-point "^[^ \t\n:]+")
		   )
	   )
  )

(defun at-point (regexp &optional n flimit blimit)
  (or flimit (setq flimit (point-max)))
  (or blimit (setq blimit (point-min)))
  (save-excursion
    (cond
     ((and (looking-at regexp) (not (sx (fc -1) (looking-at regexp))))
      )
     ((rsb regexp blimit))
     )
    (rsf regexp flimit)
    (buffer-substring (match-beginning (or n 0)) (match-end (or n 0)))
    )
  )

