(defun pc-other-file (&optional expand)
  (let ((file
	 (filename-replace-suffix
	  (cdr (car (assoc-re (file-name-suffix)
			      '((".pc" . ".c"))))))))
    (if expand (expand-file-name file) file)
    )
  )

(defun pc-mode () (interactive)
  (c-mode)
  (setq other-file-function 'pc-other-file)
  )

(setq compile-save-modes (cons 'pc-mode compile-save-modes))

