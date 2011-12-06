(defun plain-comment () (interactive)
  (bob)
  (while (rsf "//" nil t)
    (bdc 2)
    (insert "/*")
    (eol)
    (insert "*/")
    )
  )

(defun in-comment ()
  (sx (let* ((p (point))
	     (start (rsb "/\\*"))
	     (end (and start (sx (rsf "\\*/")))))
	(and start end (< p end) (> p start))))
  )

(defun delete-comment ()
  "Delete the comment starting at or before point."
  (delete-region (sxp (fc 1) (rsb "/\\*")) (sxp (rsf "*/"))))

(setq comment-seq 0)
(setq comment-format "%s")

(defun insert-comment ()
  (insert "//" (format comment-format comment-seq))
  )

(defun comment-region (r)
  (let ((start (car r)) (end (cadr r)))
    (goto-char start)
    (while (< (point) end)
      (insert-comment)
      (fl 1)
      (bol)
      )
    ))

  
