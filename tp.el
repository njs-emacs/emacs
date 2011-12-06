; transpose parts of expression based on underscore position

(defun rotate (list &optional n)
  (let* ((l (length list))
	 (n (or n 1))
	 (head (reverse (nthcdr n list)))
	 (tail (nthcdr (- l n) (reverse list)))
	 )
    (nreverse (nconc tail head))
    )
  )

(defun tp (s &optional n) (let ((x (unconcat s "_")))
		(cat (rotate x n) "_")))

(defun tp-sexp (&optional n) (interactive)
  (let* ((end (sxp (fx 1)))
	 (start (sxp (fx -1)))
	 (s (buffer-substring start end))
	 )
    (sx
     (delete-region start end)
     (insert (tp s n))
     )
    )
  )
(defun mouse-tp-sexp (arg) (interactive "e")
  (mouse-set-point arg)
  (tp-sexp)
  )
;(define-key global-map [?\C-x C-mouse-1] 'mouse-tp-sexp)

