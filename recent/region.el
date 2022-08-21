(defun region-size ()
  (cond
   ((region-active-p) (- (region-end) (region-beginning)))
   (0)
   ))
    

(defun region-or-thing (&optional thing)
  (setq thing (or thing 'sexp))
  (cond ((region-active-p) (region-text))
	((thing-at-point thing))
	)
  )

(defun bounds-of-region-or-thing (&optional thing)
  (setq thing (or thing 'sexp))
  (cond
   ((> (region-size) 0) (cons (region-beginning) (region-end)))
   ((bounds-of-thing-at-point thing))
   )
  )

(defun region-or-thing-adorn (l &optional r thing)
  (let* ((zone (bounds-of-region-or-thing thing))
	 (r (or r l))
	 )
    (sx
     (goto-char (cdr zone)) (insert r)
     (goto-char (car zone)) (insert l)
     )
    )
  )

