(defun region-size ()
  (cond
   ((region-active-p) (- (region-end) (region-beginning)))
   (0)
   ))
    

(defun region-or-thing-region ()
  (cond
   ((> (region-size) 0) (cons (region-beginning) (region-end)))
   ((bounds-of-thing-at-point 'sexp))
;   ((sx (cons (sxp (beginning-of-sexp)) (sxp (end-of-sexp)))))
   )
  )

