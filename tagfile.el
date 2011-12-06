(defun get-tag-info (pat)
  (let* ((a0
	  (maperrors
	    (let* ((file (buffer-name))
		   (line (count-lines (point-min) (point)))
		   (pos (point))
		   (pat (funcall pat))
		   )
	      (list file pat line pos))))
	 (a1 (partition a0 'car))
	 (a2 (mapcar '(lambda (x) (set-buffer (car x))
			(cons (car x) (mapcar 'cdr (cdr x)))) a1))
	 )
    a2))

(defun make-tags (name list)
  (make-file name
    (cat (mapcar '(lambda (x)
		    (let ((a (cat (mprin* (cddr x) "%s%s,%s\n"))))
		      (concat (format "\n%s,%d\n" (car x) (length a)) a)
		      )) list)) t))

