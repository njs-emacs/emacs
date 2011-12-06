(defun mapcars (fun lists)
  "Apply FUNCTION to each of the elements of LISTS in turn.
The result is a list just as long as the longest of the lists.
\n(n-lisp function)"
  (mapcar (function (lambda (x) (apply fun x))) (remap lists)))

(defun mapcarn (f list n)
  "Apply FUNCTION to each n elements from LIST until all are gone.
\n(n-lisp function)"
  (let (result)
    (while list
      (setq result
	    (cons (apply f
			 (let ((i n) args)
			   (while (> i 0)
			     (setq args (cons (car list) args)
				   i (1- i)
				   list (cdr list)))
			   (nreverse args))) result)))
    (nreverse result)))


