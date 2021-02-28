(defun minn (&rest args)
  (let ((args (delete nil args)))
    (cond (args (apply 'min args)))
    )
  )

(defun rsf-or (&rest patterns)
  (let (n (form (mapcar '(lambda (pat) `(sx (rsf ,pat))) patterns)))
    (setq form `(minn ,@form))
    (setq n (eval form))
    (cond (n (goto-char n)))
    )
  )

(defmacro rsf-or (&rest forms)
  (let (n (form (mapcar '(lambda (pat) `(sx (rsf ,pat))) patterns)))
    (setq form `(minn ,@form))
    (setq n (eval form))
    (cond (n (goto-char n)))
    )
  )

(defmacro rsf-cond (&rest forms)
  (let (n (form (mapcar '(lambda (x) `(sx (rsf ,@(car x)))) forms)))
    `(quote ,form)
    )
  )

(defmacro min-cond (&rest forms)
  (let (r min minf minr)
     (while forms
       (let* ((form (pop forms))
	      (e (sx (eval (car form))))
	      )
	 (cond
	  (e
	   (cond
	    ((or (not min) (< e min))
	     (setq min e)
	     (setq minf form)
	     (setq minr (eval `(progn ,@(cdr form))))
	     )
	    )
	   )
	  )
	 )
       )
     (goto-char (or min (point-max)))
     minr
     )
  )


(shx)

; this fails because the match state is lost between searches
; and there's no way to get it back
; also need to jump to the min location on success
