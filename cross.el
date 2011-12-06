(defun cross-2 (new partial)
  "Produce a cross product list of 2 lists NEW and PARTIAL."
  (let (x)
    (while new
      (let ((i partial))
	(while i
	  (setq x (cons (cons (car new) (car i)) x))
	  (setq i (cdr i))))
      (setq new (cdr new)))
    (nreverse x)))

(defun cross (a &rest others)
  "Produce a cross product list of n lists."
  (let ((aa (mapcar 'list a)))
    (while others
      (setq aa (cross-2 (car others) aa))
      (setq others (cdr others)))
    (mapcar 'reverse aa)))

(defun cross-eval (syms a &rest others)
  "Produce evaluated crossed product list.
Each member of SYMS is bound to the values in the LISTs and the 
expressions are evaluated"
  (let* ((cross (apply 'cross a others))
	 (fun (list
	       'mapcar
	       (list
		'quote
		(list
		 'lambda
		 (list 'VARLIST)
		 (list
		  'apply
		  (list
		   'quote
		   (list
		    'lambda
		    (append syms '(&rest bullshit))
		    (list
		     'mapcar
		     ''eval
		     'VARLIST)))
		  'VARLIST)))
	       (list 'quote cross))))
    (eval fun)))

(put 'cross-eval 'lisp-indent-hook 'defun)
