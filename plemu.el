(defmacro ns-defun (name args &rest body)
  (or (fboundp name)
      (apply 'list 'defun name args body)))

(defmacro ns-defmacro (name args &rest body)
  (or (fboundp name)
      (apply 'list 'defmacro name args body)))

;(fset 'ns-defun 'defun)
;(fset 'ns-defmacro 'defmacro)

(defin 'ns-defun)
(defin 'ns-defmacro)

(ns-defun caar (x) (car (car x)))
(ns-defun cadr (x) (car (cdr x)))
(ns-defun cdar (x) (cdr (car x)))
(ns-defun cddr (x) (cdr (cdr x)))

(ns-defun caaar (x) (car (car (car x))))
(ns-defun caadr (x) (car (car (cdr x))))
(ns-defun cadar (x) (car (cdr (car x))))
(ns-defun caddr (x) (car (cdr (cdr x))))

(ns-defun cdaar (x) (cdr (car (car x))))
(ns-defun cdadr (x) (cdr (car (cdr x))))
(ns-defun cddar (x) (cdr (cdr (car x))))
(ns-defun cdddr (x) (cdr (cdr (cdr x))))

(ns-defun adjoin (elt list)
  (if (member elt list) list (cons elt list)))

(ns-defun mconcat (list &optional sep)
  (mapconcat 'identity list sep))

(ns-defun member (elt list &optional fun)
  (or fun (setq fun 'equal))
  (while (and list (not (funcall fun elt (car list))))
    (setq list (cdr list)))
  list)

(ns-defun member-if (fun list)
  (while (and list (not (funcall fun (car list)))) (setq list (cdr list)))
  list)

(ns-defun member-if-not (fun list)
  (while (and list (funcall fun (car list))) (setq list (cdr list)))
  list)

(ns-defun subset-if (fun list)
  (let ((result))
    (while list
      (cond ((funcall fun (car list))
	     (setq result (cons (car list) result))))
      (setq list (cdr list))
      )
    (nreverse result)))

(ns-defun subset-if-not (fun list)
  (let ((result))
    (while list
      (cond ((funcall fun (car list)))
	    ((setq result (cons (car list) result))))
      (setq list (cdr list))
      )
    (nreverse result)))

(defun member3 (a b c) (member a b))

(ns-defun intersection (list listb &optional fun)
  (let ((result))
    (while list
      (cond ((member3 (car list) listb fun)
	     (setq result (cons (car list) result))))
      (setq list (cdr list))
      )
    (nreverse result)))

(ns-defun set-difference (list listb &optional fun)
  (let ((result))
    (while list
      (cond ((member3 (car list) listb fun))
	    ((setq result (cons (car list) result))))
      (setq list (cdr list))
      )
    (nreverse result)))

(ns-defun union (list listb &optional fun)
  (append list (set-difference listb list fun)))

(ns-defun nunion (list listb &optional fun)
  (nconc list (set-difference listb list fun)))

(ns-defun set-exclusive-or (list listb &optional fun)
  (nconc (set-difference list listb fun)
	 (set-difference listb list fun)))

(ns-defun assoc-if (pred list)
  (car (member-if '(lambda (x) (funcall pred (car x))) list)))

(ns-defun assoc-if-not (pred list)
  (car (member-if-not (list 'lambda '(x) (list 'funcall pred '(car x))) list)))

(ns-defun delete-first-if (pred list)
  (cond ((funcall pred (car list)) (cdr list))
	(t
	 (let ((list list))
	   (while (and (cdr list) (not (funcall pred (cadr list))))
	     (setq list (cdr list)))
	   (and list (setcdr list (cddr list)))))
	list)
  )

(ns-defun delete-first-if-not (pred list)
  (delete-first-if
   (list 'lambda '(x) (list 'not (list 'funcall pred 'x))) list)
  )

(ns-defun delete-first (elt list &optional fun)
  (delete-first-if
   (list 'lambda '(x)
	 (list 'funcall (list 'quote (or fun 'equal)) 'x elt)) list)
  )

(ns-defun delete-if (pred list)
  (while (and list (funcall pred (car list))) (setq list (cdr list)))
  (let ((list list))
    (while (cdr list)
      (if (funcall pred (cadr list))
		  (setcdr list (cddr list))
		(setq list (cdr list))))
    )
  list
  )

(ns-defun delete-if-not (pred list)
  (delete-if (list 'lambda '(x) (list 'not (list 'funcall pred 'x))) list)
  )

(ns-defun delete (elt list &optional fun)
  (delete-if
   (list 'lambda '(x)
	 (list 'funcall (list 'quote (or fun 'equal)) 'x elt)) list)
  )

(defun delete-duplicates (list)
  (while (and list (member (car list) (cdr list))) (setq list (cdr list)))
  (let ((list list))
    (while (cdr list)
      (if (member (cadr list) (cddr list))
		  (setcdr list (cddr list))
		(setq list (cdr list)))
	  )
	)
  list
  )

(ns-defun count (elt list)
  )

(ns-defun count-if (elt list)
  )

(ns-defun count-if-not (elt list)
  )

(ns-defmacro dotimes (head &rest body)
  (let ((var (car head))
	(count (cadr head))
	(result (caddr head))
	)
    (list 'progn
	  (list 'let (list (list var 0))
		(append (apply 'list 'while (list '< var count) body)
			(list (list 'setq var (list '1+ var)))))
	  result
	  )
    ))

(ns-defmacro dolist (head &rest body)
  (let ((var (car head))
	(list (eval (cadr head)))
	(result (caddr head))
	)
    (list 'progn
	  (list 'mapcar (list 'quote (apply 'list 'lambda (list var) body))
		(list 'quote list)
		)
	  result)
    ))

(ns-defun mformat (list &optional fmt sep)
  (mconcat (mapcar '(lambda (x) (format (or fmt "%s") x)) list) (or sep " "))
  )
