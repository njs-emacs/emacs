(setq xlat-ctype-alist
      '(("unsigned long" "u_long")
	("unsigned int" "u_int")
	("unsigned char" "u_char")
	("unsigned short" "u_short")
	))
	
(defun xlat-ctype (w)
  (and w (or (eval (apply 'list 'case-match w xlat-ctype-alist)) w)))

(defun pdecl (form &optional partial deep)
  "print function declaration with names of arguments"
  (let ((car (car form)))
    (cond
      ((or (eq car '$) (eq car '$k) (eq car '$a))
       (format (if deep "%s (*%s)(%s)" "%s %s(%s)")
	       (pdecl (cadr form) nil t) (or partial "")
	       (pdecl-args (cddr form))))
      ((eq car '*)
       (format "%s *%s" (pdecl (cadr form) nil t) (or partial "")))
      ((concat (xlat-ctype car) (if partial (format " %s" partial) "")))
      ))
  )

(defun pdecl-cq (form &optional partial deep)
  "print function declaration with names of arguments"
  (let ((car (car form)))
    (cond
      ((eq car '*)
       (format "%sp %s" (pdecl-cq (cadr form) nil t) (or partial "")))
      ((concat (xlat-ctype car) (if partial (format " %s" partial) "")))
      ))
  )

(defun pdecl-decl (x) (pdecl (cadr x) (nth 3 x)))

(defun pdecl-args (list)
  (mconcat (mapcar '(lambda (x) (pdecl (car x) (caddr x) t))
		   list) ",")
  )

(defun pdecl-args-noname (list)
  (mconcat (mapcar '(lambda (x) (pdecl (car x) nil t))
		   list) ",")
  )

(defun pp-fun (fun list)
  (format "%s(%s)" fun (pdecl-args list))
  )

(defun region-c-function-header (name)
  (up-list (depth))
  (fx -1)
  (let ((end (point-marker))
	(start (sx (rsb (format "\\b%s\\b" name)) (bol) (point-marker))))
    (list start end)
    ))

(defun rep-decl (decl name)
  (let* ((r (region-c-function-header name)))
    (comment-region r)
    (goto-char (car r))
    (insert (pdecl decl name) "\n")
    ))

(defun make-fun-proto (name &optional buf)
  (wcompile (format "make -i info CWARN=\"\" obj=%s.o\
 DCARGS=\"-z1file-pos -z1function-def -z1lisp\"" (basename name))
	    buf))

(defmacro let-fun (&rest body)
  `(let* ((form (read msg))
	  (class (nth 2 form))
	  (name (nth 3 form))
	  (decl (nth 1 form))
	  (type (caadr form))
	  )
     ,@body))

(defmacro let-call (&rest body)
  `(let* ((form (read msg))
	  (from (nth 1 form))
	  (to (nth 2 form))
	  (argv (nthcdr 3 form))
	  (argc (length argv))
	  )
     ,@body))

