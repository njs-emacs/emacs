(defun cs-name (s) (string-sub s "\\Sw" "_"))

(defun cs-exp (s)
  "return non-nil if STRING is a C expression, as opposed to an atom"
  (cond
   ((string-match "-[^>]\\|[:?+*=&^|<%]\\|[^-]>\\|\\sw+\\s *(" s) t)
   (nil)))

(defun cs-cast (s)
  "return the cast of expression, or nil"
  (cond
   ((string-match "^[ \t(]*(\\(\\sw\\|[* \t]\\)*)" s)
    (car (string-parse-r s '(1))))
   (nil)))

(defun cs-args ()
  (sx
   (rsf "(" nil t 0 t)
   (cond
    ((looking-at "(\\s *)") nil)
    ((looking-at "(\\s *void\\s *)") nil)
    ((sx
      (let* ((start (point))
	     (end (1- (sxp (forward-sexp 1))))
	     (p (sxp (rsf "(\\s *")))
	     (pos))
	(while (rsf "\\s *,\\s *" end t)
	  (let ((x (parse-partial-sexp start (point))))
	    (and (not (nth 3 x)) (= (nth 0 x) 1)
		 (setq pos (cons (list p (mb 0)) pos))
		 (setq p (me 0)))
	    ))
	(mapcar '(lambda (x) (strip-white (apply 'buffer-substring x)))
		(nreverse (cons (list p end) pos)))
	))))
   ))
(defun cs-argc () (length (cs-args)))

(defun cs-fun-start (&optional name)
;;; want to retire non inline function args code
  (sxp
   (cond
    (name
      (bob)
      (rsf (format "^\\(\\sw\\|\\s \\)+%s\\s *(\\(\\sw\\|\\s \\|[,*]\\)*)"
		   name) nil t 0 t)
      (rsf "^\\sw")
      (bol))
    (t   
     (up-list (- (depth)))
     (rsb "^\\sw[^;{/(]*(\\(\\sw\\|\\s \\|[,*]\\)*)" nil t)
     (while (sx (fl -1) (looking-at "^\\sw")) (fl -1))
     ))
   ))

(defun cs-fun-name ()
  "return the name of the function which point is inside."
  (sx (goto-char (cs-fun-start))
      (fms "\\(\\sw*\\)[ \t\n]*(" 1)))

(fset 'cdecl-type 'caddr)
(fset 'cdecl-name 'cadr)
(defun cdecl-format (x) (nth 3 x))

(defun cdecl-info (x) (nthcdr 4 x))
(defun cdecl-class (x)  (car (or (assoc 'extern x) (assoc 'static x))))

(defun cf-local-p (x) (member (car x) '(local variable)))
(defun cf-locals (fun) (subset-if 'cf-local-p (cdecl-info fun)))
(defun cf-local (fun name)
  (car (member-if '(lambda (x) (eq (nth 1 x) name)) (cf-locals fun))))

(defun cf-arg-p (x) (eq (car x) 'arg))
(defun cf-args (fun) (subset-if 'cf-arg-p (cdecl-info fun)))
(defun cf-arg (fun name)
  (car (member-if '(lambda (x) (eq (nth 1 x) name)) (cf-args fun))))

(defun cf-locals-not-arg (fun)
  (delete-if '(lambda (x) (cf-arg fun (cadr x))) (cf-locals fun)))

(setq cdecl-id-regexp
      "\\(\\Sw*\\)\\(\\sw*\\)\\(.*\\)")

(defun cdecl-id (s)
  (mvb (a b c) (string-parse s cdecl-id-regexp 1 2 3)
    (list (read b) (string-sub (concat a "%s" c) "\\s +" ""))))

(setq cdecl-regexp
      "\\(extern\\|static\\|\\)\\s *\\(struct\\|enum\\|union\\|\\)\\s *\\(\\sw+\\)\\([a-zA-Z0-9_ \t\n*()]+\\)")

(defun cdecl (s)
  (mvb (class cons tag id) (string-parse s cdecl-regexp 1 2 3 4)
    (let ((id (cdecl-id id)))
      (apply 'list (car id)
	     (strip-white (cond (cons (concat cons " " tag)) (tag)))
	     (cadr id)
	     (and (> (length class) 0) (list (list (read class))))
	     ))))

(defun cdecl-fun ()
  (goto-char (cs-fun-start))
  (let* ((args (cs-args))
	 (type (cdecl (bs (point) (sxp (rsf "(" nil t 0 t)))))
	 )
    (append
	   (cons 'cfun type)
     (mapcar '(lambda (x) (apply 'list 'arg (cdecl x))) args)
     )))

(defun prin-cdecl-id (x &optional format)
  (format (or format "%s %s")
	  (cdecl-type x) (format (cdecl-format x) (cdecl-name x))))

(defun prin-cdecl-id-s (x) (prin-cdecl-id x))

(defun prin-cdecl-id-t (x) (prin-cdecl-id x "%-20s%s"))

(defun prin-cdecl-fun-proto- (fun print &optional class)
  (concat
   (and (or class (setq class (cdecl-class fun))) (prin class "%s "))
   (format "%s(%s)" (prin-cdecl-id-s fun)
	   (cat (mapcar print (cf-args fun)) ","))
   ))

(defun prin-cdecl-fun-proto (fun &optional class)
  (prin-cdecl-fun-proto- fun 'prin-cdecl-id-s class))
(defun prin-cdecl-fun-proto-type-only (fun &optional class)
  (prin-cdecl-fun-proto- fun 'cdecl-type class))

(defun prin-cdecl-fun (fun)
  (concat (prin-cdecl-fun-proto fun) "\n{\n"
	  (cat (mapcar 'prin-cdecl-id-t (cf-locals-not-arg fun)) "" "    %s ;\n")
	  "    }\n\n"))

(setq cfun-list nil)

(defmacro cfun (name type class &rest info)
  (set name (apply 'list 'cfun name type class info))
  (setq cfun-list (cons name cfun-list))
  (message (format "(cfun %s ...)" name))
  nil
  )

