(defun out-c-decl (class type namelist)
  (let ((n (mapcar '(lambda (x) (apply 'concat x)) namelist)))
    (mconcat (delq nil (list class type (mconcat n ","))) " "))
  )

(defun out-c-fun-decl:ansi (fun)
  (format "%s(%s)"
   (apply 'out-c-decl (car fun))
   (mconcat (mapcar '(lambda (x) (apply 'out-c-decl x)) (cdr fun)) ",")
   ))

(defun out-c-fun-decl:kr (fun)
  (format "%s(%s)\n%s"
   (apply 'out-c-decl (car fun))
   (mconcat (apply 'append (mapcar '(lambda (x) (mapcar '(lambda (y) (nth 1 y)) (nth 2 x))) (cdr fun))) ",")
   (mconcat (mapcar '(lambda (x)
		       (format "    %-12s%s%s ;\n"
			       (nth 1 x)
			       (nth 0 (car (nth 2 x)))
			       (nth 1 (car (nth 2 x))))) (cdr fun)))
   ))

(defun out-c-fun-decl-ext:ansi (fun)
  (format "%s(%s)"
   (apply 'out-c-decl (car fun))
   (mconcat (mapcar '(lambda (x) (apply 'out-c-decl x)) (cdr fun)) ",")
   ))

(defun out-c-fun-decl-ext:kr (fun)
  (format "%s(%s)"
   (apply 'out-c-decl (car fun))
   (mconcat (apply 'append (mapcar '(lambda (x) (mapcar '(lambda (y) (nth 1 y)) (nth 2 x))) (cdr fun))) ",")
   ))

