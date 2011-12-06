(defun ffset (fun) (fset fun (intern (format "%s:%s" fun c-version))))

(defun set-c-version (sym)
  (setq c-version sym)
  (ffset 'cdefun-ext)
  (ffset 'cdefun-hdr)
  (ffset 'cdefun-hdr)
  )
(set-c-version 'ansi)

(defun c-fun-style ()
  (let* ((args (c-args))
	 (car (unconcat (car args))))
    (case (length car)
      ((1) 'kr)
      (t 'ansi))
    ))

(defun proto-kr (fun)
  (format "%s(%s)\n%s" (vprin fun)
	  (cat (mapcar 'cadr (args fun)) ",")
	  (cat (mapcar 'vprin-t (args fun)) "\n" "    %s ;")))

(defun cdefun-hdr:kr (fun)
  (let ((args (args fun)))
    (format "%s%s %s(%s) %s"
	    (if (cg-prop fun 'x) "extern " "static ")
	    (type fun) (name fun)
	    (mapconcat '(lambda (x) (format "%s" (nth 1 x))) args ",")
	    (mapconcat '(lambda (x) (apply 'format "%s %s ;" x)) args " "))
    )
  )

(defun cdefun-ext:kr (fun)
  (format "extern %s %s() ;\n" (type fun) (name fun))
  )

(set-c-version 'kr)
