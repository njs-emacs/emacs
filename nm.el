(defun dot-o (name) (format "%s.o" name))

(defun nm-get (file &optional filter)
  (let* ((cmd (concat "nm " file
		     (and filter (format " | %s" filter))))
	 (list (mapcar 'unconcat (unconcat-lines (shell-output cmd))))
	 (out (mapcar '(lambda (x)
			 (cond 
			  ((= (length x) 2) (list (substring (cadr x) 1)
						  (reads (car x))))
			  ((= (length x) 3) (list (substring (caddr x) 1)
						  (reads (cadr x))
						  (car x))))) list))
	 )
    out))

(defun nm-get-u (file)
  (mapcar 'car (nm-get file "grep U")))

(defun nm-ulist (a b)
  (let ((aa (nm-get-u a))
	(bb (nm-get b "grep -v U")))
    (delq nil (mapcar '(lambda (x) (and (assoc x bb) x)) aa)))
  )

(defun nm-ulist-mod (mod)
  (mapcar '(lambda (x)
	     (message (format "nm'ing %s" x))
	     (cons x (nm-ulist (dot-o mod) (dot-o x))))
	  (delq mod (copy-sequence (uses mod))))
  )

(defun nm-ulist-mod:format (stuff)
  (mapcar '(lambda (x)
	     (format "%s\n    %s\n\n" (car x)
		     (mconcat (cdr x) "\n    "))) stuff))

(defun nm-make-file ()
  (if (get-buffer "NMTAB") (kill-buffer "NMTAB"))
  (call-shell "nm -o *.o > NMTAB")
  )

(defmacro nm (&rest body)
  `(with-file "NMTAB" ,@body))

(defun nm-uses (sym)
  (let ((s (format " U _%s$" sym)))
    (nm (map-search s
	  (sx (bol) (readc))))))

(defun nm-owns (sym)
  (let ((s (format " [DdtT] _%s$" sym)))
    (nm (map-search s
	  (sx (bol) (readc))))))


