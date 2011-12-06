(defun get-lorder-list (files)
  (let* ((a (unconcat-lines
	     (call-shell (concat "lorder " files " | sort -u")))))
    (delete-if '(lambda (x) (eq (car x) (cadr x)))
     (mapcar
      '(lambda (x) 
	 (mapcar
	  '(lambda (x)
	     (intern (substring x 0 -2))) (unconcat x " +"))) a)
     )
    ))

(defun group-head (list)
  (let (out)
    (while list
      (let* ((a (nth 0 (car list)))
	     (b (nth 1 (car list))) 
	     (elt (assoc a out)))
	(cond
	 (elt (setcdr elt (cons b (cdr elt))))
	 ((setq out (cons (list a b) out))))
	(setq list (cdr list)))
      ) out)
  )

(defun group-tail (list) (group-head (reverse (mapcar 'reverse list))))

(defun format-group (list cap-format &optional n w)
  (setq n (or n 7))
  (setq w (format "%%-%ds" (or w 10)))
  (mconcat
   (mapcar '(lambda (x)
	      (concat 
	       (format cap-format (car x))
	       "\n    "
	       (cat
		(mapcar '(lambda (x) (cat (mprin x w)))
			(chop-list (cdr x) n))
		"\n    "))) list) "\n\n")
  )

(defun format-group-pair (list)
  (mconcat
   (mapcar '(lambda (x)
	      (mconcat
	       (mapcar '(lambda (x) (prin x wf)) x) ""))
	   (chop-list list n))
   "\n    ")
  )

(defun format-group-pairs (uses used &optional n w)
  (let* ((n (or n 7))
	 (w (or w 10))
	 (wf (format "%%-%ds" w))
	 (list (mapcar2 'list uses used))
	 )
    (mconcat
     (mapcar '(lambda (x)
		(concat 
		 (format "%s uses:\n    " (caar x))
		 (format-group-pair (cdar x))
		 (format "\n\n%s used by:\n    " (caar x))
		 (format-group-pair (cdadr x))
		 )) list) "\n\n\n")
    )
  )

(defun symbol-lessp (a b)
  (string-lessp (symbol-name a) (symbol-name b)))

(defun lorder (list)
  (setq lorder-list (get-lorder-list list))
  (setq uses (group-head (reverse lorder-list)))
  (setq used (sort (group-tail lorder-list)
		      '(lambda (a b) (symbol-lessp (car a) (car b)))))
  (setq modules (mapcar 'car used))
  )

(defun read-module (prompt)
  (intern
   (completing-read
    prompt (mapcar 'list (mapcar 'symbol-name module-list)) nil t)))

(defun uses (module)
  (interactive "P")
  (cond ((interactive-p) (setq module (read-module "Module uses: "))))
  (cdr (assoc module uses)))

(defun used (module)
  (interactive "P")
  (cond ((interactive-p) (setq module (read-module "Modules used by: "))))
  (cdr (assoc module used)))

 
