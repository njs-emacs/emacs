(defmacro setq-noisy (sym val)
  `(let ((v ,val)) (setq ,sym v) (message "%s is set to %s" ',sym v))
  )

(defun safe-funcall (&rest args)
  (condition-case e (apply 'funcall args)
;    (error (format "<error %s>" e))
    (error "error")
    )
  )

(defun safe-substring (string start end)
  (let ((n (length string)))
    (cond ((> end 0)
	   (setq end (min end n))
	   )
	  )
    (substring string start end))
  )

(defun eprogn (form) (eval (cons 'progn form)))

(defun sym (x)
  (cond ((symbolp x) x)
	((stringp x) (intern x))
	))

(defun cars-eq (x y) (eq (car x) (car y)))
(defun cars-equal (x y) (equal (car x) (car y)))

(defun less (a b)
 "A and B are strings. Return true if A < B where if A and B are
numeric strings, use their numeric values. Otherwise return (string< A B)."
  (cond
   ((and (>= (aref a 0) ?0) (<= (aref a 0) ?9))
    (let* ((a (string-to-int a))
	   (b (string-to-int b)))
      (< a b)))
   ((string< a b))
   ))

(defmacro ++ (*x) `(setq ,*x (1+ ,*x)))
(defmacro -- (*x) `(setq ,*x (1- ,*x)))

(defmacro *++ (*x) `(prog1 ,*x (setq ,*x (1+ ,*x))))
(defmacro *-- (*x) `(prog1 ,*x (setq ,*x (1- ,*x))))

(defun flatten (&rest list)
  (let ((fun '(lambda (list)
		(cond ((null list) list)
		      ((atom list) (list list))
		      (t (apply 'append (mapcar fun list)))))))
    (funcall fun list)))

(defun remap (lists)
  "Take a set of LISTS and return another set of lists containing the
n'th members of the original lists in member n.
The lists are nil padded to the length of the longest list.
\n(n-lisp function)"
  (let ((n (apply 'max (mapcar 'length lists)))
	result)
    (while (> n 0) (setq n (1- n))
      (setq result (cons (mapcar 
			  (function (lambda (x) (nth n x)))
			  lists) result)))
  result))

(defmacro mcar (args lists &rest body)
  (let* ((result nil)
	 (rlists (remap lists))
	 (fun (apply 'list 'lambda args body))
	 )
    (while rlists
      (setq result (cons (apply fun (car rlists)) result))
      (setq rlists (cdr rlists))
      )
    (cons 'list (nreverse result))
    )
  )

(defun partition (list fun &rest args)
  "Partition LIST into disjoint sets, where each set is a list of members
who evaluate function FUN to the same value.
Each set has that value as it's car.
The remaining arguments are passed to FUN each time it is evaluated."
  (let (result (list (reverse list)))
    (while list
      (let* ((x (apply fun (car list) args))
	     (i (assoc x result)))
	(or i (setq result (cons (setq i (list x)) result)))
	(setcdr i (cons (car list) (cdr i)))
	(setq list (cdr list))
	)
      ) result))

(defun make-ring-list (list)
  "Make LIST into a circular ring. You won't be able to print it anymore!"
  (setcdr (nthcdr (1- (length list)) list) list) nil)

(defun list-rotate (list &optional n)
  "Rotate LIST towards its cdr N times."
  (let* ((n (or n 1))
	 (head (nthcdr n list))
	 (tail (nreverse (nthcdr n (reverse list))))
	 )
    (setcdr (nthcdr (1- n) tail) nil)
    (append head tail)
    )
  )

(defmacro mvb (var exp &rest body)
  "Multiple Value Bind. Like (let). Bind all the symbols in VAR to the
values in EXP, and execute BODY."
  `(apply '(lambda ,(append var (list '&rest 'junk)) ,@body)
	    ,exp))

(defun max-length (&rest x) (apply 'max (mapcar 'length x)))

(defmacro with-special-binding (fun binding &rest body)
  (let ((tmp (symbol-function fun)))
    (fset fun (or (eval binding) '(lambda (&rest junk))))
    (prog1 (condition-case arg
	       (eval (cons 'progn body)))
      (fset fun tmp)
      )
    nil))

(defun not-all-equal (list)
  (let ((car (car list)))
    (while (and (setq list (cdr list)) (equal (car list) car))))
  list)

(defun mapcdr (fun list &optional funcall)
  (let (out)
    (while list
      (setq out (cons (funcall (or funcall 'funcall) fun list) out))
      (setq list (cdr list))
      )
    (nreverse out)))

(defmacro mdotimes (head &rest body)
  `(let ((result)) (dotimes ,(append head '((nreverse result)))
		     (setq result (cons ,@body result))
		     ))
  )
(defin 'mdotimes)

(defmacro mdolist (head &rest body)
  `(let ((result)) (dolist ,(append head '((nreverse result)))
		       (setq result (cons ,@body result))
		       ))
  )
(defin 'mdolist)

(defun mdi (n) (mdotimes (i n) i))

(modify-syntax-entry ?_ "w")

(defun find-mismatch () (interactive)
  (bob)
  (while t (forward-sexp 1))
  )

(defun debug1 () (or (get-buffer "*Backtrace*") (debug)))
(defmacro progd (&rest body) (cons 'progn (cons (list 'debug) body)))

(defmacro proge (&rest body)
  (append (list 'condition-case 'e) body
	  (list (list 'error (list 'trap-error 'e)))))

(defun eval-string (*s) (eval (read *s)))

(defvar pre-eval-hook nil)
(defvar post-eval-hook nil)
(make-local-variable 'pre-eval-hook)
(make-local-variable 'post-eval-hook)

(defun eval-defun-without-narrow (arg) (interactive "P")
  "Eval the preceding defun, without narrowing the current buffer."
  (let* ((*s
	  (cond ((save-excursion (beginning-of-line) (looking-at "\\sw"))
		 (buffer-substring (point^) (point$)))
		((save-excursion (beginning-of-line) (looking-at "[;#]+"))
		 (buffer-substring (match-end 0) (point$)))
		((save-excursion (beginning-of-line) (looking-at "^/\\*"))
		 (beginning-of-line) (rsf "\\*")
		 (prog1 (buffer-substring (point) (sxp (fx 1)))
		   (eol)))
		((buffer-substring
		  (save-excursion (beginning-of-defun) (point))
		  (save-excursion (end-of-defun) (point))))))
	 (*x (read *s))
	 )
    (dolist (i pre-eval-hook) (funcall i *x))
    (cond
     ((numberp arg)
      (show (%% (setq eval-result (eval *x)) arg)))
     ((equal arg '(4))
      (setq eval-result (eval (list 'progd *x))))
     ((setq eval-result (eval (list 'princ *x))))
     )
    (dolist (i post-eval-hook) (funcall i *x eval-result))
    )
  eval-result
  )

(defun insert-result (arg) (interactive "P")
  (insert (%% (eval (read (car kill-ring))) arg)))

(defun show-result (arg) (interactive "P")
  (show (%% eval-result arg)))

(defun read-buffer-sexp (&optional buf)
  (sx
   (and buf (set-buffer buf))
   (bob)
   (let (result)
     (while (rsf "[^\n\t ]" nil nil 0 t) (setq result (cons (readc) result)))
     (nreverse result))))
    
(defun eval-sexp (&optional replace)
  (let ((x (eval (sx (readc)))))
    (cond (replace
	   (kill-sexp 1)
	   (insert (format "%s" x))
	   ))))

(defun eval-c-buffer () (interactive)
  (save-excursion
    (bob)
    (while (rsf "/\\*(" nil t)
      (backward-char 1)
      (eval (read (current-buffer)))
      )
    )
  )

(defun eval-makefile-buffer () (interactive)
  (save-excursion
    (bob)
    (while (rsf "#(" nil t)
      (backward-char 1)
      (eval (read (current-buffer)))
      )
    )
  )

(setq lisp-sexp-prefix-regexp "/\\*[^(]*(")

(defun eval-other-buffer ()
  (while (rsf lisp-sexp-prefix-regexp)
    (bc 1)
    (eval (readc))))

(defun push-exit-hook (hook)
  (setq eval-buffer-exit-hooks (cons hook eval-buffer-exit-hooks)))

(defvar eval-buffer-function nil "Local buffer eval function")
(make-variable-buffer-local 'eval-buffer-function)

(put 'emacs-lisp-mode 'eval-buffer-modal 'eval-buffer)
(put 'ps-mode 'eval-buffer-modal 'eval-ps-buffer)
(put 'text-mode 'eval-buffer-modal 'eval-text-buffer)
(put 'c-mode 'eval-buffer-modal 'eval-c-buffer)
(put 'makefile-mode 'eval-buffer-modal 'eval-makefile-buffer)
				
(defun eval-buffer-modal (&optional arg) (interactive "P")
  (setq eval-buffer-exit-hooks nil)
  (let* (result
	 (pre (get major-mode 'pre-eval-buffer-hook))
	 (fun (get major-mode 'eval-buffer-modal))
	 (post (get major-mode 'post-eval-buffer-hook)))
     (and arg (debug))
     (run-hooks pre)
     (setq result
	   (cond
	    (eval-buffer-function (funcall eval-buffer-function))
	    (fun (funcall fun))
	    (t (eval-other-buffer))
	    ))
     (run-hooks 'eval-buffer-exit-hooks)
     (run-hooks 'post)
     result
     ))

(defun load-or-eval (file)
  (or (eval-buffer file) (load-library file)))

(defun toggle-debug-on-error () (interactive)
  (setq-noisy debug-on-error (not debug-on-error)))

(defun find-daily-tmp-lisp-file () (interactive)
  (if (not (boundp 'tmp-lisp-file))
      (setq tmp-lisp-file
	    (format "%s/tmp/%s.el" user-emacs-home
		    (substring (call-shell "date +%y%m%d") 0 -1))))
  (find-file tmp-lisp-file))

(add-hook 'emacs-lisp-mode-hook
  '(lambda ()
     (setq grep-spec "*.[pe]l")
     )
  )

;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gget1 (list tag1 tag2)
  "Generalized depth-1 alist get"
  (gget (gget list tag1) tag2)
  )

(defun gput1 (list tag1 tag2 value)
  "Generalized depth-1 alist put"
  (let ((cell (assoc tag1 list)))
    (cond
     (cell (setcdr cell (gput (cdr cell) tag2 value)) list)
     (t (cons (cons tag1 (list (cons tag2 value))) list))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq l-map (make-sparse-keymap))

(define-key l-map "b" 'eval-buffer)
(define-key l-map "r" 'eval-region)
(define-key l-map "k" 'top-level)

(define-key l-map "e" 'toggle-debug-on-error)
(define-key l-map "l" 'load-library)
(define-key l-map "t" 'find-daily-tmp-lisp-file)
(define-key l-map "m" 'find-mismatch)

(define-key l-map "\M-i" 'insert-result)

(define-key l-map "\M-s" 'show-result)

(define-key global-map "\el" l-map)

(define-key global-map "\e\C-M" 'eval-defun-without-narrow)

