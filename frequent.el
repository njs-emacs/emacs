; "\C-u" slot is reserved for qb utilities
; "\C-e" slot is reserved for environmental dirs
; "\C-o" slot is reserved for "other" files derived from current
; "\C-s" slot is reserved for search command files
;

(load "mini" t)

(setq b-map (make-sparse-keymap))
(setq qb-map (make-sparse-keymap))
(define-key global-map "\C-b" b-map)

(mdotimes (i 26)
  (let* ((c (char-to-string (- 26 i))))
    (define-key qb-map c (make-sparse-keymap))
    ))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun qb-key-description (keys)
  (let ((s (key-description keys)))
    (setq s (replace-regexp-in-string "RET" "C-m" s t))
    s)
  )

(defun qb-where-is (what &optional map partial)
  (or map (setq map qb-map))
  (let ((i (rassoc what map)))
    (cond
     (i (cons (car i) partial))
     ((catch 'got
	(dolist (m (cdr map))
	  (cond
	   ((and (listp (cdr m)) (eq (cadr m) 'keymap))
	    (cond ((setq i (qb-where-is what (cdr m) (cons (car m) partial)))
		   (throw 'got i)
		   ))
	    ))
	  )
	))
     )
    )
  )

(defun qb-where-is-string (what)
  (let ((s (qb-where-is what)))
    (cond (s (qb-key-description (apply 'vector (reverse s)))))
    ))

(defun qb-mapping (item) (or (qb-where-is-string item) ""))
 
(defun map-dump (map fun &optional indent)
  (let ((is (make-string (* 2 (or indent 0)) ? )))
    (mapcar
     '(lambda (i)
	(let* ((key (car i))
	       (binding (cdr i))
	       (ks (format "%s%s" is (qb-key-description (vector key))))
	       )
	  (cond ((keymapp binding)
		 (cond ((> (length binding) 1)
			(cons (format "%-12s" ks)
			      (map-dump binding fun (1+ (or indent 0)))))
		       ))
		((format "%-12s%s" ks (safe-funcall fun binding)))
		)))
	(cdr map)))
  )

(defun qb-map-show-1 (binding)
  (cond
   ((stringp binding) (format "%s" binding))
   ((let ((eb (eval binding)))
      (format "%-40s%s"
	      (cond
	       (eb (format "%s" eb))
	       ("")
	       ) binding
		 ))
    )
   )
  )

(defun qb-map-show () (interactive)
  (show (cat (flatten (map-dump qb-map 'qb-map-show-1)) "\n"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;

(defun keymap-prune (map)
  (delete nil (mapcar
   '(lambda (x)
      (cond
       ((eq x 'keymap) x)
       ((and (listp (cdr x)) (eq (cadr x) 'keymap))
	(let ((y (keymap-prune (cdr x))))
	  (cond (y (cons (car x) y)))))
       ((cdr x) x)
       )
      ) map)
  ))

(defun qb-delete (key)
  (define-key qb-map key nil)
  (setq qb-map (keymap-prune qb-map))
  (define-key b-map key nil)
  (setq b-map (keymap-prune b-map))
  (define-key global-map "\C-b" b-map)
  )

(defun qb-define (key form &optional expand)
  (let ((key (eval key)))
    (cond (expand (setq form (filename-canonical form))))
    (and (numberp key) (setq key (char-to-string key)))
    (define-key qb-map key form)
    (define-key b-map key 'qb-select)
    (format "key %s mapped to %s" key form)
    ))

(defun qb-select* (key)
  (let* ((binding (lookup-key qb-map key))
	 (buffer (eval binding)))
    (cond
     (arg (switch-to-buffer-other-window (buffer buffer)))
     (t (switch-to-buffer (buffer buffer)))
     )
    ))

(defun qb-select (arg) (interactive "P")
  (let* ((keys (this-command-keys))
	 (key (substring keys (cond (arg 2) (1))))
	 )
    (qb-select* key)
    ))

(defun qb-select-nostrip (arg) (interactive "P")
  (qb-select* (this-command-keys))
  )

(defun qb-define-read-key (item)
  (let* ((map (current-local-map)))
    (use-local-map qb-map)
    (unwind-protect
	(let ((key (read-key-sequence "Key:"))
	      )
	  (cond ((and (stringp key) (string-equal key "\C-g"))
		 (error "Cancelled")))
	  (define-key qb-map key item) 
	  (define-key b-map key 'qb-select)
	  (message "mapped %s to %s" (qb-key-description key) item)
	  )
      (use-local-map map)
      )
    )
  )
  
(defun qb-define-current () (interactive)
  (qb-define-read-key (or (buffer-file-name) (current-buffer)))
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(mapcar '(lambda (i)
;	   (let* ((c (char-to-string (1+ i)))
;		  (x (lookup-key b-map c)))
;	     (cond
;	      (x)
;	      ((define-key b-map c (make-sparse-keymap)))
;	      )
;	     )
;	   ) (mdotimes (i 26) i))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qb-and-mini (k form &optional expand)
  (cond (expand (setq form (filename-canonical form))))
  (minibuffer-dir (char-to-string k) form)
  (qb-define (control-key-vector ?/ k) form)
  )

(qb-define "\C-l\C-b" '(nvc-dirname))

(define-key b-map "\C-u\C-u" 'qb-define-current)
(define-key b-map "\C-u\C-s" 'qb-map-show)
(define-key b-map "\C-e\C-r" 'recentf-open-files)

;(qb-delete (control-key-vector ?g ?g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(describe-variable 'b-map)
