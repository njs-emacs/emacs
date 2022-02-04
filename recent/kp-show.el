(defmacro with-read-only-off (&rest body)
  `(let ((buffer-read-only nil))
    ,@body
    )
  )

(defun keys (s) (cond
		 ((symbolp s) (kbd (symbol-name s)))
		 ((stringp s) (kbd s))
		 (s)))

(defun kp-toggle-way (&optional to-num)
  (interactive)
  (with-read-only-off
   (sx
    (mapcar '(lambda (x) (bob)
	       (let* ((n (format "#%c#" (car x)))
		      (k (cdr x))
		      )
		 (cond
		  (to-num (replace-string k n))
		  (t (replace-string n k))
		  )
		 )
	       )
	    function-key-to-kp-alist)
    )
   )
  )

(defun kp-toggle ()
  (interactive)
  (let ((to-num (sx (bob) (rsf "^C-. key"))))
    (kp-toggle-way to-num)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xxx-show-bindings (list &optional buffer)
  (let* ((buffer (or buffer (current-buffer)))
	 (show (get-buffer-create "*keys*"))
	 )
  (with-current-buffer show
    (with-read-only-off
     (erase-buffer)
     (dolist (k list)
       (local-set-key (keys (format "%c" k))
	`(lambda () (interactive) (bob) (rsf ,(format "keypad key %c" k)))
	)
       (insert (format "%s key is on keypad key %c\n" k
		       (car (rassoc k function-key-to-kp-alist))))
       
       (describe-buffer-bindings buffer (keys k))
       (insert "================================================================\n")
       )
     (bob)
     (sx (while (rsf "^Global") (fl -1) (kill-line 4)))
     (sx (while (rsf "Prefix Command") (bol) (kill-line 1)))
     (local-set-key (keys "M-t") 'kp-toggle)
     )
    )
  (view-buffer-other-window show)
  )
  )

(defun xxx-show ()
  (interactive)
  (kp-show-bindings
   (mapcar 'cdr function-key-to-kp-alist)
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq kp-show-map (make-sparse-keymap))

(defun kp-show-jump (n)
  (bob) (rsf (format "keypad key %c" n)) (bol) (recenter)
  )

(dolist (i function-key-to-kp-alist)
  (let ((n (car i)) (k (cdr i)))
    (define-key kp-show-map (keys (format "%c" n))
      `(lambda () (interactive) (kp-show-jump ,n))
      )
    (define-key kp-show-map (keys k)
      `(lambda () (interactive)(kp-show-jump ,n))
      )
    )
  )

(define-key kp-show-map (keys "M-t") 'kp-toggle)


(defun kp-show-bindings (&optional buffer)
  (let* ((buffer (or buffer (current-buffer)))
	 (show (get-buffer-create "*keys*"))
	 (list function-key-to-kp-alist)
	 )
  (with-current-buffer show
    (with-read-only-off
     (erase-buffer)
     (dolist (i list)
       (let ((n (car i)) (k (cdr i)))
	 (insert (format "%s key is on keypad key %c\n" k n))
	 (describe-buffer-bindings buffer (keys k))
	 (insert "================================================================\n")
	 )
       )
     (bob)
     (sx (while (rsf "^Global") (fl -1) (kill-line 4)))
     (sx (while (rsf "Prefix Command") (bol) (kill-line 1)))
     )
    (use-local-map kp-show-map)
    )
  (pop-to-buffer show)
  
  )
  )

(defun kp-show ()
  (interactive)
  (kp-show-bindings (current-buffer))
  )

;(kp-show)
(define-key help-map (kbd "C-k") 'kp-show)

