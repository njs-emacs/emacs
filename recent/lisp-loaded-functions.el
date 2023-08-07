(defun lisp-loaded-functions (&optional file)
  (let* ((file (or file (buffer-file-name)))
	 (list (cdr (assoc file load-history)))
	 )
    (delete nil
	    (mapcar '(lambda (x) (cond ((and (listp x) (eq (car x) 'defun)) (cdr x)))) list))
    )
  )

(defun lisp-loaded-interactive-functions (&optional file)
  (let* ((list (lisp-loaded-functions file))
	 )
    (delete nil
      (mapcar '(lambda (x) (cond ((commandp x) x))) list)
      )
    )
  )

;(assoc "e:/emacs/recent/mug.el" load-history)
; (lisp-loaded-functions "e:/emacs/recent/mug.el")
; (lisp-loaded-interactive-functions "e:/emacs/recent/mug.el")
