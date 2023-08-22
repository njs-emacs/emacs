(defun mug-get-tform-expr (p)
  (let ((marker (cadr (nth 3 (cadr p)))))
    (prin1-to-string (car (mug-tform-read marker)))
    )
  )

(defun mug-hydra ()
  (interactive)
  (let* ((forms (mapcar
		 '(lambda (b)
		    (cond
		     ((listp (cdr b))
		      (let* ((mm (mug-tmarker-get-mark (cdr b)))
			     (command (mug-read-command-line mm))
			     )
;			(list (key-description (vector (car b))) (mug-tmarker-get-mark (cdr b)))
;			(list (key-description (vector (car b))) (cdr b))
;			(list (key-description (vector (car b)))
;			      `(lambda () (interactive) (goto-char ,(mug-tmarker-get-mark (cdr b)))))
			(list (key-description (vector (car b)))
			      `(lambda () (interactive) (mug-exec-here ,(mug-tmarker-get-mark (cdr b)))))
			))
		     )
		    ) (cdr mug-tmarker-map)))
	 (forms (delete nil forms))
;	 (pforms (mconcat (mapcar '(lambda (x) (mug-get-tform-expr x)) forms) "\n"))
	 (pforms (mconcat (mapcar '(lambda (x) (format "%-10s %s" (car x) (mug-get-tform-expr x))) forms) "\n"))
	 (pforms (string-replace "%" "%%%%" pforms))
	 (hform
	  `(defhydra mug-hydra* (:color pink :hint nil :timeout 10)
      ,(concat pforms "\n\nq   quit\n")
      ,@forms
      ("q" nil)
      )))
    (eval hform)
    (call-interactively 'mug-hydra*/body)
    )
  )
