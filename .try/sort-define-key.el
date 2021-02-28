(defun sort-defkey-nextrec ()
  (cond ((re-search-forward "^(define-key" (point-max) t) (bol)))
  )

(defun sort-defkey-endrec ()
  (bol) (fl 1)
  )

;;; 
; if startkey returns nil, the current point is used
; but if startkey returns (point) the endkey function doesn't get called

(defmacro sit-after (&rest forms)
  `(prog1 (progn ,@forms) (sit-for .1))
  )
			  
(defun sort-defkey-startkey1 ()
  (sx
   (fc 1) (fx 2)
   (bs (point) (sxp (fx 1)))
   )
  )

(defun sort-defkey-startkey ()
  (let ((x (sort-defkey-startkey1)))
    (message "%s" x)
    (sit-for 0.5)
    x
    )
  )

(defun sort-defkey-endkey ()
  )

(defun sort-defkey-compare (a b)
  (message (format "[%s] [%s]" a b))
  (string< a b)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sort-define-key () (interactive)
       (sx
 (sort-subr nil
  'sort-defkey-nextrec
  'sort-defkey-endrec
  'sort-defkey-startkey
  'sort-defkey-endkey
  'sort-defkey-compare
  )
  ))

