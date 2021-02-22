(defun sort-defkey-nextrec ()
  (cond ((re-search-forward "^(define-key" (point-max) t) (bol)))
  )

(defun sort-defkey-endrec ()
  (bol) (fl 1)
  )

;;; 
; if startkey returns nil, the current point is used
; but if startkey returns (point) the endkey function doesn't get called

(defun sort-defkey-startkey ()
  (fc 1)
  (fx 2)
  (sit-for 0.1)
  nil
  )

(defun sort-defkey-endkey ()
  (fx 1)
  (sit-for 0.1)
  nil
  )

(defun sort-defkey-compare (a b)
;  (message (format "[%s] [%s]" a b))
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
;  'sort-defkey-compare
  )
  ))
