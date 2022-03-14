(defmacro flipp (symbol)
   "Toggle SYM. Echos the new value, and returns the new value"
  `(prog1
       (setq ,symbol (not ,symbol))
     (message "%s is set to %s" (quote ,symbol) ,symbol)
     )
  )

(defmacro flip (symbol)
   "Toggle SYM. Does not return the value, but just echos the new value"
  `(progn
     (setq ,symbol (not ,symbol))
     (message "%s is set to %s" (quote ,symbol) ,symbol)
     )
  )

(flip case-fold-search)

(define-key org-mode-map (kbd "<H-return>") 'eval-defun-without-narrow)
