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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro setq-push (sym value)
  (let* ((ssym (intern (format "-%s--stack" sym))))
    (or (boundp ssym) (set ssym nil))
    (or (boundp sym) (set sym nil))
    `(let ()
       (setq ,ssym (cons ,sym ,ssym))
       (setq ,sym ,value)
       )
    )
  )

(defmacro setq-pop (sym)
  (let* ((ssym (intern (format "-%s--stack" sym)))
	 (ssv (and (boundp ssym) (symbol-value ssym)))
	 )
    (cond
     (ssv
      (set sym (car ssv))
      (set ssym (cdr ssv))
      `,sym
      )
     (
      `(message "stack is empty for '%s'" ',sym))
     )
    )
  )

