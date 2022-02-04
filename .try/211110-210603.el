(defmacro v* (sym op &rest args) `(setq ,sym (funcall ',op ,sym ,@args)))
(defmacro iv* (sym op &rest args)
  `(lambda () (interactive)
     (setq ,sym (funcall ',op ,sym ,@args))
     (message "%s value is %s" ',sym ,sym)
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(number-to-register 0 ?w) 

(setq jj 0)
(v* jj 1+)
(v* jj + 2)

