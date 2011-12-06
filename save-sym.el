(provide 'save-sym)

(defun setq-form-string (sym &optional value force)
  (format "(setq %s '%S)" sym (cond ((or value force) value) ((eval sym)))))

(defun save-sym-value (sym &optional value force)
  "Return a list of assignments that will re-create the value of SYM
though the original mechanism which created it (if known).
The method is in the symbol's property 'save.
If optional VALUE is given, use this value instead of the symbol's
current value.
Third parameter FORCE overrides cases where it is desired to set the
value even if nil."
  (let ((value (cond ((or value force) value) ((eval sym))))
	(save (get sym 'save))
	)
    (cond (save (funcall save sym value))
	  ((format "(setq %s '%S)" sym value)
	   ))))
