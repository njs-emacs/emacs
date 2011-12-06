;; frame parameters are in frame.el

(defun efl (pat &optional grep-args find-args)
  (compile (format "find . -name \\*.el -exec grep %s \"%s\" {} NUL ;"
		   (or grep-args "-n") pat))
  )

(efl "f10")

(efl "global-set-key")
