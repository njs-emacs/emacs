#~ this is of unknown purpose

(defun ltoc-compile (fun)
  (let* ((form (symbol-function fun))
	 (args (nth 1 form))
	 )
    (concat
     (format "lo F%s(%s) ;\n" fun (mformat args "lo %s" ","))
     )
    ))

(ltoc-compile 'compile)
    
