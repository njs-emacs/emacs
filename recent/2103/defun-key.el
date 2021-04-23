(defmacro defun-key (map key name &rest def)
  `(progn
     (defun ,name (&optional arg) (interactive "P") ,@def)
     (def-key ,map (kbd ,key) ',name)
     )
  )
