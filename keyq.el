(defun mm (p x)
  (cons x (key-binding (read (concat "\"" p x "\""))))
  )

(mdotimes (i 26) (mm "\\M-" (char-to-string (+ ?a i))))
(mdotimes (i 26) (mm "\\C-" (char-to-string (+ ?a i))))
