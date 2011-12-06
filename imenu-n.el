(defun imenu-create-index-I ()
  (let ((list))
    (sx
     (bob)
     (while (rsf "/\\*I[+-]* *\\([^*]*\\)")
       (push list (cons (ms 1) (sxp (bol)))))
     )
    (nreverse list)))

(defun imenu-create-index= ()
  (let ((list))
    (sx
     (bob)
     (while (rsf "/\\* *==+\\s *\\([^=]*\\)")
       (push list (cons (ms 1) (sxp (bol)))))
     )
    (nreverse list)))
