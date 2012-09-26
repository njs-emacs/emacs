(defun imenu-create-index-I ()
  (let ((list))
    (sx
     (bob)
     (while (rsf "/\\*I[+-]* *\\([^*]*\\)")
       (push (cons (ms 1) (sxp (bol))) list))
     )
    (nreverse list)))

(defun imenu-create-index= ()
  (let ((list))
    (sx
     (bob)
     (while (rsf "/\\* *==+\\s *\\([^=]*\\)")
       (push (cons (ms 1) (sxp (bol))) list))
     )
    (nreverse list)))
