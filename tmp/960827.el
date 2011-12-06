(defun gf (s &optional name)
  (compile
   (format "find . \\( -name sccs -prune \\) -o -name \"%s\" -exec grep -n \"%s\" /dev/null {} \\;" (or name "*.n") s)
   )
  )

(gf "LAYER")
(gf "main")
(compile "find . -name \"*.n\" -exec grep -n main /dev/null {} \\;")

(defun foo (click) (interactive "e")
  (let ((s (bs (point) (posn-point (event-end click)))))
    (mouse-set-region click)
    (bol) (fc 1) (fx 1)
    (insert " ")
    (insert s)
    )
  )

(define-key global-map [S-mouse-3] 'foo)

