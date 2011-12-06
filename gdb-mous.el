(define-key mouse-map x-button-m-middle
  '(lambda (arg) (interactive)
     (gdb-command-region (point) (sxp (x-mouse-set-point arg)) "p %s")))

(define-key mouse-map x-button-m-s-middle
  '(lambda (arg) (interactive)
     (gdb-command-region (point) (sxp (x-mouse-set-point arg)) "p *%s")))

(define-key mouse-map x-button-m-left
  '(lambda (arg) (interactive)
     (x-mouse-set-point arg)
     (gdb-command-expr (point) "p %s")))

(define-key mouse-map x-button-m-s-left
  '(lambda (arg) (interactive)
     (x-mouse-set-point arg)
     (gdb-command-expr arg "p *%s")))

(define-key mouse-map x-button-m-right
  '(lambda (arg) (interactive)
     (sx (x-mouse-set-point arg)
	 (gdb-command-exp "p pp(%s)"))))

(define-key mouse-map x-button-m-s-right
  '(lambda (arg) (interactive)
     (sx (x-mouse-set-point arg)
	 (gdb-command-exp "p pp(Fsymbol_value(intern(\"%s\")))"))))

(setq gdb-reset-string nil)

(defun gdb-reset () (interactive)
  (gdb-send (format "ex %s" gdb-exec-file))
  (gdb-send (format "sy %s" gdb-exec-file))
  (and gdb-reset-string (gdb-send gdb-reset-string))
  )

