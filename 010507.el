(defun quote-add () (interactive)
  (let* ((end (sxp (fx 1)))
	 (start (sxp (fx -1))))
    (sx (goto-char end) (insert "\"") (goto-char start) (insert "\"")) 
    )
  )
(defun mouse-quote-add (arg) (interactive "e")
  (mouse-set-point arg)
  (quote-add)
  )
;(define-key c3-map [?\C-q] 'mouse-quote-add)

(setq foo-map '(keymap))

(defmacro foo-def (key &rest body)
  `(define-key foo-map ,key '(lambda () (interactive) ,@body))
  )

(defin 'foo-def)

(defun foo-re ()
  (use-local-map foo-map)
  (prog1 (re)
    (use-local-map nil)
    )
  )

