(load-standard "macros")

(defun exec-macro () (interactive)
  (and defining-kbd-macro (macro-bracket))
  (call-last-kbd-macro)
  )

(setq defined-macros nil)
(defun macro-bracket (&optional arg) (interactive "P")
  (cond (defining-kbd-macro
	  (end-kbd-macro)
	  (setq defined-macros (cons last-kbd-macro defined-macros)))
	((start-kbd-macro arg))))

(defun browse-macros () (interactive)
  (let ((macros defined-macros))
    (pop-to-buffer (get-buffer-create "*macros*"))
    (emacs-lisp-mode)
    (erase-buffer)
    (insert "(setq defined-macros '(\n")
    (mapcar '(lambda (x)
	       (insert (format "    \"%s\"\n" x))) macros)
    (insert "    ))\n")
    (insert "(setq last-kbd-macro (nth 0 defined-macros))\n")
    ))

(define-key m-map "d"
  '(lambda (x) (interactive "aFunction: ")
     (insert (format "(fset '%s '%s)" (symbol-name x) (symbol-function x)))))

(define-key m-map "b"
  '(lambda (key) (interactive "k") (insert (symbol-name (key-binding key)))))
(define-key m-map " " 'browse-macros)

(define-key m-map "\em" 'macro-bracket)

(define-key m-map "'" 'make-macro-from-keys)
(defun make-macro-from-keys () (interactive)
  (setq defined-macros (cons (recent-keys) defined-macros))
  (browse-macros)
  )

(define-key m-map "." 'make-macro-from-keys)

(load-overrides "macros")
