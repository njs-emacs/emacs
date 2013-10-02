(setq pause-map (make-sparse-keymap))
(setq c-pause-map (make-sparse-keymap))
(setq m-pause-map (make-sparse-keymap))
(setq c-m-pause-map (make-sparse-keymap))

(define-key global-map (kbd "<pause>") pause-map)
(define-key global-map (kbd "<C-pause>") c-pause-map)
(define-key global-map (kbd "<M-pause>") m-pause-map)
(define-key global-map (kbd "<C-M-pause>") c-m-pause-map)

(defmacro pause-macro-create (map key &rest body)
  `(define-key ,map ,key (lambda (arg) (interactive "P") ,@body))
  )

(defin 'pause-macro-create)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(top-level)

(define-key m-pause-map "1" 'find-file)

(pause-macro-create pause-map "1"
  (insert "hello")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(describe-variable 'kmacro-ring)

(describe-function 'kmacro-name-last-macro)

last-kbd-macro is not the actual last maco creates, rather it is the one
that is the object on which macro functions operate by default.
more like current-kbd-macro

kmacro-name-last-macro makes it a function

