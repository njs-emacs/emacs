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

(define-key m-pause-map "1" 'find-file)

(pause-macro-create c-pause-map "1"
  (insert "hello")
  )
