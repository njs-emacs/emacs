(setq pause-map (make-sparse-keymap))
(setq c-pause-map (make-sparse-keymap))
(setq m-pause-map (make-sparse-keymap))
(setq c-m-pause-map (make-sparse-keymap))

(global-set-key (kbd "<pause>") pause-map)
(global-set-key (kbd "<C-pause>") c-pause-map)
(global-set-key (kbd "<M-pause>") m-pause-map)
(global-set-key (kbd "<C-M-pause>") c-m-pause-map)

(defmacro pause-macro-create (map key &rest body)
  `(define-key ,map ,key (lambda (arg) (interactive "P") ,@body))
  )

(defin 'pause-macro-create)

(define-key m-pause-map "1" 'find-file)

(pause-macro-create c-pause-map "1"
  (insert "hello")
  )
