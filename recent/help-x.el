(setq help-x-map (make-sparse-keymap))
(define-key help-map (kbd "C-x") help-x-map)

(define-key help-x-map (kbd "C-k")
  (ilambda ()
    (find-file-other-window "e:/ca/htdocs/help/keypad2.svg")))
