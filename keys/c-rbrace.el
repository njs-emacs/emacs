(setq c-rbrace-map (make-sparse-keymap))
(global-set-key (kbd "C-}") c-rbrace-map)

(define-key c-rbrace-map (kbd "E") 'keydef-edit-init)

(define-key c-rbrace-map (kbd "c") 'magit-commit-create)

