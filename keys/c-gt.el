(setq c-gt-map (make-sparse-keymap))
(global-set-key (kbd "C->") c-gt-map)

(define-key c-gt-map (kbd "E") 'keydef-edit-init)

(define-key c-gt-map (kbd "c") 'magit-commit-create)

