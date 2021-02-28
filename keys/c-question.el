(setq c-question-map (make-sparse-keymap))
(global-set-key (kbd "C-?") c-question-map)

(define-key c-question-map (kbd "E") 'keydef-edit-init)

(define-key c-question-map (kbd "c") 'magit-commit-create)
