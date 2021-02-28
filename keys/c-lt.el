(setq c-lt-map (make-sparse-keymap))
(global-set-key (kbd "C-<") c-lt-map)

(define-key c-lt-map (kbd "E") 'keydef-edit-init)
