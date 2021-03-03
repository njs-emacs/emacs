(setq h-slash-map (make-sparse-keymap))

(global-set-key (kbd "H-/") h-slash-map)

(define-key h-slash-map (kbd "E") 'keydef-edit-init)
