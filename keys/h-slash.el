(setq h-slash-map (make-sparse-keymap))

(define-key global-map (kbd "H-/") h-slash-map)

(define-key h-slash-map (kbd "E") 'keydef-edit-init)
