(setq c-semicolon-map (make-sparse-keymap))
(define-key global-map (kbd "C-;") c-semicolon-map)

(define-key c-semicolon-map (kbd "E") 'keydef-edit-init)

(define-key c-semicolon-map (kbd "C-;") 'er/expand-region)

