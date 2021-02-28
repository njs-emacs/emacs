(setq c-colon-map (make-sparse-keymap))
(define-key global-map (kbd "C-:") c-colon-map)

(define-key c-colon-map (kbd "E") 'keydef-edit-init)

(define-key c-colon-map (kbd "C-x") 'er/expand-region)

