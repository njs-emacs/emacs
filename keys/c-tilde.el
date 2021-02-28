(setq c-tilde-map (make-sparse-keymap))
(define-key global-map (kbd "C-~") c-tilde-map)

(define-key c-tilde-map (kbd "E") 'keydef-edit-init)

(define-key c-tilde-map (kbd "C-x") 'er/expand-region)

