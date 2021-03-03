(setq c-tilde-map (make-sparse-keymap))
(global-set-key (kbd "C-~") c-tilde-map)

(define-key c-tilde-map (kbd "E") 'keydef-edit-init)

(define-key c-tilde-map (kbd "C-x") 'er/expand-region)

