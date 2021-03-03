(setq c-colon-map (make-sparse-keymap))
(global-set-key (kbd "C-:") c-colon-map)

(define-key c-colon-map (kbd "E") 'keydef-edit-init)

(define-key c-colon-map (kbd "C-x") 'er/expand-region)

