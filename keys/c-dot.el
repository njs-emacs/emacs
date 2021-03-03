(setq c-dot-map (make-sparse-keymap))
(global-set-key (kbd "C-.") c-dot-map)

(define-key c-dot-map (kbd "E") 'keydef-edit-init)

(define-key c-dot-map (kbd "C-.") 'symbol-overlay-put)
