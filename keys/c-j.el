(global-unset-key (kbd "C-j"))
(setq c-j-map (make-sparse-keymap))
(define-key global-map (kbd "C-j") c-j-map)

(define-key c-j-map (kbd "C-j") 'symbol-overlay-remove-all)
