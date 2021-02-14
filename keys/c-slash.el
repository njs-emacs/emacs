(global-unset-key (kbd "C-/"))
(setq c-slash-map (make-sparse-keymap))
(define-key global-map (kbd "C-/") c-slash-map)

(define-key c-slash-map (kbd "C-/") 'symbol-overlay-rename)
