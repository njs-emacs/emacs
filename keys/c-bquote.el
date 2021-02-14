(global-unset-key (kbd "C-`"))
(setq c-bquote-map (make-sparse-keymap))
(define-key global-map (kbd "C-`") c-bquote-map)

(define-key c-bquote-map (kbd "C-`") 'symbol-overlay-hydra/body)
