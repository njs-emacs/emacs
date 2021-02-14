(global-unset-key (kbd "C-."))
(setq c-dot-map (make-sparse-keymap))
(define-key global-map (kbd "C-.") c-dot-map)

(define-key c-dot-map (kbd "C-.") 'symbol-overlay-put)
