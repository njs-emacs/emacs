(setq c-rbracket-map (make-sparse-keymap))
(global-set-key (kbd "C-]") c-rbracket-map)

(define-key c-rbracket-map (kbd "E") 'keydef-edit-init)

(define-key c-rbracket-map (kbd "5") 'indent-pattern-to-column)
(define-key c-rbracket-map (kbd "f") 'find-file)
(define-key c-rbracket-map (kbd "e") 'list-buffers)
(define-key c-rbracket-map (kbd "d") 'dired-quick-dot)

(define-key c-rbracket-map (kbd "a") "aaa")

(define-key c-rbracket-map (kbd "w w") 'ace-window)

