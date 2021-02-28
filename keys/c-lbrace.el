(setq c-lbrace-map (make-sparse-keymap))
(global-set-key (kbd "C-{") c-lbrace-map)

(define-key c-lbrace-map (kbd "E") 'keydef-edit-init)

(define-key c-lbrace-map (kbd "SPC") 'ace-swap-window)
(define-key c-lbrace-map (kbd "C-m") 'ace-jump-line-mode)
(define-key c-lbrace-map (kbd "/") 'ace-jump-word-mode)
(define-key c-lbrace-map (kbd "k") 'ace-delete-window)

(define-key c-lbrace-map (kbd "#") 'ace-jump-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key c-lbrace-map (kbd "'") 'ace-jump-same-mode-buffers)

(define-key c-lbrace-map (kbd "j") 'ffx)
(define-key c-lbrace-map (kbd "n") 'ffn)

(define-key c-lbrace-map (kbd "c") 'magit-commit-create)

