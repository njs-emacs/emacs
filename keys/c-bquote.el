(setq c-bquote-map (make-sparse-keymap))
(global-set-key (kbd "C-`") c-bquote-map)

(define-key c-bquote-map (kbd "E") 'keydef-edit-init)

(define-key c-bquote-map (kbd "C-c") 'avy-copy-region)

(define-key c-bquote-map (kbd "C-k C-k") 'avy-kill-whole-line)

(define-key c-bquote-map (kbd "C-x") 'avy-kill-region)

(define-key c-bquote-map (kbd "C-l") 'avy-move-line)
(define-key c-bquote-map (kbd "C-m") 'avy-move-region)

(define-key c-bquote-map (kbd "C-e") 'avy-goto-end-of-line)
(define-key c-bquote-map (kbd "C-a") 'avy-goto-line)
(define-key c-bquote-map (kbd "C-s") 'avy-goto-symbol-1)
(define-key c-bquote-map (kbd "C-w") 'avy-goto-word-1)
(define-key c-bquote-map (kbd "C-q") 'avy-goto-char-2)

