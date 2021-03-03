(setq c-z-map (make-sparse-keymap))
(global-set-key "\C-z" c-z-map)

(define-key c-z-map "\C-t" 'toggle-truncate-lines)
(define-key c-z-map "\C-i" 'init-local)
(define-key c-z-map "\C-r" 'read-only-mode)

(define-key c-z-map "\C-b\C-d" 'nvc-ls-bdiff)

(define-key c-z-map (kbd "C-.") 'dired-quick-dot)

(define-key c-z-map "\C-p" 'print-it)
(define-key c-z-map "\C-f" 'filename-to-kill)

(define-key c-z-map "\C-j" 'join-line)
(define-key c-z-map "\C-u" 'upcase-region)
(define-key c-z-map "\C-d" 'downcase-region)

(define-key c-z-map "\C-s" 'toggle-case-fold-search)

(define-key c-z-map "\C-l" 'font-lock-mode)
(define-key c-z-map "\C-k" 'find-kill-head)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq zc-z-map (make-sparse-keymap))
(define-key c-z-map "\C-z" zc-z-map)

(define-key zc-z-map "\C-z" 'undo)

(define-key c-z-map (kbd "C-/") 'toggle-slashification-region)

(setq z-map c-z-map)		; compatibility
