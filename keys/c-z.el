(setq z-map (make-sparse-keymap))
(define-key global-map "\C-z" z-map)

(define-key z-map "\C-t" 'toggle-truncate-lines)
(define-key z-map "\C-i" 'init-local)
(define-key z-map "\C-r" 'read-only-mode)

(define-key z-map "\C-b\C-d" 'nvc-ls-bdiff)

(define-key z-map (kbd "C-.") 'dired-quick-dot)

(define-key z-map "\C-p" 'print-it)
(define-key z-map "\C-f" 'filename-to-kill)

(define-key z-map "\C-j" 'join-line)
(define-key z-map "\C-u" 'upcase-region)
(define-key z-map "\C-d" 'downcase-region)

(define-key z-map "\C-s" 'toggle-case-fold-search)

(define-key z-map "\C-l" 'font-lock-mode)
(define-key z-map "\C-k" 'find-kill-head)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq zz-map (make-sparse-keymap))
(define-key z-map "\C-z" zz-map)

(define-key zz-map "\C-z" 'undo)

(define-key z-map (kbd "C-/") 'toggle-slashification-region)

