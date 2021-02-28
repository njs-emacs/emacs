(setq ctrl-hash-map (make-sparse-keymap))
(global-set-key (kbd "C-#") ctrl-hash-map)

(define-key ctrl-hash-map (kbd "SPC") 'mark-word)
(define-key ctrl-hash-map (kbd "C-SPC") 'mark-sexp)
(define-key ctrl-hash-map (kbd "C-s") 'magit-update-index)
(define-key ctrl-hash-map (kbd "C-d") 'pending-delete-mode)

(define-key ctrl-hash-map [C-home] 'beginning-of-defun)
(define-key ctrl-hash-map [C-end] 'end-of-defun)

(define-key ctrl-hash-map (kbd "C-#") 'avy-goto-line)
(define-key ctrl-hash-map (kbd "C-.") 'avy-goto-word-or-subword-1)

(define-key ctrl-hash-map (kbd "C-f") 'find-file-at-point)

(define-key ctrl-hash-map "\C-i" 'insert-register)

(define-key ctrl-hash-map "\C-c" 'copy-to-register)

;;

