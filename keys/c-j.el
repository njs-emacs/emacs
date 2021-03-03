(setq c-j-map (make-sparse-keymap))
(global-set-key (kbd "C-j") c-j-map)

(define-key c-j-map (kbd "E") 'keydef-edit-init)

(define-key c-j-map (kbd "C-j") 'symbol-overlay-remove-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/abo-abo/auto-yasnippet 

(define-key c-j-map (kbd "C-y") 'aya-expand)
(define-key c-j-map (kbd "C-k") 'aya-create)

; aya-persist-snippet

