(setq c-v-map (make-sparse-keymap))
(global-set-key (kbd "C-v") c-v-map)

(define-key c-v-map (kbd "E") 'keydef-edit-init)

; [[file:e:/.p/.emacs/extra.el::127][extra.el]]

(define-key c-v-map (kbd "C-#") 'enquote-at-point-d)

(define-key c-v-map (kbd "C-'") 'enquote-at-point-s)
(define-key c-v-map (kbd "C-q") 'enquote-at-point-d)

(define-key c-v-map (kbd "C-s") 'enquote-at-point-s)
(define-key c-v-map (kbd "C-v") 'scroll-up)

