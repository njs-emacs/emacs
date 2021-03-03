(setq c-slash-map (make-sparse-keymap))
(global-set-key (kbd "C-/") c-slash-map)

(define-key c-slash-map (kbd "E") 'keydef-edit-init)

(define-key c-slash-map (kbd "C-/") 'symbol-overlay-rename)
(define-key c-slash-map (kbd "C-f") 'org-insert-file-link)
