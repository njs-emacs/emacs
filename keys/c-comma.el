(setq c-comma-map (make-sparse-keymap))
(global-set-key (kbd "C-,") c-comma-map)

(define-key c-comma-map (kbd "E") 'keydef-edit-init)

(define-key c-comma-map (kbd "C-.") 'mc/mark-more-like-this-extended)
(define-key c-comma-map (kbd "C-x") 'mc/mark-all-like-this)
(define-key c-comma-map (kbd "C-SPC") 'mc/edit-lines)
(define-key c-comma-map (kbd "C-a") 'mc/edit-beginnings-of-lines)
(define-key c-comma-map (kbd "C-e") 'mc/edit-ends-of-lines)

;(define-key c-comma-map (kbd "C-s") 'mc/mark-all-in-region)
(define-key c-comma-map (kbd "C-s") 'mc/mark-all-in-region-regexp)

(define-key c-comma-map (kbd "C-n") 'mc/mark-next-like-this)
(define-key c-comma-map (kbd "C-p") 'mc/mark-previous-symbol-like-this)
(define-key c-comma-map (kbd "C-,") 'mc/mark-all-dwim)

(define-key c-comma-map (kbd "C-o") 'mc--mark-symbol-at-point)

