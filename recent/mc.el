; duplicated in keys/c-comma.el

(setq mc/initiator-map (make-sparse-keymap))

(define-key mc/initiator-map (kbd "C-.") 'mc/mark-more-like-this-extended)
(define-key mc/initiator-map (kbd "C-x") 'mc/mark-all-like-this)
(define-key mc/initiator-map (kbd "C-SPC") 'mc/edit-lines)
(define-key mc/initiator-map (kbd "C-a") 'mc/edit-beginnings-of-lines)
(define-key mc/initiator-map (kbd "C-e") 'mc/edit-ends-of-lines)

(define-key mc/initiator-map (kbd "s") 'mc/mark-all-in-region)
(define-key mc/initiator-map (kbd "C-s") 'mc/mark-all-in-region-regexp)

(define-key mc/initiator-map (kbd "C-n") 'mc/mark-next-symbol-like-this)
(define-key mc/initiator-map (kbd "C-p") 'mc/mark-previous-symbol-like-this)

(define-key mc/initiator-map (kbd "C-,") 'mc/mark-all-dwim)

(global-set-key (kbd "C-,") mc/initiator-map)
(global-set-key (kbd "s-,") mc/initiator-map)

(global-set-key (kbd "s-<mouse-1>") 'mc/add-cursor-on-click)

