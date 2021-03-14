(setq c-bang-map (make-sparse-keymap))
(global-set-key (kbd "C-!") c-bang-map)

(def-key c-bang-map (kbd "E") 'keydef-edit-init)

;(define-key c-bang-map (kbd "a") "hello")

;(define-key c-bang-map (kbd "a") (kbd "C-a [ ]"))
;(define-key c-bang-map (kbd "a") (kbd "C-a [ <f4> ]"))

; wierd problem with extra key input (?)
;(define-key c-bang-map (kbd "C-+") (kbd "<C-f1>"))

;(define-key c-bang-map (kbd "C-+") (kbd "<f10>"))
