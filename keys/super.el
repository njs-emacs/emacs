(require 'auto-yasnippet)
(define-key global-map (kbd "s-w") #'aya-create)
(define-key global-map (kbd "s-q") #'aya-expand)

(define-key global-map (kbd "s-SPC") 'ace-jump-line-mode)

(define-key global-map (kbd "<s-delete>") 'undo)

;(define-key global-map (kbd "s-o") 'find-file)
;(define-key global-map (kbd "s-h") 'help)

(define-key global-map [s-mouse-1] 'mc/add-cursor-on-click)


;; (define-key global-map (kbd "s-.") "->")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "<s-f9>") 'emacs-read-hash-plus)


;; (dregf "define-key.*s-" elfs "ever")
