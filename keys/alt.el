(define-key global-map (kbd "<A-backspace>") 'winner-undo)
(define-key global-map (kbd "<A-M-backspace>") 'winner-redo)

(define-key global-map (kbd "<A-return>") 'ace-jump-buffer)
(define-key global-map (kbd "A-#") 'ace-jump-line-mode)

(define-key global-map (kbd "A-[") 'bmkp-previous-bookmark-repeat)
(define-key global-map (kbd "A-]") 'bmkp-next-bookmark-repeat)

;; (define-key global-map (kbd "A-.") "p name\nc\n")

(define-key global-map (kbd "A-s") 'magit-stage-this-file)
(define-key global-map (kbd "A-c") 'magit-commit-create)
