(defvar org-master-file "e:/.org/home.org")

(defun org-master-file-edit ()
  (interactive) (find-file-other-window org-master-file))

(define-key global-map (kbd "<H-f1>") 'org-master-file-edit)

(define-key global-map (kbd "<H-return>") 'bookmark-bmenu-list)

; moved from e:/.p/stub/.emacs.el

;(define-key global-map (kbd "H-5 H-5") 'help)
;(define-key help-map (kbd "H-5") 'describe-function)
;(define-key help-map (kbd "H-4") 'describe-variable)
;(define-key help-map (kbd "H-6") 'where-is)
;(define-key help-map (kbd "H-8") 'describe-key-briefly)

;(define-key global-map (read-kbd-macro "H-h H-f") 'describe-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "H-w") #'aya-create)
(global-set-key (kbd "H-y") #'aya-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; remap kp-insert to AppsKey in Autohotkey
(global-set-key (kbd "<H-kp-space>") 'describe-key-briefly)

(define-key global-map (kbd "H-s-SPC") 'ace-jump-line-mode)
(define-key global-map (kbd "<H-s-delete>") 'ace-jump-buffer)
