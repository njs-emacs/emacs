(def-key global-map (kbd "<H-f1>") 'org-master-file-edit)

(def-key global-map (kbd "<H-return>") 'bookmark-bmenu-list)

; moved from e:/.p/stub/.emacs.el

;(def-key global-map (kbd "H-5 H-5") 'help)
;(def-key help-map (kbd "H-5") 'describe-function)
;(def-key help-map (kbd "H-4") 'describe-variable)
;(def-key help-map (kbd "H-6") 'where-is)
;(def-key help-map (kbd "H-8") 'describe-key-briefly)

;(def-key global-map (read-kbd-macro "H-h H-f") 'describe-function)

;(define-key global-map (kbd "H-c") (kbd "C-c C-c"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-key global-map (kbd "H-w") #'aya-create)
(def-key global-map (kbd "H-y") #'aya-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; remap kp-insert to AppsKey in Autohotkey
(def-key global-map (kbd "<H-kp-space>") 'describe-key-briefly)

(def-key global-map (kbd "H-s-SPC") 'ace-jump-line-mode)
(def-key global-map (kbd "<H-s-delete>") 'ace-jump-buffer)

(def-key global-map (kbd "H-M-w") 'avy-goto-char-timer)

(def-key global-map (kbd "H-a H-a") 'embark-act)

