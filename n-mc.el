;; mainly moved to keys/c-comma.el

(define-key global-map (kbd "s-<mouse-1>") 'mc/add-cursor-on-click)
(define-key global-map [s-mouse-1] 'mc/add-cursor-on-click)

;;(top-level)

;;(describe-bindings)

;; (describe-bindings (kbd "C-#"))
;; (describe-bindings (kbd "C-'"))
;; 
;; C-,		free
;; C-.		free
;; C-]		free
;; C-;		er/expand-region
;; C-#		Prefix Command
;; C-'		Prefix Command
;; C-[ ===> ESC
;; 
;; (describe-function 'set-transient-map)
;; 
;; (require 'phi-search)
;; (global-set-key (kbd "C-# C-s") 'phi-search)
;; 
