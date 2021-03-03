(setq m-c-map (make-sparse-keymap))
(global-set-key (kbd "M-c") m-c-map)

(define-key m-c-map "d" 'kill-compilation-buffers)

(define-key m-c-map "g" 'grep)
(define-key m-c-map "x" 'xgrep)
(define-key m-c-map "0" '(lambda () (interactive)
			 (bake-target "clean")
			 ))
(define-key m-c-map (kbd "M-a") 'compilation-restart-errors)

(define-key m-c-map (kbd "M-r") 'rgrep)
(define-key m-c-map (kbd "M-s") 'sgrep)
(define-key m-c-map (kbd "M-k") 'kgrep)

(define-key m-c-map (kbd "M-p") 'ngrep-last-search)

