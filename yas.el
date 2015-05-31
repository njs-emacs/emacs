;(setq yas-load-path (concat user-emacs-home "/packages/yasnippet-0.6.1c"))
(setq yas-load-path (// "boo/e/.p/yasnippet"))

(let ((load-path (cons yas-load-path load-path)))
  (require 'yasnippet)
  )

(setq yas-my-dir (concat user-emacs-home "/yas"))
(setq yas-snippet-dirs yas-my-dir)

(add-mode "\\.ys$" 'snippet-mode)

(qb-define (kbd "C-e C-y") '(car yas/root-directory))

(setq yas-snippet-dirs
  (list
   yas-my-dir
   )
  )

(require 'yas-x)

(load-file (// "boo/e/.p/tc/.yas/yas.el"))

(yas-global-mode)
