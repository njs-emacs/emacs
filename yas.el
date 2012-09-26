(let ((load-path (cons (concat user-emacs-home "/packages/yasnippet-0.6.1c") load-path)))
  (require 'yasnippet)
  )

(add-mode "\\.ys$" 'snippet-mode)

(setq yas/my-dir (concat user-emacs-home "/yas"))
(qb-define (kbd "C-e C-y") (car yas/root-directory))

(setq yas/root-directory (list yas/my-dir))
(yas/load-directory yas/my-dir)

(yas/initialize)

(top-level)

(describe-function 'yas/expand-snippet)

(menu-bar-mode)
