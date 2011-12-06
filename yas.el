(add-to-list 'load-path "~/emacs/packages/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory `("~/emacs/packages/yasnippet-0.6.1c/snippets"))
(qb-define (control-key-vector ?e ?y) "~/emacs/packages/yasnippet-0.6.1c/snippets")

(defun yas/foo-init ()
  (yas/load-directory "~/emacs/packages/yasnippet-0.6.1c/snippets")
  (catch 'error (yas/insert-snippet))
  )

(yas/foo-init)
(top-level)
(menu-bar-mode)

(add-mode "\\.ys$" 'snippet-mode)
