;(setq yas-load-path (concat user-emacs-home "/packages/yasnippet-0.6.1c"))
(setq yas-load-path (// "boo/e/.p/yasnippet"))

(let ((load-path (cons yas-load-path load-path)))
  (require 'yasnippet)
  )

(setq yas-my-dir (concat user-emacs-home "/yas"))
(setq yas-snippet-dirs yas-my-dir)

(setq yas-snippet-dirs yas-my-dir)

;(setq yas/root-directory (list yas/my-dir))
;(yas/load-directory yas/my-dir)

(top-level)

(yas-global-mode)
(yas-minor-mode)
(yas-reload-all)
;(add-mode "\\.ys$" 'snippet-mode)
(qb-define (kbd "C-e C-y") (car yas/root-directory))


(yas/initialize)

(top-level)

(describe-function 'yas/expand-snippet)

(menu-bar-mode)


pim

test (setq } ) ; OK!
field with calculated default 21/02/2014 16:34:03
<>}
test
pud


yas/root-directory

(symbol-macrolet

yas-snippet-dirs
yas/root-directory
