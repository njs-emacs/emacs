(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

; slime-js wasn't on melpa
; but we don't need it anyway because skewer works better

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;;;;;
(slime-setup '(slime-js slime-repl))

(setq httpd-port 8113)
;;;;;

; mc/mark-all-like-this
; mc/edit-lines

(global-set-key (kbd "s-a n") 'mc/mark-next-like-this)

(global-set-key (kbd "s-n") 'mc/mark-next-like-this)

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "g")

; engine/keymap-prefix

(define-key global-map (kbd "C-c /") engine-mode-map)

(global-unset-key (kbd "C-c /"))
(setq engine-mode-map (make-sparse-keymap))
emacs

() (("hello \"hel\"lo"))

(global-set-key (kbd "C-%") 'er/expand-region)

("hello")

a b ((c d)) e f

(add-to-list 'load-path "e:/emacs/packages/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")

(add-to-list 'load-path "e:/home/nick/.emacs.d/elpa/js2-mode-20150524.426/")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
