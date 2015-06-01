(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;;;;;
; skewer

(setq httpd-port 8113)
;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mc/mark-all-like-this
; mc/edit-lines

(global-set-key (kbd "s-a n") 'mc/mark-next-like-this)
(global-set-key (kbd "s-n") 'mc/mark-next-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "g")

; engine/keymap-prefix

(define-key global-map (kbd "C-c /") engine-mode-map)

(global-unset-key (kbd "C-c /"))
(setq engine-mode-map (make-sparse-keymap))


() (("hello \"hel\"lo"))

(global-set-key (kbd "C-%") 'er/expand-region)

("hello")

a b ((c d)) e f


