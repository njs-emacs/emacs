(setq c-z-map (make-sparse-keymap))
(setq z-map c-z-map)			; compatibility

(global-set-key (kbd "C-z") c-z-map)

(def-key-map "C-z" 'c-z-map)

(def-key c-z-map (kbd "C-t") 'toggle-truncate-lines)
(def-key c-z-map (kbd "C-i") 'init-local)
(def-key c-z-map (kbd "C-r") 'read-only-mode)

(def-key c-z-map (kbd "C-b C-d") 'nvc-ls-bdiff)

(def-key c-z-map (kbd "C-.") 'dired-quick-dot)

(def-key c-z-map (kbd "C-p") 'print-it)
(def-key c-z-map (kbd "C-f") 'filename-to-kill)

(def-key c-z-map (kbd "C-j") 'join-line)
(def-key c-z-map (kbd "C-u") 'upcase-region)
(def-key c-z-map (kbd "C-d") 'downcase-region)

(def-key c-z-map (kbd "C-s") 'toggle-case-fold-search)

(def-key c-z-map (kbd "C-l") 'font-lock-mode)
(def-key c-z-map (kbd "C-k") 'find-kill-head)

(def-key c-z-map (kbd "C-[") 'emacs-read-hash-plus)
(def-key c-z-map (kbd "C-]") 'emacs-read-hash-minus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is a keymap bound to C-z C-z
;; triple C-z ==> undo

(setq zc-z-map (make-sparse-keymap))
(def-key c-z-map "\C-z" zc-z-map)

(def-key zc-z-map "\C-z" 'undo)

(def-key c-z-map (kbd "C-/") 'toggle-slashification-region)

;
;(define-key c-z-map (kbd "C-=") last-kbd-macro)


;; see also e:/emacs/recent.el
