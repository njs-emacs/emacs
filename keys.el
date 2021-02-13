;(insert (format "%s" (read-key-sequence "")))

(defun 0x (s)
  (or (stringp s) (setq s (number-to-string s)))
  (string-to-number s 16)
  )

(defun control-key (k)
  (cond
   ((and (>= k ?a) (<= k ?z)) (logand k 31))
   ((eq k ?+) 67108907)
   ((logior k 67108864))
   )
  )

(defun control-key-vector (&rest k)
  (apply 'vector (mapcar 'control-key k))
  )

(mapcar 'global-unset-key
	(list
	 "\C-j"			;;; newline-and-indent
	 "\C-z"			;;; iconify-or-deiconify-frame -> generic prefix
	 "\M-c"			;;; capitalize-word -> compile prefix
	 "\M-e"			;;; end-of-sentence -> generic prefix
	 "\M-f"			;;; forward-word -> fileutil prefix
	 "\M-g"			;;; facemenu
	 "\M-h"			;;; mark-paragraph
	 "\M-j"			;;; indent-new-comment-line
	 "\M-l"			;;; downcase-word -> nlisp prefix
	 "\M-m"			;;; back-to-indentation
	 "\M-q"			;;; fill-paragraph
	 "\M-r"			;;; move-to-window-line
	 "\M-u"			;;; upcase-word
	 "\M-'"			;;; abbrev-prefix-mark
	 ))

(define-key global-map "\C-j" 'backward-char)
(define-key esc-map "\C-j" 'backward-sexp)

(define-key minibuffer-local-map "\C-j" nil)
(define-key minibuffer-local-must-match-map "\C-j" nil)
(define-key minibuffer-local-completion-map "\C-j" nil)

(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\e%" 'query-replace-regexp)

(global-set-key "\e>" 'scroll-left)
(global-set-key "\e<" 'scroll-right)

(global-set-key [f1] 'next-error)
(global-set-key [S-f1] 'previous-error)

(define-key global-map [f6]
  '(lambda () (interactive) (dired-other-window default-directory)))

(global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [f8] 'goto-shell)
(global-set-key [f9] 'eval-buffer-modal)

(global-set-key [f11] 'goto-line)
(global-set-key [f12] 'command-history)

(define-key global-map "\ef" f-map)

(setq z-map (make-sparse-keymap))

(define-key z-map "\C-t" 'toggle-truncate-lines)
(define-key z-map "\C-i" 'init-local)
(define-key z-map "\C-r" 'read-only-mode)

(setq zz-map (make-sparse-keymap))
(define-key global-map "\C-z" z-map)
(define-key z-map "\C-z" zz-map)
(define-key zz-map "\C-z" 'undo)

(setq m-map (make-sparse-keymap))
(global-set-key "\M-m" m-map)

(global-unset-key [f16])
(global-unset-key [f17])
(global-unset-key [f18])
(global-unset-key [f19])
(global-unset-key [f20])

(load-overrides "keys")

(define-key global-map "\C-v" 'scroll-up)
(define-key global-map "\M-v" 'scroll-down)

(define-key global-map "\M-o" 'other-window)
(define-key global-map [C-tab] 'bury-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (control-key-vector ?`) 'backward-delete-char)
(define-key global-map "¬" 'backward-char)

(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

(global-set-key [M-home] 'restore-killed-buffer)
(global-set-key [M-end] 'exit-recursive-edit)

(global-set-key [C-insert] 'file-history-open)

(define-key dired-mode-map "\C-t" 'today-make-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq hash-map (make-sparse-keymap))

(global-set-key (vector (control-key ?#)) hash-map)

(define-key hash-map (control-key-vector ?#) 'find-file-at-point)
(define-key hash-map "\C-i" 'insert-register)
(define-key hash-map "\C-c" 'copy-to-register)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map [S-down-mouse-1] 'mouse-yank-at-click)

(global-unset-key [C-down-mouse-1])
(global-set-key [C-down-mouse-1] 'find-file-at-point-mouse)

; "\C-z\C-b" prefix pertains to backup related functions

(define-key z-map "\C-b\C-d" 'nvc-ls-bdiff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-unset-key "\M-z")	; remove zap-to-char
(global-unset-key "\C-\\")	; remove toggle-input-method

(global-set-key (kbd "C-c C-g") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq c-colon-map (make-sparse-keymap))

(define-key global-map (kbd "C-;") c-colon-map)
(define-key c-colon-map (kbd "C-;") 'er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-# map is a scratch map. Expect any key in C-# can be overridden

(load "c-hash-map" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq slash-map (make-sparse-keymap))
(global-set-key (kbd "C-\\") slash-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "<s-f9>") 'emacs-read-hash-plus)
(define-key slash-map (kbd "C-e") 'emacs-read-hash-plus)

