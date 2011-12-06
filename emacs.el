; this used to be in ~/.emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq standard-load-path load-path)
(setq load-path (cons user-emacs-home load-path))

(load "preinit" nil t)
(load "format" nil t)
(load "extend" nil t)
(load "load" nil t)
(load "wuft" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(wuft-load "emacs.00")

(load "stdpath" nil t)

(load (filename-concat user-emacs-home "init") nil t)

(wuft-load "emacs.01")

(set-default 'buffer-file-coding-system 'utf-8-dos)
;(setq buffer-file-coding-system 'utf-8-unix)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(minibuffer-dir "/" (concat ehome "/"))

(qb-and-mini ?a apache-home)
(qb-and-mini ?e user-emacs-home)
(qb-and-mini ?l emacs-lisp-dir)

(qb-define (control-key-vector ?e ?/) "~")
(qb-define (control-key-vector ?x ?x) "*Messages*")

(qb-define "\C-e\C-p" perl-e-path)
(qb-define "\C-e\C-b" backup-root)
(qb-define "\C-e\C-h" (filename-format "%s/emacs/emacs.log" backup-root))
(qb-define "\C-e\C-e" "~/.emacs/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define-abbrev global-abbrev-table "dir" "directory")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq show-help-function '(lambda (x)))
(setq max-mini-window-height 1)
(tool-bar-mode 0)

(setq dired-dnd-protocol-alist nil)

(global-font-lock-mode nil)
(setq font-lock-global-modes nil)

(put 'scroll-left 'disabled nil)

(setq large-file-warning-threshold 50000000)

(put 'dired-find-alternate-file 'disabled nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(find-file (concat user-emacs-home "/start.el"))
