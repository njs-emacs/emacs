(qb-and-mini ?e user-emacs-home)
(qb-and-mini ?l emacs-lisp-dir)

(qb-define (control-key-vector ?e ?/) "~")
(qb-define (control-key-vector ?x ?x) "*Messages*")

(qb-define "\C-e\C-p" perl-e-path)
(qb-define "\C-e\C-b" backup-root)
(qb-define "\C-e\C-h" (filename-format "%s/emacs/emacs.log" backup-root))
(qb-define "\C-e\C-e" "~/.emacs/")
(qb-define "\C-e\C-k" (concat ehome "/ahk/a.ahk"))

(qb-define (control-key-vector ?o ?o) (concat ehome "/.org/home.org"))
(qb-define (control-key-vector ?e ?x) (concat ehome "/help/home.org"))
(qb-define (control-key-vector ?e ?t) (concat ehome "/help/tmp"))

(qb-define (control-key-vector ?d ?.) (daily-path ".emacs.el") t)
(qb-define (control-key-vector ?d ?e) (daily-path "0.el") t)
