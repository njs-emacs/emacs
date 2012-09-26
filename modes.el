;; put here because it is common to a number of modes
;; must deprecate old style directives in time

(defun emacs-read-hash-plus ()
  (sx 
   (bob)
   (let ((limit (rsf "#+__END__+#")))
     (while
	 (or (rsf "^#\\+\\(.*\\)" limit)		; old style
	     (rsf "#\\+\\(.*?\\)\\+#" limit)		; new
	     )
       (eval (read (ms 1))))
     )
   )
  )

(defun force-mode (&optional s)
  "Force a mode appropriate to a file with the named SUFFIX.
SUFFIX defaults to the suffix of the currently edited file.

force-mode is callable interactively with \\[force-mode]"
  (interactive)
  (setq s (or s (file-name-suffix (buffer-file-name))))
  (if (interactive-p)
      (setq s (read-from-minibuffer "Force mode: " s)))
  (funcall (cdr (assoc (format "\\%s$" s) auto-mode-alist))))

(defun add-mode (pat mode)
  (let ((cell (assoc pat auto-mode-alist)))
    (cond
     (cell (setcdr cell mode))
     ((setq auto-mode-alist (cons (cons pat mode) auto-mode-alist)))
     )
    )
  )

(add-mode "\\.bup$" 'bup-mode)
(autoload 'bup-mode "bup-mode")

(add-mode "\\.lsp$" 'lsp-mode)
(autoload 'lsp-mode "lsp-mode")

(add-mode "\\.gio$" 'gio-mode)
(autoload 'gio-mode "gio-mode")

(add-mode "\\.ps1$" 'ps1-mode)
(autoload 'ps1-mode "ps1-mode")

(add-mode "\\.pde$" 'pde-mode)
(autoload 'pde-mode "pde-mode")

(add-mode "\\.php$" 'php-mode)
(autoload 'php-mode "php-mode")

(add-mode "\\.pa$" 'pa-mode)
(autoload 'pa-mode "pa-mode")

(add-mode "\\.tod$" 'todo-mode)
(autoload 'todo-mode "todo")

(add-mode "\\.awl$" 'awl-mode)
(add-mode "\\.awl\\(p\\|\\)$" 'awl-mode)
(autoload 'awl-mode "awl-mode")

(add-mode "\\.pc$" 'pc-mode)
(autoload 'pc-mode "pc-mode")

(add-mode "\\.pro$" 'emacs-lisp-mode)

(autoload 'nl-mode "nl-mode" "" (interactive))
(add-mode "\\.nl$" 'nl-mode)

;(add-mode "\\.pl$" 'ol-mode)
(add-mode "\\.pl$" 'perl-mode)

(autoload 'ruby-mode "ruby" "" (interactive))
(add-mode "\\.rb$" 'ruby-mode)

(autoload 'get-mode "newsget")
(add-mode "\\.get$" 'get-mode)

(autoload 'p-mode "p-mode")
(add-mode "\\.p$" 'p-mode)

(autoload 'asn-mode "asn-mode")
(add-mode "\\.x$" 'asn-mode)
(add-mode "\\.x$" 'html-mode)

(autoload 'lex-mode "lex-mode")
(add-mode "[^.]\\.l$" 'lex-mode)

(autoload 'y-mode "y-mode")
(add-mode "\\.y$" 'y-mode)

(add-mode "\\.co$" 'compilation-mode)

(autoload 'makefile-mode "make-mode")
(add-mode "\\.wmk" 'makefile-mode)
(add-mode "\\.mak" 'makefile-mode)
(add-mode "\\.mif" 'makefile-mode)

(autoload 'rc-mode "rc-mode")
(add-mode "\\.rc$" 'rc-mode)

(autoload 'sql-mode "sql")
(add-mode "\\.sql$" 'sql-mode)

(autoload 'ktm-mode "ktm-mode")
(add-mode "\\.\\(KT[MP]\\|kt[mp]\\)\\(~[0-9]*\\|\\)$" 'ktm-mode)
;(add-mode "\\.P$" 'ktm-mode)
;(add-mode "\\.P\\(~[0-9]*\\|\\)$" 'ktm-mode)

(defun c++-mode () (c-mode))

(add-mode "\\.ic$" 'c-mode)

(autoload 'log-mode "log-mode")
(add-mode "\\.log" 'log-mode)

(autoload 'ahk-mode "ahk-mode")
(add-mode "\\.ahk" 'ahk-mode)

(autoload 'file-history-mode "filehistory")
(add-mode "\\.efh" 'file-history-mode)
