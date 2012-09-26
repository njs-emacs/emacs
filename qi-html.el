(defun assoc-current-directory (alist)
  (assoc-if '(lambda (x) (string-match x default-directory)) alist)
  )

(setq stub-html-file-alist nil)

(setq default-stub-html-file (filename-concat user-emacs-home ".stub/html/default.html"))

(defun stub-html-file ()
  (or (cdr (assoc-current-directory stub-html-file-alist))
      (locate-up-file ".stub.html" default-stub-html-file))
  )

(qi-define (control-key-vector ?h ?h)
	   '(file-contents (stub-html-file)))

(qb-define (control-key-vector ?h ?h)
	   '(stub-html-file))

(make-local-variable 'insert-link-yank-before-hook)
(set-default 'insert-link-yank-before-hook nil)

(defun insert-link-yank () (interactive)
  (run-hooks 'insert-link-yank-before-hook)
  (insert (format "<a href=\"%s\"></a>" (current-kill 0)))
  (fc -4)
  )

(defun insert-link-img () (interactive)
  (insert (format "<img src=\"%s%s\">" html-img-base (current-kill 0)))
  )

(setq html-img-base "")

(global-set-key (control-key-vector ?o ?h ?a) 'insert-link-yank)
(global-set-key (control-key-vector ?o ?h ?i) 'insert-link-img)

(qi-define "\C-h\C-i" "<img src=\"" "\">")

