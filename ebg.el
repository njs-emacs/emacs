;{#~! functions to grep through recently edited files }
;{#~@080820-111634 added function documentation and cleaned up }
;
; find patterns in recently edited files
;
; complements backup-grep, which searches the backed up conents
;
; has serious weakness where direcories have been renamed

(setq ebg-default-start nil)

(defun ebg (pat &rest args)
  "Find PAT in recently edited files\nARGS include --fpat= --start=, --end= --limit= etc."
  (let* ((args (mconcat args " "))
	 (cmd (format "perl %s/ebge.pl --pat=\"%s\" %s" perl-e-path pat args))
	 )
    (compile cmd 'grep-mode)
    )
  )

(defun ebg* (pat &optional fpat limit start end)
  "Find PAT in recently edited files\nFPAT, LIMIT, START, END are what would normally be passed using --fpat etc args."
 (let ((cmd (format "perl %s/ebge.pl --pat=\"%s\"" perl-e-path pat))
       )
   (or start (setq start ebg-default-start))

   (and fpat (setq cmd (concat cmd (format " --fpat=\"%s\"" fpat))))
   (and limit (setq cmd (concat cmd (format " --limit=\"%s\"" limit))))
   (and start (setq cmd (concat cmd (format " --start=\"%s\"" start))))
   (and end (setq cmd (concat cmd (format " --end=\"%s\"" end))))
   (compile cmd 'grep-mode)
   )
 )

(defun ebg** (pat &rest args)
  "Find PAT in recently edited files\nARGS is a plist of ebg args, using ebg-default-alist as defaults."
 (let ((cmd (format "perl %s/ebge.pl --pat=\"%s\"" perl-e-path pat))
       (fun '(lambda (sym) 
	       `(let ((v (or ,sym (setq ,sym (gget ebg-default-alist ',sym)))))
		(and v (setq cmd (concat cmd (format " --%s=\"%s\"" ',sym v))))
		)
	     ))
       fpat limit start end
       )
   (let (sym val)
     (while args (setq sym (pop args)) (set sym (setq val (pop args)))
	    (print sym)
	    (print (eval sym))
	    )
     )
   (eval (funcall fun 'fpat))
   (eval (funcall fun 'limit))
   (eval (funcall fun 'start))
   (eval (funcall fun 'end))
   (compile cmd 'grep-mode)
   )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; previously in $HTDOCS/daily/.emacs.el

(setq ebg-fpat-default-args "")

(defun ebg-fpat (pat fpat &rest args)
  "Find PAT in recently edited files\nFPAT is inline, other ARGS are command-line args."
  (ebg pat (format "--fpat=%s %s" fpat (mconcat (cons ebg-fpat-default-args args) " ")))
  )

(setq ebgh-fpat-htx "\"\\.(html|css|php|js|pl|pm)\"")
(setq ebgh-fpat-htdocs "\"\\.(html|css|php|js)\"")
(setq ebgh-fpat-perl "\"\\.(pl|pm)\"")
(setq ebgh-fpat-c "\"\\.(c|h|y|cpp|hpp|cc)\"")

(defun ebgh* (pat &optional limit start end)
  "Find PAT in recently edited HTML sources.\nOther ARGS are inline as in ebg*"
  (ebg* pat ebgh-fpat-htdocs limit start end)
  )

(defun ebgp* (pat &optional limit start end)
  "Find PAT in recently edited perl sources.\nOther ARGS are inline as in ebg*"
  (ebg* pat ebgh-fpat-perl limit start end)
  )

(defun ebgc* (pat &optional limit start end)
  "Find PAT in recently edited c sources.\nOther ARGS are inline as in ebg*"
  (ebg* pat ebgh-fpat-c limit start end)
  )

(defun ebgh (pat &rest args)
  "Find PAT in recently edited HTML sources.\nOther ARGS are command-line style."
  (apply 'ebg-fpat pat ebgh-fpat-htdocs args)
  )

(defun ebgp (pat &rest args)
  "Find PAT in recently edited perl sources.\nOther ARGS are command-line style."
  (apply 'ebg-fpat pat ebgh-fpat-perl args)
  )

(defun ebgc (pat &rest args)
  "Find PAT in recently edited c sources.\nOther ARGS are command-line style."
  (apply 'ebg-fpat pat ebgh-fpat-c args)
  )

(defun ebge (pat &rest args)
  "Find PAT in recently edited elisp sources.\nOther ARGS are command-line style."
  (apply 'ebg-fpat pat "\\.el$" args)
  )

(defun ebgn (pat &rest args)
  "Find PAT in recently edited nlisp sources.\nOther ARGS are command-line style."
  (apply 'ebg-fpat pat "\\.nl$" args)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ebs-script-file "e:/daily/1004/nose/emacs.backup.db/ebs.pl")

(defun ebs (&rest args)
 (setq ebs-buffer (get-buffer-create "*history*"))
 (switch-to-buffer-other-window ebs-buffer)
 (erase-buffer)
 (apply 'call-process "perl" nil ebs-buffer t ebs-script-file args)
 (bob)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
