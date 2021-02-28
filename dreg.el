(defun enquote (x) (concat "\"" x "\""))
(defun enquote (x) x)

(setq dreg-cmd-default "year")

(setq dreg-script-file "e:/_backup/.meta/dreg/dreg.pl")

(defun dreg** (&rest args)
 (let ((cmd (format "perl %s" dreg-script-file))
       )
  (setq cmd (mconcat (apply 'list cmd args) " "))
  (compile cmd)
  )
 )

(defun dreg* (pat &rest args)
 (apply 'dreg** (format "--pat=%s" pat) args)
 )

(defun dregf (pat &optional fpat cmd &rest args)
 (cond (fpat (setq args (push (format "--fpat=\"%s\"" fpat) args))))
 (setq args (push (format "--cmd=%s" (or cmd dreg-cmd-default)) args))
 (apply 'dreg** (format "--pat=\"%s\"" pat) args)
; (apply 'dreg** (format "--pat='%s'" pat) args)
 )

(defun dref (&optional fpat cmd &rest args)
 (cond (fpat (setq args (push (format "--fpat=\"%s\"" fpat) args))))
 (setq args (push (format "--cmd=%s" (or cmd dreg-cmd-default)) args))
 (apply 'dreg** "--typefilter=." "--pat=\".\"" "--one=1" args)
 )

; (dreg* "1 day" "--where=file ~ '.p[lm]'" "--cmd=month")
; (dreg** "--pat=1 week" "--where=file~'.p[lm]'" "--cmd=since" "--when='01 jan 2010'")

; (dregf "time" ".p[lm]" "week")
; (dregf "time" ".p[lm]" "today")
; (dregf "time" ".p[lm]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun drez (pat) (interactive "SPattern: ")
  (let ((tr "and time between now() - interval '1 year' and now() "))
    (dreg**
;   (format "--cmd=querz \"select distinct foo from (select file from log where file ~ '%s' and type = 'S' order by time desc) as foo limit 30\"" pat)
     (format "--cmd=querz \"select file from log where file ~ '%s' and type = 'S' %s order by time desc limit 100\"" pat tr)
;     (format "--cmd=querz \"select foo from (select file from log where file ~ '%s' and type = 'S' %s order by time desc) as foo limit 40\"" pat tr)
     )
    )
)

(defun drezz (x) (interactive "SType: ") (drez (format "\\.%s$" x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yas-x)

(defvar dreg-mode-keymap (make-sparse-keymap) "dreg-mode keymap.")
(setq dreg-arg-map (make-sparse-keymap))

(define-minor-mode dreg-mode
  "."
  :init-value nil
  :lighter " dreg "
  :keymap dreg-mode-keymap
  (cond 
   (dreg-mode
    (yas-x-define (kbd "C-c C-x") "dr")
    )
   )
  )

(defun dreg-map (key arg)
  (let ((key (read-kbd-macro (format "C-c C-%s" key))))
    (define-key dreg-arg-map key arg)
    (define-key dreg-mode-keymap key 'dreg-insert)
    )
  )

(defun dreg-insert (&optional when) (interactive "p")
  (let* ((keys (this-command-keys-vector))
	 (binding (lookup-key dreg-arg-map keys))
	 )
    (insert "\n(dregf \"")
    (sx
     (insert (format "\" %s \"%s\")" binding
		    (cond
		     ((= when 4) "year")
		     ((= when 1) "ever")
		     (t "month")
		     )
		    )
	     ))
    )
  )

(setq elfs "\\.(el|emacs)$")
(setq htfs "\\.(html|css|php|js)$")
(setq plfs "\\.p[lm]$")
(setq cfs "\\.[ch]$")
(setq nlfs "\\.nl$")
(setq ofs "\\.(el|org)$")

(dreg-map "e" "elfs")
(dreg-map "h" "htfs")
(dreg-map "c" "cfs")
(dreg-map "p" "plfs")
(dreg-map "n" "\"\\\\.nl\"")

(defun dfp (pat &rest args) (apply 'dregf pat plfs args))
(defun dfh (pat &rest args) (apply 'dregf pat htfs args))

(defun dresh (p &optional since)
  (dregf p "sh-.*\\.input" (or since dreg-since "month"))
  )

(defun dregx (&optional arg) (interactive "p")
       (dregf (x-get-selection) (dired-glob-regexp (grep-spec))
	      (cond
	       ((= arg 4) "ever")
	       ((= arg 1) "month")
	       ((= arg 0) "year")
	       ))
  )

