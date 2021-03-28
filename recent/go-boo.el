;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go-boo is an interesting code generation technique
;; where code is piped out to a perl script which replaces
;; the code with a transformed version according to ad-hoc
;; pseudo-language forms
;; the original code will be lost so cannot be controlled
;; however the generator code format does not have to be commented
;; out or look like valid code

(defvar go-boo-prev "main")
(defvar go-boo-script "boo.pl")

(make-local-variable 'go-boo-script)

(defvar-local go-boo-env nil "Extra buffer-local arg for boo.pl")

(defun go-boo (fun &optional loop)
  (interactive "sFun: \nP")
  (cond ((equal fun "") (setq fun go-boo-prev)))
  (setq go-boo-prev fun)
  (let* ((loop (cond (loop 1) (0)))
	 (script (locate-up-file go-boo-script))
	 start end cmd
	 )
    (cond
     (script
      (cond
       ((region-active-p)
	(setq start (region-beginning) end (region-end))
	)
       ((looking-at "^$")
	(setq start (point) end (point))
	)
       ((bolp)
	(setq start (point) end (point$))
	)
       (t
	(setq start (point) end (point$))
	)
       )
      (copy-region-as-kill start end)
      (setq cmd
	(format "perl %s --fun=%s --loop=%s --mode=%s --file=\"%s\""
		script fun loop major-mode (buffer-file-name))
	)
      (cond
       (go-boo-env
	(setq cmd (concat cmd (format " --env=\"%s\"" go-boo-env)))
	)
       )
      (shell-command-on-region start end cmd nil t)
      )
     ((error "no script found"))
     )
    )
  )

(define-key global-map (kbd "<s-f10>") 'go-boo)

