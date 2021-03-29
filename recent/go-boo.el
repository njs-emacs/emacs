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
(defvar boo-history nil)

(make-local-variable 'go-boo-script)

(defvar-local go-boo-env nil "Extra buffer-local arg for boo.pl")

(defun boo-get-script-or-die ()
  (let* ((script (locate-up-file go-boo-script))
	 )
    (cond (script)
	  ((error "no script found"))
	  )
    )
  )

(defun boo-raw (fun)
  "Execute boobie FUN purely for side effects"
  (interactive "sFun: ")
  (cond ((equal fun "") (setq fun go-boo-prev)))
  (setq go-boo-prev fun)
  (let* ((script (boo-get-script-or-die))
	 )
    (setq cmd
      (format "perl %s --fun=%s --mode=%s --file=\"%s\""
	      script fun major-mode (buffer-file-name))
      )
    (cond
     (go-boo-env
      (setq cmd (concat cmd (format " --env=\"%s\"" go-boo-env)))
      )
     )
    (shell-command-on-region (point) (point) cmd nil)
    )
  )

(defun boo-command ()
  (let (cmd)
    (setq cmd
      (format "perl %s --fun=bum --loop=%s --mode=%s --file=\"%s\""
	      script loop major-mode (buffer-file-name))
      )
    (cond
     (go-boo-env
      (setq cmd (concat cmd (format " --env=\"%s\"" go-boo-env)))
      )
     )
    cmd
    )
  )

(defun boo-bum (text &optional loop)
  "Execute boobie 'bum' on TEXT (with optional LOOP).
Return the output from bum boobie,
which is appended to the \" *bum*\" buffer."
  (let* ((script (boo-get-script-or-die))
	 (loop (cond (loop 1) (0)))
	 (file-name (buffer-file-name))
	 buffer start end cmd
	 )
    (setq buffer (get-buffer-create " *bum*"))
    (setq cmd (boo-command))
    (setq cmd 
      (format "perl %s --fun=bum --loop=%s --mode=%s --file=\"%s\""
	      script loop major-mode (buffer-file-name))
      )
    (cond
     (go-boo-env
      (setq cmd (concat cmd (format " --env=\"%s\"" go-boo-env)))
      )
     )
    (sx
     (set-buffer buffer)
;    (erase-buffer)
     (goto-char (setq start (point-max)))
     (insert text)
     (shell-command-on-region start (point-max) cmd nil t)
     (insert "\n")
     (bs start (point-max))
     )
    )
  )

(defun boo-boo (fun &optional loop)
  "Execute boobie FUN on region (with optional LOOP) and replace
   the region with the output."
  (interactive 
    (list (read-string "Fun: " (car boo-history) 'boo-history)
	  current-prefix-arg))
  (cond ((equal fun "") (setq fun go-boo-prev)))
  (setq go-boo-prev fun)
  (let* ((script (boo-get-script-or-die))
	 (loop (cond (loop 1) (0)))
	 start end cmd
	 )
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
  )

(define-key global-map (kbd "<s-f10>") 'boo-boo)

