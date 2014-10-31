(defun nl-buffer-eval-shell ()
  (shell-command
   (format "%s \"%s\"" nl-exec-program (buffer-file-name))
   )
  )

(defun nl-buffer-eval-compile ()
  (compile
   (format "%s \"%s\"" nl-exec-program (buffer-file-name))
   )
  )

(defun nl-buffer-eval-process ()
  (let* ((file (buffer-file-name))
	 (name (concat (basename) (format-time-string "-%H%M%S") ".out"))
	 (command (format "%s \"%s\"" nl-exec-program file))
	 (outbuf (get-buffer-create name))
	 )
    (start-process-shell-command name outbuf command)
    (switch-to-buffer-other-window outbuf)
    (bob)
;    (set-visited-file-name name)
    )
  )

(defun nl-buffer-eval ()
  (save-buffer)
  (cond
   ((file-exists-p ".nl-eval")
    (load-file ".nl-eval")
    )
   (t (funcall nl-buffer-eval-fun))
   )
  )

(put 'nl-mode 'eval-buffer-modal 'nl-buffer-eval)

(defun nl-mode () (interactive)
  (emacs-lisp-mode)
  (setq major-mode 'nl-mode)
  (setq mode-name "NL")
  (setq grep-spec "*.[pen]l")
  (define-key emacs-lisp-mode-map [M-f9] 'nl-exec-this)
  (save-excursion
    (bob)
    (while (rsf "^;#~e") (eval (readc)))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-variable-buffer-local 'nl-buffer-eval-fun)
(set-default 'nl-buffer-eval-fun 'nl-buffer-eval-compile)

(make-variable-buffer-local 'nl-exec-program)
(set-default 'nl-exec-program "wpl -mpl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun defun-at-point ()
  (let ((fun
	 (cond 
	  ((progn (sx (bol)
		      (looking-at "^ *(defun \\(.*?\\) *(")))
		  (match-string 1)
		  )
	  ((progn (sx
		   (rsb "^ *(defun \\(.*?\\) *(")))
		  (match-string 1)
		  )
	  )
	 )
	)
    fun)
  )

(defun nl-exec-this () (interactive)
  (let ((fun (defun-at-point)))
    (compile (format "%s \"%s\" -f%s" nl-exec-program (buffer-file-name) fun))
    )
  )
