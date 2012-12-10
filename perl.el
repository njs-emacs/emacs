(set-default 'perl-options nil)
(make-variable-buffer-local 'perl-options)

(make-local-variable 'perl-process-start-hooks)
(make-local-variable 'perl-compile-start-hooks)

(set-default 'perl-process-start-hooks nil)
(set-default 'perl-compile-start-hooks nil)

(defun perl-command (file &optional options)
  (let* ((perl-opts (mconcat perl-options " "))
	 (script-opts (mconcat options " ")))
    (format "perl %s \"%s\" %s" perl-opts file script-opts)
    )
  )

(defun start-perl () (interactive)
  (save-buffer)
  (start-process "*sex*" nil "perl" (format "%s" (buffer-file-name)))
  )

(defun perl-exec-this (&optional cmd) (interactive)
  (or cmd (setq cmd (sx (rsb "^sub *\\(\\w+\\)") (ms 1))))
  (compile (format "perl %s --cmd=%s" (buffer-file-name) (or cmd "")))
  )

(defun perl-compile (&optional options) (interactive "i")
  (save-buffer)
  (let ((hooks perl-compile-start-hooks)
	) 
    (save-excursion
      (set-buffer
       (compile (perl-command (buffer-file-name) options)
		)
       )
      (run-hooks 'hooks)
      (setq compile-protect t)
      (setq font-lock-mode nil)
      )
    )
  )

(defun eval-perl-buffer () (interactive)
  (save-buffer)
  (shell-command (perl-command (buffer-file-name)))
  )

;(put 'perl-mode 'eval-buffer-modal 'eval-perl-buffer)
(put 'perl-mode 'eval-buffer-modal 'perl-compile)
;(put 'perl-mode 'eval-buffer-modal 'perl-process)

(defun perl-mode-hook-ns ()
  (setq grep-spec "*.p[lm]")
  (emacs-read-hash-plus)
  (define-key perl-mode-map [M-f9] 'perl-exec-this)
  (setq log-short-time-insert-format "%d_%H%M")
  )

(add-hook 'perl-mode-hook 'perl-mode-hook-ns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun perl-process ()
  (let* ((file (buffer-file-name))
	 (name (concat (basename) (format-time-string "-%H%M%S") ".out"))
	 (opts (mconcat perl-options " "))
	 (command (format "%s %s \"%s\"" "perl" opts file))
	 (outbuf (get-buffer-create name))
	 )
    (save-buffer)
    (start-process-shell-command name outbuf command)
    (switch-to-buffer-other-window outbuf)
    (run-hooks 'perl-process-start-hooks)
;    (set-visited-file-name name)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bake-perl (&optional target)
  (let* ((mf (locate-up-file "make.pl"))
	 )
    (cond
     (target
      (let* ((d (file-name-directory target))
	     (f (basename target))
	     (s (file-name-suffix target))
	     )
	(compile (format "perl %s --target=\"%s\"" mf target))
	)
      )
     ((compile (format "perl %s" mf)))
     )
    )
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(file-class-linked-file-add 'perl-mode '((other . perl-other-file)))

(defun perl-other-file (&optional file)
  (or file (setq file (buffer-file-name)))
  (cond
   ((string-match "\\.pm" file) (concat (basename file) ".pl"))
   ((string-match "\\.pl" file) (concat (basename file) ".pm"))
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun perl-stub-sub ()
  (list
   (format "sub _%s {\n    " (format-time-string "%m%d_%H%M"))
   "\n}\n"
   ))

(qi-define "\C-p\C-s" '(perl-stub-sub))


