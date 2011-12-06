(set-default 'php-options nil)
(make-variable-buffer-local 'php-options)

(defun php-command (file)
  (let ((opts (mconcat php-options " ")))
    (format "%s %s \"%s\"" php-exe opts file)
    )
  )

(defun php-compile () (interactive)
  (save-buffer)
  (save-excursion
    (set-buffer (compile (php-command (buffer-file-name))))
    (setq compile-protect t)
    )
  )

(defun eval-php-buffer () (interactive)
  (save-buffer)
  (shell-command (php-command (buffer-file-name)))
  )

;(put 'php-mode 'eval-buffer-modal 'eval-php-buffer)
(put 'php-mode 'eval-buffer-modal 'php-compile)
;(put 'php-mode 'eval-buffer-modal 'php-process)

(defun php-mode-hook-ns ()
  (bob)
  (setq grep-spec "*.p[lm]")
  (while (rsf "^#\\+\\(.*\\)") (eval (read (ms 1))))
  )

(add-hook 'php-mode-hook 'php-mode-hook-ns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun php-process ()
  (let* ((file (buffer-file-name))
	 (name (concat (basename) (format-time-string "-%H%M%S") ".out"))
	 (opts (mconcat php-options " "))
	 (command (format "%s %s \"%s\"" "php" opts file))
	 (outbuf (get-buffer-create name))
	 )
    (start-process-shell-command name outbuf command)
    (switch-to-buffer outbuf)
;    (set-visited-file-name name)
    )
  )

(defun insert-close-tag (arg) (interactive "p")
  (debug)
  )

(defun sgml-close-tag* (arg)
  (case (car (sgml-lexical-context))
    (comment 	(insert " -->"))
    (cdata 	(insert "]]>"))
    (pi 	(insert " ?>"))
    (jsp 	(insert " %>"))
    (tag 	(insert " />"))
    (text
     (let ((context (save-excursion (sgml-get-context "html"))))
       (if context
           (progn
             (save-excursion
	       (insert "</" (sgml-tag-name (car (nthcdr arg (reverse context)))) ">")
	       )
             (indent-according-to-mode)))))
    (otherwise
     (error "Nothing to close"))))

(defun sgml-close-tag (&optional arg)
  "Close current element.
Depending on context, inserts a matching close-tag, or closes
the current start-tag or the current comment or the current cdata, ..."
  (interactive "p")
  (cond ((zerop arg))
	(t (sgml-close-tag* (1- arg)))
	)
  )

(defun mouse-close-tag () (interactive)
  (let ((s (sx
	     (mouse-set-point last-command-event)
	     (bs (1+ (sxp (fc 1) (rsb "<"))) (1- (sxp (rsf "[ >]"))))))
      )
    (insert "</" s ">")
    )
  )

(defun insert-blank-tag () (interactive) (insert "<>") (bc 1))

(defun php-mode () (interactive)
  (html-mode)
;  (setq major-mode 'php-mode)
;  (setq mode-name "PHP")
  (setq grep-spec "*.php")
  (setq grep-flags "-n")
  (setq rgrep-flags grep-flags)
  (setq buffer-mode-comment-alist
    (gput buffer-mode-comment-alist 'php-mode (gget buffer-mode-comment-alist 'html-mode)))
  (define-key sgml-mode-map [S-mouse-2] 'mouse-close-tag)
  (define-key sgml-mode-map (control-key-vector ?c ?/) 'sgml-close-tag)
  (define-key sgml-mode-map (control-key-vector ?c ?;) 'insert-blank-tag)
  )
