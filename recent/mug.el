;;; mug-mode
;;; a kind of repl, but executes a nearby expression template
;;; using the current location as parameters
;;;
;;; it only shares some basic features with fug-mode
;;;
;;; output is not generally echoed
;;; mapping commands to global keys makes no sense
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mug-header-pattern "^#~+\\s *")
(setq mug-header-pattern "^#~+\\s *")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-stringify (list) 
  (mapcar '(lambda (x) (sprint x t)) list)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-arg-reader-generic (start end)
  (let ((s (bs start end)))
    `(,s))
  )

(defun mug-arg-reader-readc (start end)
  (sx
   (goto-char start)
   (let ((o))
     (while (< (point) end)
       (setq o (cons (readc) o))
       )
     (mug-stringify (nreverse o))
     )
   )
  )

(defun mug-arg-reader-list (start end)
  (mug-stringify (read (format "(%s)" (bs start end))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-arg-reader-apply (fun) (funcall fun (point^) (point$)))

(defun mug-read-command-line ()
  (let ((command-line
	 (sx
	  (cond
	   ((looking-at mug-header-pattern))
	   ((rsb mug-header-pattern))
	   ((error "Can't find a command line"))
	   )
	  (bol)
	  (let* ((limit (point$)) s start)
	    (setq start (rsf mug-header-pattern))
	    (eval (read (format "`(%s)" (bs (point) (point$)))))
	    )
	  )
	 )
	)
    command-line)
  )

(defun mug-read-command ()
  (let* ((command-line (mug-read-command-line))
	 (body (car command-line))
	 (plist (cdr command-line))
	 (arg-spec (or (plist-get plist :args) '(&optional a b c d e)))
	 (fun `(lambda ,arg-spec ,body))
	 (arg-reader (or (plist-get plist :reader) 'mug-arg-reader-generic))
	 (start (region-beginning-if-active (point^)))
	 (end (region-end-if-active (point$)))
	 (args (funcall arg-reader start end))
	 )
    `(funcall ',fun ,@args)
    )
  )

(defun mug-exec-here (point &optional echo)
  (sx
   (goto-char point)
   (bol)
   (let* ((command-line (mug-read-command-line))
	  (command (mug-read-command))
	  (plist (cdr command-line))
	  (result (eval command))
	  )
     (cond
      ((or echo (plist-get plist :echo))
       (message result)
       )
      )
     )
   )
  )

(defun mug-exec ()
  (interactive)
   (cond
    ((mug-exec-here (point)))
    )
   )

(defun mug-exec-echo ()
  (interactive)
   (cond
    ((mug-exec-here (point) t))
    )
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-intern ()
  (interactive)
  (let* ((command-line (mug-read-command-line))
	 (plist (cdr command-line))
	 (key (plist-get plist :key))
	 )
;    (debug)
    )
  )

(defun mug-eval-buffer ()
  (interactive)
  (sx (bob)
      (while (rsf mug-header-pattern)
	(sx (bol) (mug-intern) (sit-for .5)))
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mug-install-directory "e:/borough/barnet")

(defun mug-visit-org-file ()
  (interactive)
  (find-file-other-window (filename-concat mug-install-directory "mug.org"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mug-mode-map (make-sparse-keymap))
(defvar mug-electric-keymap (make-sparse-keymap) "Key map to use when mug-electric-mode is active")

(defvar mug-prefix-map (make-sparse-keymap))
(def-key mug-prefix-map (kbd "x") 'mug-exec)

(define-key mug-mode-map (kbd "C-c C-e") 'mug-electric-mode)

(mapcar '(lambda (x)
	   (define-key mug-electric-keymap
		       (kbd (car x)) (cdr x))
	   (define-key mug-mode-map
		       (kbd (format "C-c C-%s" (car x))) (cdr x))
	   )
	`(
	  ("e" . mug-intern)
          ("c" . mug-exec)
          ("x" . mug-exec-echo)
	  ("h" . mug-visit-org-file)
	  )
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode mug-electric-mode
  "."
  :init-value nil
  :lighter " E "
  :keymap mug-electric-keymap
  (cond 
   (mug-electric-mode
    )
   (t
    )
   )
  )

(defun mug-mode ()
  (interactive)
  (use-local-map mug-mode-map)
  (setq major-mode 'mug-mode)
  (setq mode-name "mug")
  (setq mug-buffer-mru (current-buffer))
;  (add-hook 'kill-buffer-hook 'mug-mode-kill-hook t t)
  )

(setq auto-mode-alist (alist-put auto-mode-alist "\\.mug$" 'mug-mode))

(put 'mug-mode 'eval-buffer-modal 'mug-eval-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
