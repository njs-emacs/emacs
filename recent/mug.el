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
(defun interactive-arg-read (spec)
  (call-interactively `(lambda (&rest args) (interactive ,spec) args))
  )

; (interactive-arg-read "SThis: \nSThat: ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mug-header-pattern "^#~+\\s *")
(setq mug-header-pattern "^#~+\\s *")

(defvar-local mug-always-show nil "Always act if mug command has :show t or show prefix arg was given")
(defvar-local mug-always-echo nil "Always act if mug command has :echo t or echo prefix arg was given")
(defvar-local mug-always-insert nil "Always act if mug command has insert prefix arg given")

(set-default 'mug-always-show nil)
(set-default 'mug-always-echo nil)
(set-default 'mug-always-insert nil)

(defvar-local mug-arg-reader-default 'mug-arg-reader-generic "Default arg reader for mug commands in file")

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

(defun mug-arg-reader-sexp (start end)
  (let ((s (bs start end)))
;    (debug)
    (list (read s))
    )
  )

(defun mug-arg-reader-quote (start end)
  (let ((s (bs start end)))
    (list `(quote ,(read s)))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-arg-reader-apply (fun) (funcall fun (point^) (point$)))

(defun mug-locate-command-line ()
  (let ()
    (sx
     (cond
      ((looking-at mug-header-pattern))
      ((rsb mug-header-pattern))
      ((error "Can't find a command line"))
      )
     (point^)
     )
    )
  )

(defun mug-read-command-line ()
  (let ((command-line
	 (sx
	  (cond
	   (mug-active-command (goto-char mug-active-command))
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
	 (arg-reader (or (plist-get plist :reader) mug-arg-reader-default))
	 (start (region-beginning-if-active (point^)))
	 (end (region-end-if-active (point$)))
	 (args (funcall arg-reader start end))
	 (extra (plist-get plist :extra))
	 (extra-args (cond (extra (interactive-arg-read extra))))
	 (args (nconc args extra-args))
	 )
    `(funcall ',fun ,@args)
    )
  )

(defun mug-exec-here (&optional arg echo)
  (sx
   (let* ((command-line (mug-read-command-line))
	  (command (mug-read-command))
	  (plist (cdr command-line))
	  (cd (plist-get plist :cd))
	  result
	  )
     (cond ((plist-get plist :debug) (debug)))
     (setq result (save-cd cd (eval command)))

     (cond
      ((or (eq arg 16) mug-always-insert (plist-get plist :insert))
       (goto-char (region-end-if-active (point$)))
       (insert "\n" result)
       )
      ((or (eq arg 4) mug-always-show (plist-get plist :show))
       (show result)
       )
      ((or (eq arg 0) mug-always-echo echo (plist-get plist :echo))
       (message (sprint result))
       )
      )
     result
     )
   )
  )

(defun mug-exec (&optional arg)
  (interactive "p")
   (cond
    ((mug-exec-here arg))
    )
   )

(defun mug-exec-echo (&optional arg)
  (interactive "p")
   (cond
    ((mug-exec-here arg t))
    )
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; these functions are for compatibily. They don't really do anything
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
; maybe use avy instead

(defun mug-define-key (key)
  (interactive "Kkey: ")
  (define-key mug-mode-map (concat (kbd "C-v") key)
    `(lambda (arg) (interactive "p")
       (goto-marker ,(set-marker (make-marker) (point^)))
       (mug-exec arg)
       )
    )
  )
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mug-mode-map (make-sparse-keymap))
;(setq mug-mode-map (make-sparse-keymap))
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
	  ("a" . mug-active-command-mark)
	  ("j" . mug-active-command-jump)
	  ("e" . mug-intern)
          ("c" . mug-exec)
          ("x" . mug-exec-echo)
	  ("o" . mug-visit-org-file)
	  ("k" . mug-define-key)
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
  (emacs-lisp-mode)
  (use-local-map mug-mode-map)
  (setq major-mode 'mug-mode)
  (setq mode-name "mug")
  (setq mug-buffer-mru (current-buffer))
;  (add-hook 'kill-buffer-hook 'mug-mode-kill-hook t t)
  )

(setq auto-mode-alist (alist-put auto-mode-alist "\\.mug$" 'mug-mode))

(put 'mug-mode 'eval-buffer-modal 'mug-eval-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local mug-active-command nil
  "mug-active-command overrides the usual nearest command. When active it will be hilighted")

(defvar-local mug-active-command-end nil
  "Used to maintain highlight on active template")

(make-variable-buffer-local 'mug-active-command)
(make-variable-buffer-local 'mug-active-command-end)

(defun mug-active-command-end ()
  (sxp (goto-char mug-active-command) (eol))
  )

(defun mug-active-command-mark ()
  (interactive)
  (cond
   (mug-active-command
    (message "active command cleared")
    (put-text-property mug-active-command (mug-active-command-end) 'face nil)
    (setq mug-active-command nil)
    (setq mug-active-command-end nil)
    )
   (t
    (setq mug-active-command
      (set-marker (make-marker) (mug-locate-command-line)))
    (put-text-property mug-active-command (mug-active-command-end) 'face 'info-index-match)
    (message "active command set to %s" mug-active-command)
    )
   )
  )

(defun mug-active-command-jump ()
  (interactive)
  (cond
   (mug-active-command
    (goto-char mug-active-command)
    )
   (t
    (error "mug-active-command is not active")
    )
   )
  )

