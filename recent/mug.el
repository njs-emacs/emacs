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
(defun bit-set-p (v mask &optional match)
  (= (logand v mask) (or match mask))
  )

(defun interactive-arg-read (spec)
  (call-interactively `(lambda (&rest args) (interactive ,spec) args))
  )

; (interactive-arg-read "SThis: \nSThat: ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mug-header-pattern "^#~+\\s *")
(setq mug-header-pattern "^#~+\\s *")

(defvar-local mug-always-show nil "Always act if mug command has :show t or show prefix arg was given")
(defvar-local mug-always-echo nil "Always act if mug command has :echo t or echo prefix arg was given")
(defvar-local mug-always-kill nil "Always act if mug command has :kill t or kill prefix arg was given")
(defvar-local mug-always-insert nil "Always act if mug command has insert prefix arg given")

(set-default 'mug-always-show nil)
(set-default 'mug-always-echo nil)
(set-default 'mug-always-kill nil)
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

(defun mug-arg-reader-list-ns (start end)
  (read (format "(%s)" (bs start end)))
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

(defun mug-read-command-line (&optional tloc)
  (let ((command-line
	 (sx
	  (cond
	   (tloc (goto-char tloc))
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

(defun mug-read-command (&optional tloc)
  (let* ((command-line (mug-read-command-line tloc))
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

(defun mug-exec-here (&optional tloc arg echo)
  (sx
   (let* ((command-line (mug-read-command-line tloc))
	  (command (mug-read-command tloc))
	  (plist (cdr command-line))
	  (cd (plist-get plist :cd))
	  (echo (or mug-always-echo (plist-get plist :echo) (not (bit-set-p arg 1))))
	  (kill (or mug-always-kill (plist-get plist :kill) (bit-set-p arg 2)))
	  (show (or mug-always-show (plist-get plist :show) (bit-set-p arg 4)))
	  (insert (or mug-always-insert (plist-get plist :insert) (bit-set-p arg 16)))
	  result
	  )
     (cond ((plist-get plist :debug) (debug)))
     (setq result (save-cd cd (eval command)))

     (and kill (kill-new result))
     (and show (show result))
     (and echo (message (sprint result)))
     (cond
      (insert 
       (goto-char (region-end-if-active (point$)))
       (insert "\n" result)
       )
      )
     result
     )
   )
  )

(defun mug-exec (&optional arg)
  (interactive "p")
   (cond
    ((mug-exec-here nil arg))
    )
   )

(defun mug-exec-echo (&optional arg)
  (interactive "p")
   (cond
    ((mug-exec-here nil arg t))
    )
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mug-install-directory "e:/borough/barnet")

(defun mug-visit-org-file ()
  (interactive)
  (find-file-other-window (filename-concat mug-install-directory "mug.org"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local mug-active-command nil
  "mug-active-command overrides the usual nearest command. When active it will be hilighted")

(defvar-local mug-active-command-end nil
  "Used to maintain highlight on active template")

(make-variable-buffer-local 'mug-active-command)
(make-variable-buffer-local 'mug-active-command-end)

(set-default 'mug-active-command nil)

(defun mug-active-command-end ()
  (sxp (goto-char mug-active-command) (eol))
  )

(defun mug-active-command-clear ()
  (cond
   (mug-active-command
    (put-text-property mug-active-command (mug-active-command-end) 'face nil)
    (setq mug-active-command nil)
    (setq mug-active-command-end nil)
    )
   )
  )

(defun mug-active-command-mark (&optional force)
  (interactive)
  (cond
   (mug-active-command
    (mug-active-command-clear)
    (cond 
     (force
      (mug-active-command-mark))
     (t (message "active command cleared"))
     )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-avy-arg-candidates ()
  (let ((avy-all-windows nil))
    (let (r)
      (sx (bob)
	  (while (rsf "^\.")
	    (cond
	     ((looking-at-at "#~" (point^)))
	     (t (setq r (cons (point^) r)))
	     )
	    )
	  )
      (nreverse r)
      )
    )
  )

(defun mug-avy-arg-pick ()
  (sxp (avy-process
	(mug-avy-arg-candidates)
	(avy--style-fn 'at-full))
    )
  )

(defun mug-avy-template-candidates ()
  (let ((avy-all-windows nil))
    (let (r)
      (sx (bob)
	  (while (rsf "^\.")
	    (cond
	     ((looking-at-at "#~" (point^)) (setq r (cons (point^) r)))
	     )
	    )
	  )
      (nreverse r)
      )
    )
  )

(defun mug-avy-template-pick ()
  (sxp (avy-process
	(mug-avy-template-candidates)
	(avy--style-fn 'at-full))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avy customizations :: 41a6226 Fri Feb 25 13:18:00 2022 +0000

(defun mug-avy-template-activate (&optional prefix)
  (interactive "p")
  (let ((tloc (mug-avy-template-pick)))
    (cond
     (tloc
      (sx
       (goto-char tloc)
       (mug-active-command-mark t)
       )
      )
     )
    )
  )

(defun mug-avy-execute (&optional prefix)
  (interactive "p")
  (let* ((aloc (mug-avy-arg-pick))
	 (tloc nil)
	 )
    (cond
     (aloc
      (sx
       (goto-char aloc)
       (mug-exec-here tloc prefix)
       )
      )
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-avy-template-execute (&optional prefix)
  (interactive "p")
  (let ((tloc (mug-avy-template-pick)))
    (cond
     (tloc
      (mug-exec-here tloc prefix)
      )
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-avy-avy (&optional prefix)
  (interactive "p")
  (let ((aloc (mug-avy-arg-pick))
	(tloc (mug-avy-template-pick)))
    (cond
     (tloc
      (sx (goto-char aloc) (mug-exec-here tloc prefix))
      )
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defvar mug-electric-keymap (make-sparse-keymap) "Key map to use when mug-electric-mode is active")

(defvar mug-prefix-map (make-sparse-keymap))
(def-key mug-prefix-map (kbd "x") 'mug-exec)

(define-key mug-mode-map (kbd "C-c C-e") 'mug-electric-mode)
(define-key mug-mode-map (kbd "C-c C-t") 'mug-avy-template-activate)
(define-key mug-mode-map (kbd "C-c C-v") 'mug-avy-execute)

(define-key mug-mode-map (kbd "C-c C-y") 'mug-avy-template-execute)
(define-key mug-mode-map (kbd "C-c C-u") 'mug-avy-avy)


(mapcar '(lambda (x)
	   (define-key mug-electric-keymap
		       (kbd (car x)) (cdr x))
	   (define-key mug-mode-map
		       (kbd (format "C-c C-%s" (car x))) (cdr x))
	   )
	`(
	  ("a" . mug-active-command-mark)
	  ("j" . mug-active-command-jump)
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

;;(yas--define-parents 'mug-mode '(emacs-lisp-mode))
