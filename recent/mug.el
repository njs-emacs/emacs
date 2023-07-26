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

(defun mug-arg-region (start end) (list start end))

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
	 (end (cond
	       ((region-active-p) (region-end))
	       ((plist-get plist :end) (sxp (rsf (plist-get plist :end))))
	       (t (point$))
	       ))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-tmarker-view ()
  (interactive)
;  (debug)
  (let ((s (mconcat (mapcar
		     '(lambda (s) (format "%s\n" s))
		     (cdr mug-tmarker-map)) "\n")))
    (show s)
    )
  )

(defun mug-tmarker-get-mark (b)
  (let* ((m b)
	 (mm (nth 1 (car (nth 1 (nth 3 m))))))
    mm)
  )

(defun mug-marker-get-text (m)

  )


(defun mug-tmarker-view ()
  (interactive)
;  (debug)
  (let ((s (mconcat (mapcar
		     '(lambda (b)
			(cond
			 ((listp (cdr b))
			  (let* ((mm (mug-tmarker-get-mark (cdr b)))
				 (command (mug-read-command-line mm))
				 )
			    (format "-%c %s \n   %s\n%s\n"
				    (car b) mm b (car command)
				    ))
			  )
			 ))
		     (cdr mug-tmarker-map)) "\n")))
    (show s)
    )
  )

(defun mug-tmarker-jump (key)
  (interactive "Kkey: ")
  (let* ((k (key-description key))
	 (b (lookup-key mug-tmarker-map k))
	 (bb (and b (mug-tmarker-get-mark b)))
	 )
    (cond
     (bb (goto-marker bb))
     ((error (format "%s not mapped" k)))
     )
    )
  )

(defun mug-define-tmarker (key)
  (interactive "Kkey: ")
  (define-key mug-tmarker-map key
    `(lambda (arg) (interactive "p")
       (let ((mug-active-command ,(set-marker (make-marker) (mug-locate-command-line))))
	 (mug-exec arg)
	 )
       )
    )
  )

(defun mug-tmarker-dispatch (arg) (interactive "p")
;  (debug)
  (let* ((overriding-local-map mug-tmarker-map)
	 (keys (read-key-sequence nil t))

;	 (k (key-description key))
	 (b (lookup-key mug-tmarker-map keys))
;	 (x (lookup-key mug-tmarker-map k))
	 )
    (call-interactively b 1)
    )
  )
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defvar mug-mode-map nil "")
(defvar mug-electric-keymap nil "Key map to use when mug-electric-mode is active")

(make-variable-buffer-local 'mug-tmarker-map)
(make-variable-buffer-local 'mug-electric-keymap)

(defvar-local mug-tmarker-map nil "Key map to use when mug-electric-mode is active")
(setq mug-tmarker-map nil)

(defun mug-electric-define-key (key binding)
  (define-key mug-electric-keymap
	      (kbd key) binding)
  (define-key mug-mode-map
	      (kbd (format "C-c C-%s" key)) binding)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mug-mode-map (make-sparse-keymap))
(setq mug-electric-keymap (make-sparse-keymap))
(setq mug-tmarker-map (make-sparse-keymap))

(define-key mug-mode-map (kbd "C-c C-e") 'mug-electric-mode)
(define-key mug-mode-map (kbd "C-c C-t") 'mug-avy-template-activate)
(define-key mug-mode-map (kbd "C-c C-v") 'mug-avy-execute)

(define-key mug-mode-map (kbd "C-c C-y") 'mug-avy-template-execute)
(define-key mug-mode-map (kbd "C-c C-u") 'mug-avy-avy)

(mapcar '(lambda (x)
	   (mug-electric-define-key (car x) (cdr x))
	   )
	`(
	  ("a" . mug-active-command-mark)
	  ("j" . mug-active-command-jump)
          ("c" . mug-exec)
          ("x" . mug-exec-echo)
	  ("o" . mug-visit-org-file)
	  ("k" . mug-define-key)
	  ("q" . mug-electric-mode)
	  ("S" . mug-define-tmarker)
	  ("s" . mug-tmarker-dispatch)
	  )
	)

(define-key mug-tmarker-map (kbd "C-h") 'mug-tmarker-view)
(define-key mug-tmarker-map (kbd "C-j") 'mug-tmarker-jump)

(setq mug-mode-map-base mug-mode-map)
(setq mug-electric-keymap-base mug-electric-keymap)
(setq mug-tmarker-map-base mug-tmarker-map)

(defun mug-define-local-keys ()
 (setq mug-mode-map (copy-keymap mug-mode-map-base))
 (setq mug-electric-keymap (copy-keymap mug-electric-keymap-base))
 (setq mug-tmarker-map (copy-keymap mug-tmarker-map-base))
 (mug-electric-define-key "s" mug-tmarker-map)
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

  (setq major-mode 'mug-mode)
  (setq mode-name "mug")
  (setq mug-buffer-mru (current-buffer))

  (mug-define-local-keys)

  (use-local-map mug-mode-map)

;  (add-hook 'kill-buffer-hook 'mug-mode-kill-hook t t)
  )

(setq auto-mode-alist (alist-put auto-mode-alist "\\.mug$" 'mug-mode))

(put 'mug-mode 'eval-buffer-modal 'mug-eval-buffer)

;;(yas--define-parents 'mug-mode '(emacs-lisp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mug-install-directory "e:/borough/barnet")

(defun mug-visit-org-file ()
  (interactive)
  (find-file-other-window (filename-concat mug-install-directory "mug.org"))
  )

