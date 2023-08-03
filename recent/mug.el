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
(defun alist-put (list tag val)
  "Like plist-put but acts on alists. Should really be built-in."
  (let ((cell (assoc tag list)))
    (cond
     (cell (setcdr cell val))
     ((setq list (cons (cons tag val) list)))
     )
    list)
  )

(defmacro sxp (&rest body)
  "Like save-excursion, but returns where point was at the end of theb ody execution."
  `(save-excursion ,@body (point)))

(defun point^ ()
  "The value of (point) at the start of the line."
 (sxp (beginning-of-line)))

(defun point$ ()
  "The value of (point) at the end of the line."
 (sxp (end-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bit-set-p (v mask &optional match)
  (= (logand v mask) (or match mask))
  )

(defun interactive-arg-read (spec)
  "Simulate the process of gathering args for an interactive function."
  (cond
   ((and (symbolp spec) (fboundp spec))
    (interactive-arg-read (cdr (interactive-form spec)))
    )
   ((stringp spec)
    (call-interactively `(lambda (&rest args) (interactive ,spec) args))
    )
   ((listp spec)
    (call-interactively `(lambda (&rest args) (interactive ,@spec) args))
    )
   )
  )

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
  (mapcar '(lambda (x) (prin1-to-string x t)) list)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-arg-reader-generic (start end)
  (let ((s (buffer-substring start end)))
    `(,s))
  )

(defun mug-arg-reader-readc (start end)
  (save-excursion
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
  (read (format "(%s)" (buffer-substring start end)))
  )

(defun mug-arg-reader-list (start end)
  (mug-stringify (read (format "(%s)" (buffer-substring start end))))
  )

(defun mug-arg-reader-sexp (start end)
  (let ((s (buffer-substring start end)))
;    (debug)
    (list (read s))
    )
  )

(defun mug-arg-reader-quote (start end)
  (let ((s (buffer-substring start end)))
    (list `(quote ,(read s)))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-on-tform-line ()
  (save-excursion
    (bol)
    (looking-at mug-header-pattern)
    )
  )

(defun mug-tform-read ()
  "Read a tform from the current buffer"
  (save-excursion
    (beginning-of-line)
    (re-search-forward mug-header-pattern nil t)
    (eval (read (format "`(%s)" (buffer-substring (point) (point$)))))
    )
  )

(defun mug-tmarker-define-key (tloc key)
  "Define a tmarker mapping."
  (define-key mug-tmarker-map key
    `(lambda (arg) (interactive "p")
       (let ((mug-active-command ,(set-marker (make-marker) tloc)))
	 (mug-exec arg)
	 )
       )
    )
  )

(defun mug-tform-exec ()
  (interactive)
  (debug)
  (let ((tform (mug-tform-read)))
    (let* ((plist (cdr tform))
	   (key (plist-get plist :key))
	   )
      (cond
       (key (mug-tmarker-define-key (point^) key))
       )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-arg-reader-apply (fun) (funcall fun (point^) (point$)))

(defun mug-locate-command-line ()
  (let ()
    (save-excursion
     (cond
      ((looking-at mug-header-pattern))
      ((re-search-backward mug-header-pattern))
      ((error "Can't find a command line"))
      )
     (point^)
     )
    )
  )

(defun mug-read-command-line (&optional tloc)
  (let ((command-line
	 (save-excursion
	  (cond
	   (tloc (goto-char tloc))
	   (mug-active-command (goto-char mug-active-command))
	   ((looking-at mug-header-pattern))
	   ((re-search-backward mug-header-pattern))
	   ((error "Can't find a command line"))
	   )
	  (beginning-of-line)
	  (let* ((limit (point$)) s start)
	    (setq start (re-search-forward mug-header-pattern nil t))
	    (eval (read (format "`(%s)" (buffer-substring (point) (point$)))))
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
	 (arg-spec (or (plist-get plist :args) '(&optional a b c d e f)))
	 (fun `(lambda ,arg-spec ,body))
	 (arg-reader (or (plist-get plist :reader) mug-arg-reader-default))
	 (start (region-beginning-if-active (point^)))
	 (end (cond
	       ((region-active-p) (region-end))
	       ((plist-get plist :end) (sxp (re-search-forward (plist-get plist :end) nil t)))
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
  "Apply the current tcommand to the arg line at LOC.
the current tcommand will be either mug-active-command if set, otherwise it will be
the tcommand above the location."
  (save-excursion
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
     (and echo (message (prin1-to-string result)))
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
  "Call mug-exec-here with default environmental context."
  (interactive "p")
   (cond
    ((mug-on-tform-line) (mug-tform-exec))
    ((mug-exec-here nil arg))
    )
   )

(defun mug-exec-echo (&optional arg)
  "Call mug-exec-here with default environmental context, but override any :echo parameters."
  (interactive "p")
   (cond
    ((mug-on-tform-line) (mug-tform-exec))
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
  "Returns the end of the tcommand. By default this the end of the line."
  (sxp (goto-char mug-active-command) (end-of-line))
  )

(defun mug-active-command-clear ()
  "Remove the active command marker, and revert to default behaviour."
  (cond
   (mug-active-command
    (put-text-property mug-active-command (mug-active-command-end) 'face nil)
    (setq mug-active-command nil)
    (setq mug-active-command-end nil)
    )
   )
  )

(defun mug-active-command-mark-set (&optional pos)
  "Move the active commmand to POS or clear it if POS is nil, or same as existing active command."
  (let ((old mug-active-command))
    (cond
     (mug-active-command
      (mug-active-command-clear)
      (cond
       ((and pos (not (= old pos)))
	(mug-active-command-mark-set pos))
       (t (message "active command cleared"))
       )
      )
     (t
      (setq mug-active-command
	(set-marker (make-marker) pos))
      (put-text-property mug-active-command (mug-active-command-end) 'face 'match)
      (message "active command set to %s" mug-active-command)
      )
     )
    )
  )

(defun mug-active-command-mark-here (&optional arg)
  "Set the active command to where the point is."
  (interactive "p")
  (mug-active-command-mark-set (point))
  )

(defun mug-active-command-jump ()
  "Jump to the location of the active command."
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
  "Return a list, ready for any, which is the locations of all the arglines in the buffer."
  (let ((avy-all-windows nil))
    (let (r)
      (save-excursion (beginning-of-buffer)
	  (while (re-search-forward "^\." nil t)
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
  "Use avy to pick an argline."
  (save-excursion (avy-process
	(mug-avy-arg-candidates)
	(avy--style-fn 'at-full))
    )
  )

(defun mug-avy-template-candidates ()
  "Return a list, ready for any, which is the locations of all the commandlines in the buffer."
  (let ((avy-all-windows nil))
    (let (r)
      (save-excursion (beginning-of-buffer)
	  (while (re-search-forward "^\." nil t)
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
  "Use avy to pick an tline."
  (save-excursion (avy-process
       (mug-avy-template-candidates)
       (avy--style-fn 'at-full))
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avy customizations :: 41a6226 Fri Feb 25 13:18:00 2022 +0000

(defun mug-avy-execute (&optional prefix)
  "Use avy to select a parameter line to execute in the active command context."
  (interactive "p")
  (let* ((aloc (mug-avy-arg-pick))
	 (tloc nil)
	 )
    (cond
     (aloc
      (save-excursion
       (goto-char aloc)
       (mug-exec-here tloc prefix)
       )
      )
     )
    )
  )

(defun mug-avy-template-execute (&optional prefix)
  "Use avy to select a command context to execute the parameter line at point."
  (interactive "p")
  (let ((tloc (mug-avy-template-pick)))
    (cond
     (tloc
      (mug-exec-here tloc prefix)
      )
     )
    )
  )

(defun mug-avy-avy (&optional prefix)
  "Use avy to select a command context to execute the parameter line selected also with avy."
  (interactive "p")
  (let ((aloc (mug-avy-arg-pick))
	(tloc (mug-avy-template-pick)))
    (cond
     (tloc
      (save-excursion (goto-char aloc) (mug-exec-here tloc prefix))
      )
     )
    )
  )

(defun mug-avy-template-activate (&optional prefix)
  "Use avy to select a command context to execute the parameter line at point."
  (interactive "p")
  (let ((tloc (mug-avy-template-pick)))
    (cond
     (tloc
      (save-excursion
       (mug-active-command-mark-set tloc)
       )
      )
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a tmarker key is one that is mapped to a executable form.
; these are held in mug-tmarker-map, where keys are mapped to exec forms
; when positioned on a parameter form, the dispatch key 's' is followed
; by the mapped key, which selects the appropriate exec-form
; 

(defun mug-tmarker-get-mark (b)
  "Get the command location for a tmark binding."
  (let* ((m b)
	 (mm (nth 1 (car (nth 1 (nth 3 m))))))
    mm)
  )

(defun mug-tmarker-view ()
  "Display all the defined tmarkers."
  (interactive)
  (let* ((map (sort-copy (cdr mug-tmarker-map) '<-car))
	 (s (mconcat (mapcar
		     '(lambda (b)
			(cond
			 ((listp (cdr b))
			  (let* ((mm (mug-tmarker-get-mark (cdr b)))
				 (command (mug-read-command-line mm))
				 )
			    (format "%c\t%s\n"
				    (car b) (car command)
				    ))
			  )
			 ))
		     map) "")))
    (show s)
    )
  )

(defun mug-define-tmarker (key)
  "Define a tmarker mapping."
  (interactive "Kkey: ")
  (let* ((current (lookup-key mug-tmarker-map key)))
    (cond ((and current (symbolp current))
	   (error "Don't try to remap that key %s -> %s" (key-description key) current))
	  )
    (define-key mug-tmarker-map key
     `(lambda (arg) (interactive "p")
        (let ((mug-active-command ,(set-marker (make-marker) (mug-locate-command-line))))
	  (mug-exec arg)
	  )
	)
     )
    )
  )

(defun mug-tmarker-dispatch-read-key ()
  "Read a mug-tmarker-dispatch key."
  (let* ((overriding-local-map mug-tmarker-map)
	 (keys (read-key-sequence "Key: "))
	 )
    keys
    )
  )

(defun mug-tmarker-dispatch (arg) (interactive "p")
  "Dispatch a tmarker key."
  (let* ((keys (mug-tmarker-dispatch-read-key))
	 (b (lookup-key mug-tmarker-map keys))
	 )
    (cond
     ((commandp b) (call-interactively b 1))
     ((error "%s is not a tmarker-dispatch key in this buffer" (key-description keys)))
     )
    )
  )

(defun mug-tmarker-jump ()
  "Jump to the location of a tmarker."
  (interactive)
  (let* ((keys (mug-tmarker-dispatch-read-key))
	 (b (lookup-key mug-tmarker-map keys))
	 (bb (and b (mug-tmarker-get-mark b)))
	 )
    (cond
     (bb (goto-marker bb))
     ((error (format "%s not mapped" k)))
     )
    )
  )

(defun mug-tmarker-active-mark ()
  "Set the active command to the tmark location."
  (interactive)
  (let* ((keys (mug-tmarker-dispatch-read-key))
	 (b (lookup-key mug-tmarker-map keys))
	 (bb (and b (mug-tmarker-get-mark b)))
	 )
    (cond
     (bb (mug-active-command-mark-set bb))
     ((error (format "%s not mapped" k)))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mug-define-key (key)
  "Assign a key to a execute a parameter line."
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

(defvar-local mug-tmarker-map nil "Key map to hold tmarker bindings")
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
	  ("a" . mug-active-command-mark-here)
	  ("j" . mug-active-command-jump)
          ("c" . mug-exec)
          ("x" . mug-exec-echo)
	  ("o" . mug-visit-org-file)
	  ("k" . mug-define-key)
	  ("q" . mug-electric-mode)
	  ("S" . mug-define-tmarker)
	  ("m" . mug-define-tmarker)
	  ("s" . mug-tmarker-dispatch)
	  )
	)

(define-key mug-tmarker-map (kbd "C-a") 'mug-tmarker-active-mark)
(define-key mug-tmarker-map (kbd "C-h") 'mug-tmarker-view)
(define-key mug-tmarker-map (kbd "C-j") 'mug-tmarker-jump)

(setq mug-mode-map-base mug-mode-map)
(setq mug-electric-keymap-base mug-electric-keymap)
(setq mug-tmarker-map-base mug-tmarker-map)

(defun mug-define-local-keys ()
  "Internal function to clone base keymaps for local maps."
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
  "Major mode for editing and using mug buffers."
  (interactive)

  (setq major-mode 'mug-mode)
  (setq mode-name "mug")
  (setq mug-buffer-mru (current-buffer))

  (mug-define-local-keys)

  (use-local-map mug-mode-map)

  )

(setq auto-mode-alist (alist-put auto-mode-alist "\\.mug$" 'mug-mode))

