;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gdb-send-command (cmd)
  (comint-send-string gud-comint-buffer (format "%s\n" cmd))
  )

(defun gud-process () (get-buffer-process gud-comint-buffer))

(defun gud-get-filter () (process-filter (gud-process)))

(defun gud-set-filter (filter)
  (let* ((proc (gud-process))
	 (prev (process-filter proc))
	 )
    (set-process-filter proc filter)
    prev
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fug-buffer-name (name)
  (format "*fug-%s*" name)
  )

(defun fug-set-mru-buffer ()
  (set-buffer fug-buffer-mru)
  )

(defun fug-pop-to-mru-buffer ()
  (interactive)
  (pop-to-buffer fug-buffer-mru)
  )

(defun fug-buffer-create (name)
  (interactive "SName: ")
  (setq fug-buffer-mru (get-buffer-create (fug-buffer-name name)))
  (fug-pop-to-mru-buffer)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar fug-header-pattern "^#~+\\s *")
(setq fug-header-pattern "^#~+\\s *")

(defun fug-goto-start ()
  (interactive)
  (cond
   ((sx (bol) (looking-at fug-header-pattern)) (bol))
   ((sx (rsb fug-header-pattern)) (goto-char (mb 0)))
   )
  )

(defun fug-goto-output-start ()
  (interactive)
  (fug-goto-start)
  (fl 1)
  )

(defun fug-region-end ()
  (sx
    (cond
     ((rsf fug-header-pattern nil nil 0 t))
     ((point-max))
     )
    )
  )

(defun fug-goto-next ()
  (interactive)
  (fug-goto-output-start)
  (rsf fug-header-pattern nil nil 0 t)
  )

(defun fug-goto-prev ()
  (interactive)
  (fug-goto-start)
  (rsb fug-header-pattern)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fug-outout-kill ()
  (interactive)
  (sx (fug-goto-output-start)
      (delete-region (point) (fug-region-end))
      )
  )

(defun fug-insert (s)
  (sx
   (fug-set-mru-buffer)
   (fug-goto-output-start)
   (delete-region (point) (fug-region-end))
   (setq s (gud-marker-filter s))
   (insert "\n" s "\n")
   )
  )

(defun fug-read-command ()
  (sx
   (cond
    ((looking-at fug-header-pattern))
    ((rsb fug-header-pattern))
    ((error "Can't find a command line"))
    )
   (bol)
   (let ((limit (point$)) s start)
     (setq start (rsf fug-header-pattern))
     (setq s
       (cond
	((rsf "#\\s *\\(.*?\\)#" limit)
	 (apply 'list (ms 1) (read-as-list (bs (me 0) (point$))))
	 )
	((rsf "#\\s *\\(.*\\)$" limit)
	 (list (ms 1))
	 )
	((or (looking-at "(") (looking-at "\""))
	 (apply 'list (eval (readc)) (read (format "(%s)" (bs (point) limit))))
	 )
	; "<expr>" followed by meta parameters
	((looking-at "\"")
	 (read (format "(%s)" (bs start limit)))
	 )
	((list (bs start limit)))
	))
     )
   )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; global key mapping

(defvar fug-global-map (make-sparse-keymap))

(defvar fug-global-command-map nil)
; (setq fug-global-command-map nil)

(global-set-key (kbd "s-#") fug-global-map)
(global-set-key (kbd "C-z C-n") fug-global-map)

;(describe-variable 'fug-global-command-map)

(define-key fug-global-map (kbd "#") 'fug-repeat-last)

(defun fug-partial-command-key (keys)
  (let* ((len (length keys)))
    (cond
     ((and (vectorp keys)
	   (eq (lookup-key global-map (vector (aref keys 0)))
	       fug-global-map))
      (apply 'chars-to-string (nthcdr 1 (append keys nil)))
      )
     ((eq (lookup-key global-map (substring keys 0 1)) fug-global-map)
      (substring keys 1))
     ((eq (lookup-key global-map (substring keys 0 2)) fug-global-map)
      (substring keys 2))
     )
    )
  )

(defun fug-global-exec ()
  (interactive)
  (let* ((keys (this-command-keys))
	 (key (fug-partial-command-key keys))
	 (command (glist-get fug-global-command-map key))
	 )
    (fug-set-mru-buffer)
    (goto-char (plist-get (cdr command) :location))
    (fug-exec)
    (setq fug-last-command command)
    )
  )

(defun fug-repeat-last ()
  (interactive)
  (fug-set-mru-buffer)
  (goto-char (plist-get (cdr fug-last-command) :location))
  (fug-exec)
  )

(defun fug-global-bind (key command)
  (define-key fug-global-map key 'fug-global-exec)
  (glist-put-sym fug-global-command-map key command)
  (message "key \"%s\" assigned to \"%s\"" (key-description key) (car command))
  )

(defun fug-intern ()
  (interactive)
  (let* ((mark (point-marker))
	 (command (fug-read-command))
	 (plist (cdr command))
	 (key (plist-get plist :key))
	 )
    (setq plist (plist-put plist :location mark))
    (cond
     (key
      (cond
       ((symbolp key) (setq key (symbol-name key)))
       ((setq key (kbd key)))
       )
      (fug-global-bind key command))
     )
    )
  )

(setq fug-install-directory "e:/borough/barnet")

(defun fug-visit-org-file ()
  (interactive)
  (find-file-other-window (filename-concat fug-install-directory "fug.org"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fug-eval-buffer ()
  (interactive)
  (sx (bob)
      (while (rsf fug-header-pattern) (sx (bol) (fug-intern) (sit-for .5)))
      )
  )

(defun fug-exec-here (point)
  (sx
   (goto-char point)
   (bol)
   (let* ((command (fug-read-command))
	  (expr (car command))
	  )
     (fug-send expr 'fug-insert)
     )
   )
  )

(defun fug-exec ()
  (interactive)
   (cond
   ((sx (bol) (looking-at fug-header-pattern)) (fug-exec-here (point)))
   ((sx (rsb fug-header-pattern)) (fug-exec-here (mb 0)))
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar fug-buffer-mru nil "Most recent fug buffer")

(defvar fug-mode-map (make-sparse-keymap))
(defvar fug-electric-keymap (make-sparse-keymap) "Key map to use when fug-electric-mode is active")

(defvar fug-prefix-map (make-sparse-keymap))
(def-key fug-prefix-map (kbd "f") 'fug-buffer-create)
(def-key fug-prefix-map (kbd "x") 'fug-exec)
(def-key fug-prefix-map (kbd "C-r") 'fug-pop-to-mru-buffer)
(def-key fug-prefix-map (kbd "s-s") 'fug-state-sane)

(define-key global-map (kbd "s-j") fug-prefix-map)

(define-key fug-mode-map (kbd "C-c C-e") 'fug-electric-mode)
(define-key fug-mode-map (kbd "C-c C-q") 'fug-state-sane)

(mapcar '(lambda (x)
	   (define-key fug-electric-keymap
		       (kbd (car x)) (cdr x))
	   (define-key fug-mode-map
		       (kbd (format "C-c C-%s" (car x))) (cdr x))
	   )
	`(
          ("x" . fug-exec)
          ("k" . fug-outout-kill)
          ("p" . fug-goto-prev)
          ("n" . fug-goto-next)
	  ("c" . fug-intern)
	  ("h" . fug-visit-org-file)
	  )
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sx-gud-buffer (&rest forms)
  `(sx (set-buffer gud-comint-buffer) ,@forms)
  )

(define-minor-mode fug-electric-mode
  "."
  :init-value nil
  :lighter " E "
  :keymap fug-electric-keymap
  (cond 
   (fug-electric-mode
    )
   (t
    )
   )
  )

(defun fug-mode ()
  (interactive)
  (use-local-map fug-mode-map)
  (setq major-mode 'fug-mode)
  (setq mode-name "fug")
  (setq gud-marker-filter 
    (sx-gud-buffer gud-marker-filter)
    )
  (setq fug-buffer-mru (current-buffer))
;  (add-hook 'kill-buffer-hook 'fug-mode-kill-hook t t)
  )

(setq auto-mode-alist (alist-put auto-mode-alist "\\.fug$" 'fug-mode))
(put 'fug-mode 'eval-buffer-modal 'fug-eval-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; process handling

(defvar fug-state nil)

(defun fug-get-state (key) (glist-get fug-state key))
(defun fug-set-state (key val) (glist-put-sym fug-state key val))

(defun fug-state-sane ()
  (interactive)
  (fug-set-state 'state 'done)
  (set-process-filter (gud-process) 'gud-filter)
  )

(defun fug-set-response (text start end)
  (fug-set-state 'response (substring text start end))
  )

(defun fug-accept-response (text start end)
  (fug-set-response text start end)

  (set-process-filter proc (fug-get-state 'filter-save))

  (funcall (fug-get-state 'callback)
	   (fug-get-state 'response))

  (fug-set-state 'state 'done)
  )

(defun fug-filter (proc text)
  (setq fug-filter-text (concat fug-filter-text text))
  (cond
   ((string-match "^^done" fug-filter-text)
    (fug-accept-response fug-filter-text 0 (mb 0))
    )
   ((string-match "^(gud)" fug-filter-text)
    (fug-accept-response fug-filter-text 0 (mb 0))
    )
   ((string-match "^ *DB<" fug-filter-text)
    (fug-accept-response fug-filter-text 0 (mb 0))
    )
   )
  )

(defun fug-send (cmd callback)
  (let* ((proc (gud-process))
	 )
    (fug-set-state 'state 'send)
    (fug-set-state 'filter-save (process-filter proc))
    (fug-set-state 'response "")
    (setq fug-filter-text "")
    (fug-set-state 'callback callback)
    (set-process-filter proc 'fug-filter)
    (gdb-send-command cmd)
    )
  )
