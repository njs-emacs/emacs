(require 'comint)

(load-standard "shell")

(defun qshell (cmd) (call-process "sh" nil nil nil "-c" cmd))

;(qshell "if [ -f tttetet ] ; then exit 0 ; else exit 3 ; fi")

(defun goto-shell (&optional arg) (interactive "P")
  (let* ((name (format "shell-%s" (or arg 0)))
	 (buf (get-buffer name)))
    (cond (buf (switch-to-buffer buf))
	  (t
	   (shell)
	   (rename-buffer name)
	   )))
  )

;; shell logs are forced into history when they are first created

(defun kill-shell-buffer-yes (&optional save-session save-input)
  (let ((ts (format-time-string "%y%m%d-%H%M%S" shell-start-timestamp))
	(host (downcase system-name))
	(date (home-daily-today))
	)
    (cond
     ((or save-session (y-or-n-p "Save session log? "))
      (let (
	    (name (format "%s/sh-%s-%s.log" date host ts))
	    (nvc-enable-force t)
	    )
	(bob)
	(insert
	 (format "#~type:{shell.log}\n##host:%s\n##nobackup##\n" host))
	(write-file name)
	)
      )
     )
    (cond
     ((or save-input (y-or-n-p "Save input-ring? "))
      (let ((cmds (nthcdr 2 comint-input-ring))
	    (nvc-enable-force t)
	    (name
	     (format "%s/sh-%s-%s.input" date host ts)
	     ))
	(with-file name
	  (insert (format "#~type:{shell.input-history}\n##host:%s\n##nobackup##\n" host))
	  (mapcar '(lambda (x) (and x (insert x "\n"))) cmds)
	  (save-buffer)
	  (kill-buffer (current-buffer))
	  )
	)
      )
     ))
  )

(defun kill-shell-buffer-maybe ()
  (cond ((y-or-n-p "Kill shell buffer? ")
	 (kill-shell-buffer-yes)
	 t)
	((error "cancelled"))
	)
  )

(defun shell-kill-force () (interactive)
  (let ((buf (current-buffer)))
    (remove-hook 'kill-current-buffer-hook 'bury-buffer-instead t)
    (remove-hook 'kill-buffer-hook 'kill-shell-buffer-maybe t)
    (kill-shell-buffer-yes)
    (kill-buffer buf)
    )
  )
  
(fset 'kill-buffer*
  `(lambda (&optional buffer)
       (let ((default-directory))
	 (funcall ,(symbol-function 'kill-buffer) buffer)))
     )

(defun get-shell-buffer ()
  (let* ((buf (get-buffer-create "**shell**")))
    (set-buffer buf)
    (erase-buffer)
    buf
    ))
  
(defun shell-buffer (cmd &optional input keep)
  (let* ((buf (get-shell-buffer)))
    (sx
     (and input (insert input))
     (call-process-region 1 (point-max)
			  shell-file-name
			  (not keep) t t
			  shell-command-switch
			  cmd)
     buf
     )
    ))

(defun call-shell (cmd &optional input read keep)
  "return output from shell command"
  (sx
   (let* ((buf (shell-buffer cmd input keep)))
     (set-buffer buf)
     (bob)
     (prog1 (funcall (or read 'buffer-string))
       (set-buffer-modified-p nil)
       (copy-region-as-kill (point-min) (point-max))
       (kill-buffer* buf))
     )
   ))

(defun start-shell (cmd &optional filter sentinel)
  (let* ((buf (get-shell-buffer))
	 (process (start-process "sh" buf
				 shell-file-name
				 "-c"
				 cmd))
	 )
    (set-process-filter process filter)
    (set-process-sentinel process sentinel)
    process
    ))

(defun bury-buffer-instead () (bury-buffer) t)

(defun shell-mode-hook-ns ()
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'kill-shell-buffer-maybe t)
  (add-hook 'kill-current-buffer-hook 'bury-buffer-instead t t)
  (setq shell-process (get-buffer-process "*shell*"))
  (setq kill-buffer-query-functions nil)
  (make-local-variable 'shell-start-timestamp)
  (setq shell-start-timestamp (current-time))
  )

(add-hook 'shell-mode-hook 'shell-mode-hook-ns)

(defun cmd-shell (arg) (interactive "P")
  (let ((explicit-shell-file-name "cmd")
;	(explicit-cmd-args '("/Q"))
	)
    (goto-shell arg)
    )
  )

(load-overrides "shell")
(set-default 'comint-input-ring-size 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq shell-prompt-regexp "^\\([0-9]\\{6\\}\\) \\([0-9]+\\) \\(.*\\) $ ")

(defun shell-prompt-rsf () (interactive)
  (cond
   ((rsf shell-prompt-regexp)
    (gme 0))
   )
  )


(defun shell-prompt-rsb () (interactive)
  (cond
   ((sx (fl 0) (rsb shell-prompt-regexp))
    (gme 0))
   )
  )

(defun overlay-filter (overlays property value)
  (delete nil (mapcar '(lambda (o)
			 (cond ((equal (overlay-get o property) value) o))) overlays))
  )

(defun looking-at-at (pat point)
  (sx (goto-char point) (looking-at pat)))

(defun bol-looking-at (pat)
  (sx (fl 0) (looking-at pat)))

(defun shell-output-get-region ()
  (sx 
   (cond
    ((bol-looking-at shell-prompt-regexp)
     (fl 0)
     (let* ((end (point))
	    (start
	     (sxp (cond
		   ((rsb shell-prompt-regexp) (bol) (fl 1))
		   ((error "no previous prompt"))
		   )
	       )
	     )
	    )
       (list start end)
       )
     )
    )
   )
  )

(defun shell-output-hide-toggle () (interactive)
  (sx 
   (cond
   ((bol-looking-at shell-prompt-regexp)
    (fl 0)
    (let* ((end (point))
	   (start
	    (sxp (cond
		  ((rsb shell-prompt-regexp) (bol) (fl 1))
		  ((error "no previous prompt"))
		  )
	      )
	    )
	   )
      (cond ((get-char-property start 'invisible)
	     (let ((overlays
		    (overlay-filter (overlays-at start) 'shell-hide t)))
	       (mapcar 'delete-overlay overlays))
	     (remove-text-properties start end '(invisible))
	     )
	    (t 
	     (setq overlay (make-overlay start end))
	     (overlay-put overlay 'shell-hide t)
	     (overlay-put overlay 'before-string "... hidden ...\n")
	     (add-text-properties start end '(invisible t))
	     )
	    )
      
      )
    )
   )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq shell-annotation-map (make-sparse-keymap))
(setq shell-annotation-history nil)

(defun shell-annotation-read ()
  (list (read-from-minibuffer
	 "Stuff: " ""
	 nil
	 nil '(shell-annotation-history . 1)))
  )

(defun shell-annotate-insert (format stuff)
  (sx 
   (cond
   ((bol-looking-at shell-prompt-regexp)
    (fl 0)
    (insert (format format stuff))
    )
   )
   )
  )

(defun shell-annotate-pre (stuff)
  (interactive (shell-annotation-read))
  (shell-annotate-insert "==> %s ==\n" stuff)
  )

(defun shell-annotate-post (stuff)
  (interactive (shell-annotation-read))
  (shell-annotate-insert "==< %s ==\n" stuff)
  )

(defun shell-annotate (where)
  (interactive "P")
  (call-interactively (cond
		       (where 'shell-annotate-pre)
		       (t 'shell-annotate-post)
		       )
		      )
  )

(defun shell-output-purge ()
  (interactive)
  (let ((r (shell-output-get-region)))
    (apply 'kill-region r)
    (sx (goto-char (car r)) (insert (format "== killed ==\n")))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq shlog-mode-map (make-sparse-keymap))

(define-key shlog-mode-map (control-key-vector ?# ?k) 'shell-output-purge)
(define-key shlog-mode-map (control-key-vector ?# ?') 'shell-annotate)
(define-key shlog-mode-map (control-key-vector ?# ?/) 'shell-output-hide-toggle)
(define-key shlog-mode-map (control-key-vector ?# ?s) 'shell-prompt-rsf)
(define-key shlog-mode-map (control-key-vector ?# ?r) 'shell-prompt-rsb)
(define-key shlog-mode-map [C-down] 'shell-prompt-rsf)
(define-key shlog-mode-map [C-up] 'shell-prompt-rsb)

(map-keymap '(lambda (k b) (define-key shell-mode-map (vector k) b)) shlog-mode-map)

(defun remove-bogus-overlays (&optional start end) (interactive)
  (let* ((pos (or start (point-min)))
	 (end (or end (point-max)))
	 )
    (while (< pos end)
      (mapcar 'delete-overlay (overlays-at pos))
      (setq pos (next-overlay-change pos))
      )
    )
  )
  
(defun shlog-mode () (interactive)
  (setq major-mode 'shlog-mode)
  (use-local-map shlog-mode-map)
  )

;(set-keymap-parent)

(defun instant-shell () (interactive)
  (let* ((name (format "shell-%s" default-directory))
	 (buf (get-buffer name)))
    (cond (buf (switch-to-buffer buf))
	  (t
	   (shell)
	   (rename-buffer name)
	   )))
  )
(define-key global-map [M-f8] 'instant-shell)

(defun shell-from-dired (arg) (interactive "P")
  (let* ((name (format "shell-%s" (or arg 0)))
	 (buf (get-buffer name)))
    (cond (buf (switch-to-buffer buf))
	  (t
	   (shell)
	   (rename-buffer name)
	   )))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shell-buffer-info-save (&optional arg) (interactive "P")
  (let* ((ts (format-time-string "%y%m%d-%H%M%S" shell-start-timestamp))
	(host (downcase system-name))
	(log-name (format "sh-%s-%s.log" host ts))
	(hist-name (format "sh-%s-%s.input" host ts))
	(nvc-enable-force t)
	(cmds (nthcdr 2 comint-input-ring))
	(log (buffer-string))
	)
    (with-file log-name
      (insert
       (format "#~type:{shell.log}\n##host:%s\n##nobackup##\n" host)
       log
       )
      (save-buffer)
      (kill-buffer (current-buffer))
      )
    (with-file hist-name
      (insert (format "#~type:{shell.input-history}\n##host:%s\n##nobackup##\n" host))
      (mapcar '(lambda (x) (and x (insert x "\n"))) cmds)
      (save-buffer)
      (kill-buffer (current-buffer))
      )
    )
  )
(fset 'sbis 'shell-buffer-info-save)

(define-key global-map [M-f8] 'shell-buffer-info-save)
