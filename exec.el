;#~

(setq exec-map (make-sparse-keymap))

(define-key exec-map "\ee" 'exec-run)
(define-key exec-map "\er" 'exec-again)

; exec-map needs to find a binding

(defun exec-init (f) (interactive "FFile: ")
  (setq exec-file (expand-file-name f))
  (setq exec-out "*exec*")
  (setq exec-args nil)
  (setq exec-tty nil)
  )

(defun exec-sanity (args)
  (or (boundp 'exec-file) (call-interactively 'exec-init))
  (and args (setq exec-args (cond ((listp args) args) ((list args)))))
  )
 
(defun exec-tty-sanity (args)
  (exec-sanity args)
  (or (and (boundp 'exec-tty) exec-tty)
      (setq exec-tty (read-from-minibuffer "tty number: " nil nil t)))
  )
 
(defun exec-run (&rest args) (interactive "xargs: ")
  (exec-sanity args)
  (and (get-buffer-create exec-out)
       (set-buffer exec-out)
       (setq truncate-lines t)
       (erase-buffer))
  (apply 'call-process exec-file nil exec-out nil exec-args)
  (display-buffer exec-out)
  )

(defun exec-gdb (&rest args) (interactive "xargs: ")
  (exec-sanity args)
  (gdb-ssend (concat "run " (cat exec-args " ")))
  )

(defun exec-tty (&rest args) (interactive "xargs: ")
  (exec-tty-sanity args)
  (let ((ttyname (format "/dev/ttyp%s" exec-tty)))
    (apply 'call-shell exec-file ">" ttyname "<" ttyname args))
  )

(defun exec-gdb-tty (&rest args) (interactive "xargs: ")
  (exec-tty-sanity args)
  (let ((ttyname (format "/dev/ttyp%s" exec-tty)))
    (gdb-ssend (concat "run " (cat exec-args " ") " >" ttyname " <" ttyname)))
  )

(defun exec-smart (&rest args) (interactive "xargs: ")
  (exec-sanity args)
  (apply (if (and (fboundp 'gdb-proc) (gdb-proc)) 'exec-gdb 'exec-run) args)
  )

(defun argx (&rest args)
  (apply 'concat
	 (mapcar '(lambda (arg)
		    (and arg
			 (cond ((stringp arg) arg)
			       ((symbolp arg) (symbol-name arg))
			       ((numberp arg) (hex-l arg))
			       ((listp arg) (apply 'argx arg))))) args)))


