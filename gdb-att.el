(defun get-pid (s) (interactive)
  (save-window-excursion
    (shell-command "ps" nil)
    (unwind-protect
	(save-excursion
	  (set-buffer "*Shell Command Output*")
	  (goto-char (point-min))
	  (re-search-forward
	   (format "^ *\\([0-9]*\\)[^:]*:.. %s[ \n]"
		   (file-name-nondirectory s)))
	  (match-string 1))
      (kill-buffer "*Shell Command Output*")))
  )

(defun gdb-attach-process () (interactive)
  (message "attaching...")
  (gdb-send (format "attach %s" (get-pid gdb-exec-file)))
  (message "done")
  )

(define-key gdb-a-map "\et" 'gdb-exec-tty-get)

(defun gdb-exec-tty-get () (interactive)
  (setq gdb-exec-tty (read-from-minibuffer "tty: " "/dev/ttyp")))

(define-key gdb-a-map "\ek"
  '(lambda () (interactive) (gdb-send "kill")))

(define-key gdb-a-map "\er"
  '(lambda () (interactive)
     (gdb-send (format "run >%s <%s" gdb-exec-tty gdb-exec-tty))))


