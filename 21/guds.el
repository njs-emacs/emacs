(defun gud-proc () (get-buffer-process gud-comint-buffer))

(defun gud-send (&rest s) (interactive)
  (save-window-excursion
    (set-buffer gud-comint-buffer)
    (goto-char (point-max))
    (set-marker comint-last-input-start	(process-mark (gud-proc)))
    (insert (cat s "\n"))
    (comint-send-input)
    (end-of-buffer)
    (sit-for 1)
    ))

(defun gud-ssend (s) (interactive)
  (process-send-string (gud-proc) (concat s "\n"))
  )

(setq gud-filter-stack nil)

(defun gud-filter-push (fun)
  (setq gud-filter-stack (cons (process-filter (gud-proc)) gud-filter-stack))
  (set-process-filter (gud-proc) fun))

(defun gud-filter-pop ()
  (set-process-filter (gud-proc) (car gud-filter-stack))
  (setq gud-filter-stack (cdr gud-filter-stack)))

(defun gud-get-filter (proc string)
  (setq gud-output (concat gud-output string))
  )

(defun buffer-local-value (sym &optional buffer)
 (cdr (assq sym (buffer-local-variables buffer))))

(defun gud-value (sym)
 (buffer-local-value sym gud-comint-buffer))

(defun gud-prompt ()
  (gud-value 'comint-prompt-regexp))

(defun gud-get (s &optional match)
  (setq match (or match (gud-prompt)))
  (let (i)
    (gud-filter-push 'gud-get-filter)
    (setq gud-output "")
    (gud-ssend s)
    (unwind-protect
	(while
	    (not (setq i (string-match match gud-output)))
	  (accept-process-output (gud-proc)))
      (gud-filter-pop))
    (substring gud-output 0 i))
  )

(defun dbx-where ()
  (let* ((s (gud-get "where"))
		 (f (string-parse s "=>\\[[0-9]*] *\\([^(]*\\).*line *\\([0-9]*\\)[^\"]*\"\\([^\"]*\\)" 3 2 1))
		 )
	(gud-display-line (car f) (read (cadr f)))
	)
  )

(defun gud-up () (interactive)
  (gud-send "up")
  (dbx-where)
  )

(defun gud-down () (interactive)
  (gud-send "down")
  (dbx-where)
  )

