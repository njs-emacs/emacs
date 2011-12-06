(load-standard "gud")

(defun gud-quick (arg) (interactive "P")
  (if arg (call-interactively gud-debugger)
    (if (not (and (boundp 'gud-comint-buffer)
		  gud-comint-buffer
		  (buffer-name gud-comint-buffer)))
	(if gud-exec-file
	    (funcall gud-debugger (concat gdb-exec-dir gdb-exec-file))
	  (call-interactively gud-debugger))))
  (switch-to-buffer gud-comint-buffer)
  (comint-wait)
  )

(defun gud-insert (&rest s) (interactive)
  (sx (set-buffer gud-comint-buffer)
      (eob)
      (insert-before-markers (insert (cat s " ")))
      )
  )

(defun gud-ssend (s) (interactive "s")
  (let ((buffer gud-comint-buffer))
    (process-send-string (get-buffer-process buffer) s)
    )
  )

(defun gud-send (s) (interactive "s")
  (let ((buffer gud-comint-buffer))
    (send (get-buffer-process buffer) s t)
    )
  )

(defun gud-get (s &optional match)
  (comint-get gud-comint-buffer s match)
  )

(defun gud-display-line (true-file line)
  (cond ((not (boundp gud-source-frame)) 
	 (setq gud-source-frame (selected-frame)))
	)
  (select-frame gud-source-frame)
  (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
	 (buffer (gud-find-file true-file))
	 (window (display-buffer buffer))
	 (pos))
;;;    (if (equal buffer (current-buffer))
;;;	nil
;;;      (setq buffer-read-only nil))
    (save-excursion
;;;      (setq buffer-read-only t)
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))

(load-overrides "gud")
