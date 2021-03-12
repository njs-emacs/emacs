(defun pch ()
  (message "%s %s %s" (buffer-file-name) this-command (this-command-keys)))

(add-hook 'pre-command-hook 'pch)
(remove-hook 'pre-command-hook 'pch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ch-last-info nil)

(defun ch-pre ()
  (let* ((name (or (buffer-file-name)
		  (format "<%s>" (buffer-name))))
	 (n (buffer-chars-modified-tick))
	 (cmd (cond ((symbolp this-command) this-command)
		    ("?")
		    ))
	 (keys (key-description (this-command-keys)))
	 (confirmed (cond ((buffer-file-name))))
	 )
    (cond
     (confirmed
      (save-excursion
	(set-buffer (get-buffer-create " *bch"))
	(end-of-buffer)
	(insert (format "%s %s %-10s %s\n" name n keys cmd))
	(end-of-buffer)
	)
      )
     )
    )
  )

(add-hook 'pre-command-hook 'ch-pre)
(remove-hook 'pre-command-hook 'ch-pre)

(setq ch-buffer-key-count 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ch-pre ()
  (let* ((name (or (buffer-file-name)
		  ( format "<%s>" (buffer-name))))
	 (n (buffer-chars-modified-tick))
	 (confirmed (cond ((buffer-file-name))))
	 )
    (cond
     (confirmed
      (save-excursion
	(set-buffer (get-buffer-create " *bch"))
	(end-of-buffer)
	(insert (format "(pre %S %s %d)\n" name n ch-buffer-key-count))
	(end-of-buffer)
;	(sit-for 0.1)
	)
      )
     )
    )
  )

(add-hook 'pre-command-hook 'ch-pre)
(remove-hook 'pre-command-hook 'ch-pre)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delete-line ()
  (bol)
  (delete-region (point) (sxp (fl 1)))
  )
 
(defun ch-post ()
  (let* ((nb (current-buffer))
	 (name (or (buffer-file-name)
		  (format "<%s>" (buffer-name))))
	 (n (buffer-chars-modified-tick))
	 (confirmed (cond ((buffer-file-name))))
	 (prev)
	 )
    (cond
     (confirmed
      (setq ch-buffer-key-count (1+ ch-buffer-key-count))
      (save-excursion
	(set-buffer (get-buffer-create " *bch"))
	(end-of-buffer)
	(fl -1)
	(bol)
	(setq prev (read (current-buffer)))
	(bol)
	(delete-region (point) (sxp (fl 1)))
	(let* ((obn (cadr prev))
	       (ob (get-file-buffer obn))
	       (on (caddr prev))
	       )
;	  (message "%s %S %s" prev ob nb)
	  (cond
	   ((eq ob nb)
	    (cond ((eq on n))
		  (t
		   (cond ((> ch-buffer-key-count 0) (fl -1) (delete-line)))
		   (insert (format "(post %S %s %d)\n" name n ch-buffer-key-count)))
		  )
	    )
	   (t
	    (setq ch-buffer-key-count 0)
	    (insert (format "(post %S %s %d)\n" name n  ch-buffer-key-count))
	    )
	   )
	  )
	)
      )
     )
    )
   )

(ch-in)
(ch-out)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (add-hook 'post-command-hook 'ch-post)
(remove-hook 'post-command-hook 'ch-post)

(defun ch-in ()
  (add-hook 'pre-command-hook 'ch-pre)
  (add-hook 'post-command-hook 'ch-post)
  )

(defun ch-out ()
  (remove-hook 'pre-command-hook 'ch-pre)
  (remove-hook 'post-command-hook 'ch-post)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


post-command-hook

this-command

this-command

(key-description "\C-d")
modif
change.* hook
(buffer-modified-tick)
(buffer-chars-modified-tick)

; https://www.gnu.org/software/emacs/manual/html_node/elisp/Change-Hooks.html#Change-Hooks
; before-change-functions
; after-change-functions
