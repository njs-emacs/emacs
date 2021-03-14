;; the aim of this package is to analyze which files get edited close together
;; and provide a mechanism to quickly switch to commonly linked buffers
;; through an automated correlation.
;; we log information about edits into a buffer which can be
;; analyzed to give the files most likely to be edited next in any buffer
;;
;; some files should be excluded, and treated like non-file buffers
;; some switches to files where no editing took place should be
;; removed.
;; In any case the log may come in handy as an addition to the old
;; emacs.log file which does not always deliver good results
;;
;; there are definite problems when switching into a non-file buffer

(defun delete-line ()
  (bol)
  (delete-region (point) (sxp (fl 1)))
  )
 
(defun delete-last-line ()
  (eob)
  (fl -1)
  (delete-line)
  )
 
(defun ch-in ()
  (add-hook 'pre-command-hook 'ch-pre)
  (add-hook 'post-command-hook 'ch-post)
  )

(defun ch-out ()
  (remove-hook 'pre-command-hook 'ch-pre)
  (remove-hook 'post-command-hook 'ch-post)
  )


(setq ch-buffer-key-count 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ch-save () (interactive)
  (setq name (daily-date-path (format-time-string "boo-%y%m%d.bch")))
  (set-buffer (get-buffer" *bch"))
  (write-region (point-min) (point-max) name)

  (setq name (daily-date-path (format-time-string "boo-%y%m%d.kch")))
  (set-buffer (get-buffer" *kch"))
  (write-region (point-min) (point-max) name)
  )

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
	(eob)
	(insert (format "(pre %S %s %d)\n" name n ch-buffer-key-count))
;	(sit-for 0.1)
	)
      )
     )
    )
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
	(eob)
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

;(ch-in)
;(setq ch-save-timer (run-with-timer 600 600 'ch-save))

;(ch-out)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq kch-buffer-name " *kch")
(defvar kch-buffer-current nil)
(defvar kch-current-keys nil)

(defun kch-pre ()
  (let* ((buffer (current-buffer))
	 (name (or (buffer-file-name)
		  (format "<%s>" (buffer-name))))
	 (n (buffer-chars-modified-tick))
	 (cmd (cond ((symbolp this-command) this-command)
		    ("?")
		    ))
	 (keys (key-description (this-command-keys)))
	 (confirmed
	  (cond
	   ((buffer-file-name)))
	  )
	 )
    (cond
     (confirmed
      (save-excursion
	(set-buffer (get-buffer-create kch-buffer-name))
	(eob)
	(cond ((not (eq kch-buffer-current buffer))
	       (insert (format "%s\n" name))
	       ))
	(cond ((equal keys kch-current-keys)
	       (delete-last-line)
	       )
	      ((setq kch-current-count 0))
	      )
	(setq kch-current-count (1+ kch-current-count))
	(setq kch-current-keys keys)
	(setq kch-buffer-current buffer)
	(insert (format "[%d] %-24s %s\n" kch-current-count keys cmd))
	)
      )
     )
    )
  )

;(add-hook 'pre-command-hook 'kch-pre)
;(remove-hook 'pre-command-hook 'ch-pre)

(top-level)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pch ()
  (message "%s %s %s" (buffer-file-name) this-command (this-command-keys)))

(add-hook 'pre-command-hook 'pch)
(remove-hook 'pre-command-hook 'pch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'pre-command-hook 'ch-pre)
(remove-hook 'pre-command-hook 'ch-pre)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (add-hook 'post-command-hook 'ch-post)
(remove-hook 'post-command-hook 'ch-post)

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
