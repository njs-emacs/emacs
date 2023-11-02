;; gmug is a variation on mug for use in gdb buffers

(defvar gud-filter-last-reception-time nil)
(defvar gmug-filter-text "")

(defun gmug-timeout (duration)
  (> (time-to-seconds
      (time-subtract (current-time) gud-filter-last-reception-time)) duration)
  )

(defun gmug-timeout-reset ()
  (setq gud-filter-last-reception-time (current-time))
  )

(defun gmug-filter (proc text)
  (gmug-timeout-reset)
;  (debug)
  (setq gmug-filter-text (concat gmug-filter-text text))
  (cond
   ((string-match "(gdb) " text) (throw 'done gmug-filter-text))
   )
  )

(defun gmug-output-complete (proc text)
  (gmug-timeout-reset)
;  (debug)
  (setq gmug-filter-text (concat gmug-filter-text text))
  )



(defmacro gmug-let (&rest body)
  `(let* ((gbuf gud-comint-buffer)
	  (gproc (get-buffer-process gbuf)))
     ,@body)
  )

(defun gmug-send (s &optional timeout)
  (let* ((i (string-match "#" s))
	 (lplist)
	 (plist (copy-list plist))
	 )
    (cond (i
	   (setq lplist (eval (read (format "`(%s)" (substring s (match-end 0))))))
	   (setq plist (plist-merge-x plist lplist))
	   (setq s (substring s 0 i))
	   )
	  )
	 
  (let* ((gbuf gud-comint-buffer)
	 (gproc (get-buffer-process gbuf))
	 (filter (process-filter gproc))
	 (timeout (or timeout 10))
	 )

    (unwind-protect
	(catch 'done
	  (let ()
	    (gmug-timeout-reset)
	    (setq gmug-filter-text "")
	    (set-process-filter gproc 'gmug-filter)
	    (comint-send-string gproc s)
	  
	    (while t
	      (cond ((gmug-timeout timeout) (throw 'done 'timeout)))
	      (sleep-for 0.1)
	      )
	    )
	  )
      (set-process-filter gproc filter)
      )
    gmug-filter-text
    )
  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gmug-split (out) (unconcat-lines out))

(defun gmug-split-filter (out tag)
  (let* ((lines (unconcat-lines out)))
    (subset-if '(lambda (x) (string= (substring x 0 1) tag)) lines)
    )
  )

(defun gmug-parse (out tag)
  (let* ((lines (gmug-split-filter out tag))
	 (rlines (mapcar '(lambda (x) (substring x 1)) lines))
	 (rlines (mapcar '(lambda (x) (read x)) rlines))
	 )
    (mconcat rlines "")
    )
  )

(defun gmug-send-t (input &optional timeout)
  (let* ((out (gmug-send input timeout))
	 (pout (gmug-parse out "~"))
	 )
    pout
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
