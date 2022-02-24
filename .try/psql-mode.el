(defvar-local psql-sentinel-string ">>>" "Sentinel")
(defvar-local psql-timeout 10 "Timeout")
(defvar-local psql-timeout-timer nil "Timer")
(defvar-local psql-response nil "Responser")
(defvar-local psql-filter-buffer nil "Filter buffer")
(defvar-local psql-filter-timer nil "Filter timer")
(defvar-local psql-filter-state nil "Filter state")
(defvar-local psql-filter-mark nil "Filter state")

(defun psql-reset-defaults ()
  (interactive)
  (set-default 'psql-sentinel-string ">>>")
  (set-default 'psql-timeout 10)
  )

(defmacro psql-sx (process &rest body)
  `(let ((buffer (process-buffer ,process)))
     (sx (set-buffer buffer)
	 ,@body)
     )
  )

(defmacro psql-swx (process &rest body)
  `(let ((buffer (process-buffer ,process)))
     (swx (switch-to-buffer-other-frame buffer)
	 ,@body)
     )
  )

(defin 'psql-sx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; [1] assume within psql-sx (i.e process and buffer are in scope)

(defun psql-response-found (process o)	; [1]
  (setq psql-response (substring psql-filter-buffer 0 o))
  (setq psql-filter-buffer (substring psql-filter-buffer o))
  (psql-timeout-cancel process)
  (setq psql-filter-state 'done)
  )

(defun psql-filter (process text)
  (psql-sx process
;    (insert (format "<<%s" psql-filter-state))
    (setq psql-filter-state 'dirty)
    (goto-char (point-max))
    (insert text)
;    (insert (format "%d\n" (length text)))
    (goto-char (point-max))

;    (insert "[[" text "]]")
    
    (setq psql-filter-buffer (concat psql-filter-buffer text))
    (let ((o (string-match psql-sentinel-string psql-filter-buffer)))
      (cond
       (o (psql-response-found process o))
       )
      )
;    (insert (format "%s>>" psql-filter-state))
    )
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun psql-timeout-cancel (process)
  (cond ((timerp psql-timeout-timer) (cancel-timer psql-timeout-timer)))
  (setq psql-timeout-timer nil)
  )

(defun psql-timeout-expire (process)
  (psql-sx process
    (end-of-buffer)
    (message "whoops")
    (insert "whoops\n")
    (setq psql-timeout-timer nil)
    (setq psql-filter-state 'timeout)
    )
  )

(defun psql-command-reset (process)
  (setq psql-filter-buffer "")
  (psql-timeout-cancel process)
  )

(defun psql-timeout-set (process &optional timeout)
  (setq psql-timeout-timer
    (run-at-time (or timeout psql-timeout) nil 'psql-timeout-expire process))
  )

(defun psql-send-command-raw* (process send plist)
  (let* ((sentinel (or (plist-get plist :sentinel) psql-sentinel-string))
	 (timeout (or (plist-get plist :timeout) psql-timeout 10))
	 )
    (cond
     (sentinel (setq send (concat send "\n\\echo " sentinel "\n")))
     )
    (end-of-buffer)

    (setq psql-filter-plist plist)
    (setq psql-filter-state 'clean)
    (setq psql-filter-mark (set-marker (make-marker) (point)))
    (psql-command-reset process)
    (process-send-string process send)
    (psql-timeout-set process)
    (with-local-quit
      (while (not (or
		   (eq psql-filter-state 'done)
		   (eq psql-filter-state 'timeout)
		   quit-flag
		   ))
	(sit-for 0.1)
	))
    (message "quit? %s %s" quit-flag psql-filter-state)
    (cond
     ((eq psql-filter-state 'done) psql-response)
     ((eq psql-filter-state 'timeout) 'timeout)
     (quit-flag 'quit)
     )
    )
  )

(defun psql-send-command-raw (process send &rest plist)
  (psql-sx process (psql-send-command-raw* process send plist))
  )

(defun psql-send-command (process command &rest plist)
  (psql-sx process
    (psql-send-command-raw* process command plist)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun psql-process-mode (buffer process)
  (let ((buffer (or buffer (process-buffer process)))
	)
    (sx
     (set-buffer buffer)
     (setq major-mode 'psql-mode)
     (setq mode-name "psql")
     (set-process-filter process 'psql-filter)
     )
    process
    )
  )

(defun psql-process-start-shell (command)
  (let* ((buffer (get-buffer-create "*psql*"))
	 (process (start-process-shell-command "psql" buffer command)))
    (process-put process 'type 'shell)
    (process-put process 'command command)
    (psql-process-mode buffer process)
    )
  )

(defun psql-process-start-noshell (&rest args)
  (let* ((buffer (get-buffer-create "*psql*"))
	 (process (apply 'start-process "psql" buffer "psql" args)))
    (process-put process 'type 'noshell)
    (process-put process 'args args)
    (psql-process-mode buffer process)
    )
  )

(defun psql-restart (process)
  (let* ((type (process-get process 'type))
	 (buffer (process-buffer process)))
    (and (process-live-p process) (kill-process process))
    (cond
     ((eq type 'noshell)
      (apply 'psql-process-start-noshell (process-get process 'args))
      )
     ((eq type 'shell)
      (psql-process-start-shell (process-get process 'command))
      )
     )
    )
  )
	 
