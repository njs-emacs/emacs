(defvar-local psql-sentinel-string ">>>" "Sentinel")
(defvar-local psql-sentinel-active nil "Actual expected sentinel")
(defvar-local psql-timeout 10 "Timeout")
(defvar-local psql-timeout-timer nil "Timer")
(defvar-local psql-response nil "Response")
(defvar-local psql-filter-accumulator nil "Filter buffer")
(defvar-local psql-filter-timer nil "Filter timer")
(defvar-local psql-filter-state nil "Filter state")
(defvar-local psql-filter-mark nil "Buffer insertion point")
(defvar-local psql-filter-insert-mute nil "Mute filter insertion")
(defvar-local psql-create-form nil "")

(setq psql-mode-map (make-sparse-keymap))

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
; we are going to disable this

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
  (setq psql-filter-accumulator "")
  (psql-timeout-cancel process)
  )

(defun psql-timeout-set (process &optional timeout)
  (setq psql-timeout-timer
    (run-at-time (or timeout psql-timeout) nil 'psql-timeout-expire process))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; [1] assume within psql-sx (i.e process and buffer are in scope)

(defun psql-response-found (process o)	; [1]
  (setq psql-response (substring psql-filter-accumulator 0 o))
  (setq psql-filter-accumulator (substring psql-filter-accumulator o))
;;  (psql-timeout-cancel process)
  (setq psql-filter-state 'done)
  )

(defun psql-filter (process text)
  (psql-sx process
    (setq psql-filter-state 'dirty)
;    (goto-char (point-max))
    (cond
     (psql-filter-insert-mute)
     (t (insert text))
     )
;    (goto-char (point-max))

    (setq psql-filter-accumulator (concat psql-filter-accumulator text))
    (let ((o (string-match psql-sentinel-active psql-filter-accumulator)))
      (cond
       (o (psql-response-found process o))
       )
      )
    )
  )
  
(defun psql-filter-insert-mute ()
  (interactive)
  (setq psql-filter-insert-mute t)
  )

(defun psql-filter-insert-mute-toggle (&optional process)
  (interactive)
  (setq psql-filter-insert-mute (not psql-filter-insert-mute))
  (message "psql-filter-insert-mute set to %s" psql-filter-insert-mute)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun psql-send-command-raw* (process send plist)
  (let* ((sentinel (or (plist-get plist :sentinel) psql-sentinel-string))
	 (timeout (or (plist-get plist :timeout) psql-timeout 10))
	 )
    (cond
     (sentinel (setq send (concat send "\n\\echo " sentinel "\n")))
     )
    (goto-char (point-max))

    (setq psql-sentinel-active sentinel)

    (setq psql-filter-plist plist)
    (setq psql-filter-state 'clean)
    (setq psql-filter-mark (set-marker (make-marker) (point)))
    (psql-command-reset process)
    (process-send-string process send)
;;    (psql-timeout-set process)
    (with-local-quit
      (while (not (or
		   (eq psql-filter-state 'done)
		   (eq psql-filter-state 'timeout)
		   quit-flag
		   ))
	(sit-for 0.1)
	))
;    (message "quit? %s %s" quit-flag psql-filter-state)
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
(defun psql-process-mode (&optional buffer process)
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
	 (process (get-buffer-process buffer))
	)
    (sx
     (set-buffer buffer)
     (setq major-mode 'psql-mode)
     (use-local-map psql-mode-map)
     (setq mode-name "psql")
     (set-process-filter process 'psql-filter)
     )
    process
    )
  )

(defun psql-process-start-shell-1 (buffer command)
  (let* ((process (start-process-shell-command "psql" buffer command)))
    (psql-process-mode buffer process)
    (setq psql-create-form `(psql-process-start-shell-1 ,command))
    process
    )
  )

(defun psql-process-start-shell (command)
  (let* ((buffer (get-buffer-create "*psql*")))
    (psql-process-start-shell-1 buffer command)
    )
  )

(defun psql-process-start-noshell-1 (buffer &rest args)
  (let* ((process (apply 'start-process "psql" buffer "psql" args)))
    (psql-process-mode buffer process)
    (setq psql-create-form `(psql-process-start-noshell-1 ,@args))
    process
    )
  )

(defun psql-process-start-noshell (&rest args)
  (let* ((buffer (get-buffer-create "*psql*")))
    (apply 'psql-process-start-shell-1 buffer args)
    )
  )

(defun psql-restart* (buffer)
  (debug)
  (sx (set-buffer buffer)
      (let* ((process (get-buffer-process buffer))
	     (type (car psql-create-form))
	     )
	(and (process-live-p process) (kill-process process))
	(cond
	 ((eq type 'psql-process-start-noshell-1)
	  (message "restarting %s psql process (%s)" type process)
	  (apply type buffer (cdr psql-create-form))
	  )
	 ((eq type 'psql-process-start-shell-1)
	  (message "restarting %s psql process (%s)" type process)
	  (apply type buffer (cdr psql-create-form))
	  )
	 )
	)
      )
  )

(defun psql-restart (&optional buffer)
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
	 )
    (psql-restart* buffer)
    )
  )

(define-key psql-mode-map (kbd "C-c C-c") 'psql-restart)
	 
;; interactive psql-restart does not restore 'psql symbol
;; 
