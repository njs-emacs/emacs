(define-key query-replace-map "S" 'auto-sit)
(define-key query-replace-map "D" 'debug)

(defun query-put (tag)
  (let ((x (assoc tag query-info)))
	(cond
	 (x (setcdr x (1+ (cdr x))))
	 ((setq query-info (cons (cons tag 0) query-info)))
	 )
	)
  )

(defun query-get (tag)
  (let ((x (assoc tag query-info)))
	(cond
	 (x (cdr x))
	 (0)
	 )
	)
  )

(defun dsit-for (n)
  "Like sit-for, but flushes all pending input before returning"
  (cond ((sit-for n) t)
		(t (while (not (sit-for 0)) (read-char)) nil)
		))

(defun query-read (&optional prompt)
  (message (or prompt "?"))
  (setq query-key (vector (read-event)))
  (lookup-key query-replace-map query-key)
  )

(defun \ query (scan message get list)
  (let (*x *xx (conf t) (go-on t) sit query-info stay prev)
    (while (and go-on (or stay (eval scan)))
      (let ((default-directory default-directory))
	(cond (conf
	       (setq *x (funcall (or get 'query-read) (eval message)))
	       (setq *xx *x)
	       (setq stay t)
	       ))
	(and sit (cond
		  ((setq sit (dsit-for 1)))
		  (t (setq conf t *x 'stay))
		  ))
	(case *x
	  (automatic
	   (setq conf nil)
	   (setq *x 'act)
	   (setq *xx 'act)
	   )
	  (auto-sit
	   (setq conf nil)
	   (setq *x (or prev 'act))
	   (setq *xx *x)
	   (setq sit t)
	   )
	  (recenter
	   (recenter nil)
	   )
	  (edit
	   (recursive-edit))
	  (delete-and-edit
	   (recursive-edit))
	  (act-and-show
	   (setq *xx 'act)
	   (setq *x 'show)
	   )
	  (act-and-exit
	   (setq *xx 'act)
	   (setq *x 'exit)
	   )
	  (debug
	   (debug))
	  (backup
	   )		
	  (act
	   )
	  (skip
	   )
	  (quit
	   )
	  (stay
	   )
	  (t
	   (setq *xx 'unknown)
	   (setq *x 'unknown)
	   )
	  )
	(eval (append '(case *xx) list))
	(case *x
	  ((act skip)
	   (query-put 'total)
	   (query-put *xx)
	   (setq stay nil prev *x))
	  (exit
	   (setq go-on nil))
	  (show
	   (read-event)
	   (setq stay nil))
	  (unknown
	   (setq unread-command-events
	     (append (listify-key-sequence query-key) unread-command-events))
	   (setq go-on nil)
	   )
	  )
	))
    (or unread-command-events (message "Done"))
    )
  nil
  )

(defmacro query (scan message get &rest body)
  "Repeatedly evaluate SCAN form until it evaluates to nil.
For each iteration, read a confirmation (prompt with PROMPT) 
the evaluate the form found in HANDLERS which matches the 
command which would have been appropriate in query-replace and.
"
  (` (\ query '(, scan) '(, message) (, get) '(, body)))
  )

(defin 'query)

