;;; sqlint-mode is inferior sql process interactive mode
;;; sql-mode is mode for editing sql files
;;;
;;;
;;;

;;; 

(defun sql-show (s)
  )

(defun sql-exec () (interactive)
  (save-buffer)
  (sqlint-start)
  (let ((name (format "%s.out" (file-name-nondirectory (buffer-file-name)))))
    (show1 name
     (sqlint-get (format "start %s" (buffer-file-name))))
    (set-buffer name)
    (set-visited-file-name name)
    (save-buffer)
    )
  )

(defun sql-exec-region () (interactive)
  (let ((start (sx (cond ((rsb "^rem(") (fl 1) (point^)) ((point-min)))))
	(end (sx (cond ((rsf "^rem)") (point^)) ((point-max)))))
	)
    (sqlint-start)
    (show1 "*<sql output>*"
	   (sqlint-get (bs start end)))
    )
  )

(defun sql-exec-command () (interactive)
  (let* ((start (sxp (rsb "^\\S " nil nil 0 nil)))
	 (end (sxp (fl 1) (point^)))
	 (colon (sx (goto-char start) (rsf ";" end)))
	 (s (bs start (1- (or colon end))))
	 )
    (sqlint-send s)
    (show1 "*<sql output>*" (sqlint-get "/"))
    )
  )

(defun sql-mouse-exec-region (arg) (interactive "e")
  (mouse-set-point arg)
  (sql-exec-region)
  )

(defun sql-mode-variables ()
  (setq lisp-sexp-prefix-regexp "^rem\\s *(")
  )

(defun sql-mode-commands ()
  (setq sql-mode-map (make-sparse-keymap))
  (define-key sql-mode-map [f9] 'sql-exec)
  (define-key sql-mode-map [M-kp-enter] 'sql-exec-region)
  (define-key sql-mode-map [M-S-mouse-1] 'sql-mouse-exec-region)
  (define-key sql-mode-map "\e\C-M" 'sql-exec-command)
  (define-key sql-mode-map "\C-x\C-h" 'sql-get-help)
  )

(setq sql-mode-hook nil)

(defun sql-mode ()
  "Major mode for editing sql code.
Commands:
\\{sql-mode-map}
Entry to this mode calls the value of `sql-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (sql-mode-commands)
  (use-local-map sql-mode-map)
  (setq major-mode 'sql-mode)
  (setq mode-name "SQL source")
  (sql-mode-variables)
  (run-hooks 'sql-mode-hook)
  (setq case-fold-search t)
  )

;;;

(setq sql-commands
  (list
  "accept"
  "append"
  "break"
  "btitle"
  "change"
  "clear"
  "column"
  "compute"
  "connect"
  "copy"
  "define"
  "del"
  "describe"
  "disconnect"
  "edit"
  "execute"
  "exit"
  "get"
  "host"
  "input"
  "list"
  "pause"
  "prompt"
  "print"
  "remark"
  "run"
  "runform"
  "save"
  "set"
  "show"
  "spool"
  "sqlplus"
  "start"
  "timing"
  "undefine"
  "variable"
  "whenever"
  "alter cluster"
  "alter database"
  "alter index"
  "alter rollback"
  "alter sequence"
  "alter table"
  "alter tablespace"
  "alter user"
  "audit"
  "begin"
  "close"
  "comment"
  "commit"
  "create cluster"
  "create database"
  "create db link"
  "create index"
  "create rollback"
  "create sequence"
  "create synonym"
  "create table"
  "create tablespace"
  "create view"
  "declare"
  "delete"
  "drop"
  "end"
  "exception"
  "exit"
  "fetch"
  "goto"
  "grant"
  "if"
  "insert"
  "lock table"
  "loop"
  "noaudit"
  "null"
  "open"
  "raise"
  "rename"
  "revoke"
  "rollback"
  "savepoint"
  "select"
  "set transaction"
  "update"
  "validate index"
  ))

(setq sql-command-args (list
  '("select" (list "distinct"))
  '("describe" sql-tables))
  )

(defun sqlint-dynamic-complete-command ()
  (let* ((partial (or (comint-match-partial-filename) ""))
		 (cmd (bs (sx (rsb comint-prompt-regexp) (me 0))
				  (sxp (rsb "\\s +" (me 0) 0))
				  ))
   		 (args (cond ((string-match cmd "^\\s *$") nil)
					 ((cdr (assoc cmd sql-command-args)))))
		 )
	(cond (args (comint-dynamic-simple-complete partial
						    (eval (cons 'progn args))))
		  ((comint-dynamic-simple-complete partial sql-commands))
		  ))
  )

(defvar sqlint-mode-syntax-table nil)
(defvar sqlint-mode-abbrev-table nil)
(defvar sqlint-mode-map nil)
  
(if sqlint-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?  " " table)
    (setq sqlint-mode-syntax-table table))
  )

(define-abbrev-table 'sqlint-mode-abbrev-table ())

(defun sqlint-mode-commands (map)
  (define-key sqlint-mode-map "\C-x\C-h" 'sql-get-help)
  )

(defun sqlint-mode-variables ()
  (set-syntax-table sqlint-mode-syntax-table)
  (setq local-abbrev-table sqlint-mode-abbrev-table)
  )

(defvar sqlint-mode-map nil)
(defvar sqlint-buffer nil)

(defun sqlint-mode ()
  (interactive)
  (require 'comint)
  (setq comint-input-ring-size 64)
  (comint-mode)
  (setq major-mode 'sqlint-mode
	mode-name "SQL Interactive"
	comint-prompt-regexp sqlint-prompt-regexp)
  (sqlint-mode-variables)
  (if sqlint-mode-map nil
    (setq sqlint-mode-map (copy-keymap comint-mode-map))
    (sqlint-mode-commands sqlint-mode-map))
  (define-key sqlint-mode-map "\t" 'comint-dynamic-complete)
  (use-local-map sqlint-mode-map)
  (setq comint-input-filter
	'(lambda (x)
	   (or (ring-empty-p comint-input-ring)
		   (not (equal x (ring-ref comint-input-ring 0))))))
  (setq sqlint-buffer (current-buffer))
  (set-process-sentinel (get-buffer-process (current-buffer)) 'sqlint-sentinel)
  (setq comint-input-ring-file-name ".sqlhist")
  (comint-read-input-ring t)
  (setq comint-dynamic-complete-functions
	'(comint-replace-by-expanded-history
	  sqlint-dynamic-complete-command
	  )
	)
  (run-hooks 'sqlint-mode-hook)
  )

(defun sqlint-sentinel (proc msg)
  (cond ((memq (process-status proc) '(signal exit))
		 (comint-write-input-ring)
		 (kill-buffer (process-buffer proc))
		 ))
  )

(defun sqlint-start ()
  (require 'comint)
  (cond ((process sqlint-buffer))
	(t (sx (set-buffer
		(apply 'make-comint "SQL" sqlint-program-name nil
		       sqlint-user-password sqlint-start-args))
	       (sqlint-mode)
	       (comint-wait)
	       )
	   )
	)
  sqlint-buffer
  )

(defun sqlint ()
  "Run an inferior SQL process, input and output via buffer *SQL*."
  (interactive)
  (switch-to-buffer (sqlint-start))
  )

(defun sqlint-send (&rest list)
  (sqlint-start)
  (apply 'send sqlint-buffer list)
  )
  
(defun sqlint-get (s)
  (sqlint-start)
  (comint-get sqlint-buffer s))

(defun sqlint-get-describe-table (name)
  (sxk
   (set-buffer (show (sql-get (format "describe %s ;" name))))
   (bob)
   (kill-line 2)
   (map-buffer-lines
    (and (rsf "^ *\\(\\S +\\)\\s +\\(NOT NULL\\|\\)\\s +\\([A-Z][^(\n]*\\)\\(([0-9,]+)\\|\\)$")
	 (let* ((name (ms 1))
		(not-null (ms 2))
		(type (ms 3))
		(size (ms 4))
		)
	   (list
	    name
	    (intern type)
	    (reads (string-substitute size ?, ? ))
	    (> (length not-null) 0)
	    )
	   )))
   ))

(defun sql-get-select-list (command)
  (sxk
   (set-buffer (show (sql-get command)))
   (bob)
   (narrow-to-region (point-min) (sxp (rsf "rows selected") (bol)))
   (map-search "^\\s *\\S +"
	       (gmb 0)
	       (readc))
   ))

(autoload 'find-tag-default "etags")

(defun sql-get-help (&optional subject)
  (interactive
   (let* ((subject (and (not (comint-clean)) (find-tag-default))))
     (list (read-from-minibuffer "Help on? " subject))))
  (message "getting help on `%s'... wait" subject)
  (show1 "*SQL help*" (sqlint-get (format "help %s" (or subject ""))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	site specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(defvar sqlint-program-name "sqlplus"
;  "*Program name for invoking an inferior SQL with `sqlint'.")
;
;(setq sqlint-user-password "/")
;(setq sqlint-start-args nil)
;(setq sqlint-prompt-regexp "^SQL> *")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defvar sqlint-program-name "J:\\ORANT\\BIN\\plus33"
;  "*Program name for invoking an inferior SQL with `sqlint'.")
;(setq sqlint-user-password "system/manager")
;(setq sqlint-start-args nil)
;(setq sqlint-prompt-regexp "^SQL> *")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defvar sqlint-program-name "J:\\ORANT\\BIN\\plus33"
;  "*Program name for invoking an inferior SQL with `sqlint'.")
(defvar sqlint-program-name "sqlplus"
  "*Program name for invoking an inferior SQL with `sqlint'.")
(setq sqlint-user-password "/")
(setq sqlint-start-args nil)
(setq sqlint-prompt-regexp "^SQL> *")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sql.el ends here
;;;
