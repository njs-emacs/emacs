(load "emacsql")
(load "emacsql-sqlite")
(load "emacsql-psql")
(load "emacsql-mysql")

(apropos "emacsql")
(sql-postgres)

(setq db (emacsql-sqlite "company.db"))

(setq db (emacsql-psql "emacshist" :username "postgres" :hostname "boo"))
(setq db (emacsql-psql "emacshist"))

(call-shell "psql -U postgres")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(start-process-shell-command
 "emacsql-psql" 
 (get-buffer " *emacsql-psql*")
 "stty raw && \"psql\" \"emacshist\" \"postgres\" \"-n\" \"-h\" \"boo\""
)

"stty raw && " "\"psql\" \"emacshist\" \"postgres\" \"-n\" \"-h\" \"boo\""
"stty raw && \"psql\" \"emacshist\" \"postgres\" \"-n\" \"-h\" \"boo\""

(defun goo (command)
 (start-process-shell-command
 "emacsql-psql" 
 (get-buffer " *emacsql-psql*")
 command
))

(goo "psql -n -d emacshist -U postgres -h boo")
(goo "stty raw && \"psql\" \"emacshist\" \"postgres\" \"-n\" \"-h\" \"boo\"")
(goo "\"psql\" \"-n\" \"-d\" \"emacshist\" \"-U\" \"postgres\" \"-h\" \"boo\"")
(goo "stty raw && psql -n -d emacshist -U postgres -h boo")
(goo "psql -n -d emacshist -U postgres -h boo")

"stty raw && \"psql\" \"-n\" \"-d\" \"emacshist\" \"-U\" \"pos...")
(get-buffer-process (get-buffer " *emacsql-psql*"))

(kill-process (get-process "emacsql-psql<1>"))
(kill-process (get-process "emacsql-psql"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun emacsql-psql-command (args)
  (let* ((psql emacsql-psql-executable)
	 (args-string (mapconcat #'shell-quote-argument (cons psql args) " "))
         (command (concat "stty raw && " args-string))
	 )
    command)
  )

(cl-defun emacsql-psql (dbname &key username hostname port debug)
  "Connect to a PostgreSQL server using the psql command line program."
  (let ((args))
    (push "-n" args)
    (push "-d" args)
    (push dbname args)
    (when username
      (push "-U" args)
      (push username args))
    (when port
      (push "-p" args)
      (push port args))
    (when hostname
      (push "-h" args)
      (push hostname args))

    (setf args (nreverse args))
    (let* ((buffer (generate-new-buffer " *emacsql-psql*"))
           (psql emacsql-psql-executable)
           (command (emacsql-psql-command args))
	(zz (debug))
           (process (start-process-shell-command
                     "emacsql-psql" buffer command))
           (connection (make-instance 'emacsql-psql-connection
                                      :process process
                                      :dbname dbname)))
      (setf (process-sentinel process)
            (lambda (proc _) (kill-buffer (process-buffer proc))))
      (when debug (emacsql-enable-debugging connection))
      (mapc (apply-partially #'emacsql-send-message connection)
            '("\\pset pager off"
              "\\pset null nil"
              "\\a"
              "\\t"
              "\\f ' '"
              "SET client_min_messages TO ERROR;"
              "\\set PROMPT1 ]"
              "EMACSQL;")) ; error message flush
      (emacsql-wait connection)
      (emacsql connection
               [:set (= default-transaction-isolation 'SERIALIZABLE)])
      (emacsql-register connection))))

