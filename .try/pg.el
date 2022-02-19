(load "emacsql")
(load "emacsql-sqlite")
(load "emacsql-psql")
(load "emacsql-mysql")

(apropos "emacsql")
(sql-postgres)

(setq db (emacsql-sqlite "company.db"))

(setq db (emacsql-psql "emacshist" :username "postgres" :hostname "localhost"))
(setq db (emacsql-psql "emacshist"))

(call-shell "psql -U postgres 
