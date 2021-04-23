(make-comint "psql" "psql" nil "--host=localhost" "--user=postgres")
(make-comint "psql" "d:/P/PostgreSQL/9.3/bin/psql" nil "--host=localhost" "--user=postgres")
(make-comint "psql" "d:/P/PostgreSQL/9.3/bin/psql" nil "--help")


(setq comint-prompt-regexp "^.*?=#")
(setq comint-use-prompt-regexp t)
