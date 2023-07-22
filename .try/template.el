(defvar template-find-alist nil "")

(defun emacshist-query (s)
  "Execute query S on the emacshist database and return the trimmed output text."
  (let ((text (shell-execute-text (format "psql -n -t -d emacshist -U postgres -h boo --command=\"%s\"" s))))
    (strip-white text)
    )
  )

(defun eh-latest (file)
  "Query the last saved file with name NAME."
  (emacshist-query (format "select file from log where name = '%s' and type = 'S' order by time desc limit 1" file))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq template-find-alist nil)

(defun template-get-template-function (&optional buffer)
;; 
  "(incomplete) Return the function which is used to find the template filename for BUFFER."
  (let* ((path (buffer-file-name buffer))
	 (name (file-name-nondirectory path))
	 )
    (alist-get name template-find-alist nil nil 'string=)
    )
  )

(defun template-contents (&optional buffer)
  (interactive)
  (let* ((fun (template-get-template-function buffer))
	 (last (funcall fun buffer))
	 )
  (file-contents last)
  ))

(defun template-insert (&optional arg)
  (interactive "P")
  (let* ((contents (template-contents (current-buffer))))
    (insert (format "%s %s" (buffer-name) contents))
    (insert contents)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun template-matching-file (buffer)
  (let* ((path (buffer-file-name buffer))
	 (name (file-name-nondirectory path))
	 (last (eh-latest name))
	 )
    last))
    

(setq template-find-alist `(("boo.pl" . template-matching-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(template-insert "boo.pl")

(emacshist-query "select file from log where type = 'S' order by time desc limit 1")
(emacshist-query "select file from log where name = 'boo.pl' and type = 'S' order by time desc limit 1")

(eh-latest "boo.pl")
(dregf "psql" elfs "ever")


(shell-execute-text "psql -n -t -d emacshist -U postgres -h boo --command=\"select count(*) from log\"")
(shell-execute-text "psql -n -t -d emacshist -U postgres -h boo --command=\"select path from log order by time desc limit 1\"")
(shell-execute-text "psql -n -t -d emacshist -U postgres -h boo --command=\"select file from log order by time desc limit 1\"")
(shell-execute-text "psql -n -t -d emacshist -U postgres -h boo --command=\"select file from log where type = 'S' order by time desc limit 1\"")
(shell-execute-text "psql -n -t -d emacshist -U postgres -h boo --command=\"select file from log where type = 'S' order by time desc limit 1\"")


