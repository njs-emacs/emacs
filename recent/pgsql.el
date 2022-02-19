(defun psql (cmd &optional buffer)
  (let ((shell-cmd (format "psql -h localhost -U postgres -d emacshist -c \"%s\"" cmd)))
    (cond 
     ((stringp buffer) (ps cmd (make-buffer buffer)))
     ((numberp buffer) (ps cmd (format "*ps-%d*" buffer)))
     ((shell-command shell-cmd buffer))
     ))
  )

(defun psql (cmd)
  (let ((shell-cmd (format "psql -h localhost -U postgres -d emacshist -c \"%s\"" cmd)))
    (shell-execute-text shell-cmd) 
    )
  )


