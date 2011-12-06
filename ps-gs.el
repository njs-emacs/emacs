(setq ps-shell "d:/g/gs/gs8.51/bin/gswin32c.exe")
(setq ps-process nil)

(defun psh (arg) (interactive "P")
  (let ((p (start-process "psh" nil ps-shell)))
    (process-send-string p (buffer-string))
;    (process-send-eof p)
    (set-process-filter p '(lambda (proc string)
			     (setq psh-out (concat psh-out string))))
    (setq psh-out "")
    (cond (arg (set-process-sentinel p '(lambda (proc state) (show psh-out)))))
    )
  )

(defun ps-start ()
  (or (and (processp ps-process) (eq (process-status ps-process) 'run))
      (setq ps-process (start-process "psh" "*psh*" ps-shell "-q"))
      )
  (set-process-filter
   ps-process
   '(lambda (proc string)
      (debug)
      (set-buffer (process-buffer proc))
      (setq string (replace-regexp-in-string "GS<[0-9]*>" "" string))
      (setq string (replace-regexp-in-string "GS>" "" string))
      (insert string)
      )
   )
  ps-process
  )

(defun ps-eof ()
  (process-send-eof ps-process)
  )

(defun ps-send (s)
  (process-send-string ps-process s)
  )

(defun ps-eval-buffer () (interactive)
  (ps-start)
  (ps-send (buffer-string))
  )
