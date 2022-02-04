;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun t32 (x)
  (let* ((s (secure-hash 'sha256
			(format-time-string "%c")
			))

(setq s (string-to-list (secure-hash 'sha256
			(format-time-string "%c")
			nil nil t)))
(base64-encode-string 

 'concat (mapcar 'char-to-string s))

(defun fake-base26 (s)
  (let* ((clist (string-to-list s))
	 (digits "abcdefghijklmnopqrstuvwxyz")
	 (dlist (string-to-vector digits)) 
	 (n (length digits))
	 )
    (apply 'concat (mapcar '(lambda (x)
			      (char-to-string (aref dlist (% x n)))) clist))
    )
  )

(fake-base26 "\000\001")
(fake-base26 (secure-hash 'sha256 (format-time-string "%c.%6N") nil nil t))

