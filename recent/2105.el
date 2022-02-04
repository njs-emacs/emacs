(defun ts-sub-ident (&optional arg)
  (format-time-string "_%m%d_%H%M%S")
  )

(defun ts-dwim (&optional arg)
  (interactive "P")
  (let (start end)
    (sx
     (cond
      ((sx (bol)
	   (rsf "^sub \\(_[0-9]\\{4\\}_[0-9]\\{6\\}\\)" (point$))
	   )
       (goto-char (mb 1))
       (delete-region (mb 1) (me 1))
       (insert (ts-sub-ident))
       )
      )
     )
    )
  )

(def-key global-map (kbd "<S-f7>") 'ts-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun left-substring (s len)
  (cond ((< len 0)
	 (left-string s (+ (length s) len))
	 )
	((substring s 0 len))
	)
  )

(defun right-substring (s len)
  (let ((slen (length s)))
    (cond ((< len 0)
	   (right-string s (+ slen len))
	   )
	  ((substring s (- slen len) slen))
	  )
    ))

(fset 'lss 'left-substring)
(fset 'rss 'right-substring)

