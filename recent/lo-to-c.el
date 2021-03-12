(defun lo-to-c (s)
  "Replace c symbol with corresponding lisp symbol.
Reverse direction if symbol is already a lisp symbol."
  (cond ((string-match "_" s)
	 (setq s (replace-regexp-in-string "_" "-" s))
	 (setq s (replace-regexp-in-string "^[FQV]" "" s))
	 (setq s (replace-regexp-in-string "[FQV]$" "" s))
	 )
	(t (setq s (replace-regexp-in-string "-" "_" s)))
	)
    s)

(defun lo-to-c-symbol-at-point ()
  "Perform lo-to-c on symbol at point. Copy result as kill."
  (interactive "")
  (let* ((s (symbol-at-point))
	 (ss (lo-to-c (symbol-name s)))
	 )
    (kill-new ss)
    (message "Copied to kill ring: %s" ss)
    )
  )

(defun lo-to-c-symbol-replace ()
  "Replace symbol-at-point with its c/lisp equivalent."
  (interactive)
  (replace-thing-at-point-fun 'symbol 'lo-to-c)
  )
