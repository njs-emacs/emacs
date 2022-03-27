(defun look-rsf (s &optional limit n beg)
  (let* ((ok (looking-at s))
	 )
    
    (cond
     ((and ok (rsf s limit nil n beg)))
     )
    )
  )

(defun look-rsf-match (s &optional limit n beg)
  (let* ((ok (look-rsf s limit n beg))
	 )
    
    (cond
     (ok (ms (or n 0))))
    )
  )

(defun look-or-rsb (pat)
  (or (looking-at pat) (rsb pat))
  )

(defun read-as-list (s) (eval (read (format "`(%s)" s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-buffer-forms (&optional start end)
  (let* ((start (or start (region-dwim-beginning)))
	 (end (or end (region-dwim-end)))
	 list
	 )
    (goto-char start)
    (catch 'done
      (while (< (point) end)
	(let ((sexp (condition-case nil
			(read (current-buffer))
		      (end-of-file (throw 'done nil)))))
	  (setq list (cons sexp list))
	  )
	)
      )
    (nreverse list)
    )
  )

(defun sort-region-stuff (fun &optional start end)
  (let* ((start (or start (region-dwim-beginning)))
	 (end (or end (region-dwim-end)))
	 (list (read-buffer-forms start end))
	 )
    (setq list (sort list fun))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun region-or-thing ()
  (cond ((region-active-p) (region-text))
	((thing-at-point 'sexp))
	)
  )

(defun region-size ()
  (cond
   ((region-active-p) (- (region-end) (region-beginning)))
   (0)
   ))
    

(defun region-or-thing-region ()
  (cond
   ((> (region-size) 0) (cons (region-beginning) (region-end)))
   ((bounds-of-thing-at-point 'sexp))
;   ((sx (cons (sxp (beginning-of-sexp)) (sxp (end-of-sexp)))))
   )
  )

(defun region-or-thing-adorn (l &optional r)
  (let* ((zone (region-or-thing-region))
	 (r (or r l))
	 )
    (sx
     (goto-char (cdr zone)) (insert r)
     (goto-char (car zone)) (insert l)
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kdf-sort (&optional start end)
  (interactive "r")
  (let ((sorted (sort-region-stuff
		 '(lambda (x y)
		    (string< (prin1-to-string (nth 3 x)) (prin1-to-string (nth 3 y)))) start end)))
    (apply 'show (mapcar '(lambda (x) (printf "%s\n" x)) sorted))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun git-call (&rest args)
  (call-shell
   (format "%s %s" vc-git-program (mconcat args " "))
   )
;  (apply 'call-process vc-git-program nil nil nil args)
  )
