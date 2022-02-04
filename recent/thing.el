(defun replace-thing-at-point-fun (thing fun)
  (let* ((it (thing-at-point thing))
	 (bounds (bounds-of-thing-at-point thing))
	 (new (funcall fun it))
	 )
    (kill-region (car bounds) (cdr bounds))
    (insert new)
    )
  )

;;; there doesn't seem toi be any reliable code that will
;;; tell you which element in an object you are looking at
;;; these solutions don't like when you are inside a string
;;;

(defun point-inside-string-p (&optional point begin)
  (let* ((begin (or begin (point^)))
	 (point (or point (point)))
	 (state (parse-partial-sexp begin point))
	 )
    (nth 3 state)
    )
  )

(defun goto-beginning-of-sexp (&optional arg)
  (interactive "P")
  (cond
   ((point-inside-string-p)
    (rsb "\"")
    )
   (t 
    (thing-at-point--end-of-sexp)
    (thing-at-point--beginning-of-sexp)
    )
   )
  (cond
   (arg (forward-sexp arg)
	(rsf "\\s +")
	)
   )
  (point)
  )

(def-key c-semicolon-map (kbd "C-.") 'goto-beginning-of-sexp)

(defun sexp-where ()
  (sx   
   (let* ((p (goto-beginning-of-sexp))
	  (state (parse-partial-sexp (point^) p))
	  (n 0)
	  )
     (goto-char (1+ (nth 1 state)))
    (while (< (point) p)
       (goto-beginning-of-sexp 1)
       (setq n (1+ n))
       )
    n)
   )
  )
