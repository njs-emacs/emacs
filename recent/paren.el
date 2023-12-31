(defun insert-paren-around (chars &optional start end)
  (setq start (or start (point)))
  (setq end (or end start))
  (sx (goto-char end) (insert (cdr chars))
      (goto-char start) (insert (car chars))
      ))

(defun insert-paren-dwim (&optional c)
  (interactive "c")
 ;  (debug)
  (let* ((chars
	  (cond
 	   ((or (= c ?\") (= c ?q)) '(?" . ?"))
 	   ((or (= c ?<) (= c ?a)) '(?< . ?>))
 	   ((or (= c ?!) (= c ?x)) '(?! . ?!))
 	   ((or (= c ?{) (= c ?c)) '(?{ . ?}))

	   ((or (= c ?\') (= c ?b)) '(?' . ?'))
	   ((or (= c ?/) (= c ?f)) '(?/ . ?/))
	   ((or (= c ?s) (= c ?\[)) '(?[ . ?]))
	   (t '(?( . ?)))
	   )
	  )
	 start end)
    (cond
     ((region-active-p)
      (insert-paren-around chars (region-beginning) (region-end))
      )
     ((looking-at "^$")
      (insert-paren-around chars)) 
     ((eolp)
      (cond
       ((looking-back "\\s ")
	(insert-paren-around chars)
	)
       (t
	(sx (fc -1) (insert-paren-dwim c))
	)
      ))
      ((looking-at "\\s ")
       (insert-paren-around chars)
       )
     (t (sx (goto-beginning-of-sexp) 
	    (insert-paren-around chars (point)
				 (sxp (thing-at-point--end-of-sexp))))
	)
     )
    )
  )

(def-key global-map (kbd "s-o") 'insert-paren-dwim)
