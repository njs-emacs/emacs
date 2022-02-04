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
(defun region-beginning-dwim (&optional pos) (or pos (region-beginning-if-active) (point-min)))
(defun region-end-dwim (&optional pos) (or pos (region-end-if-active) (point-max)))

(defun read-buffer-forms (&optional start end)
  (let* ((start (region-beginning-dwim start))
	 (end (region-end-dwim end))
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
  (let* ((start (region-beginning-dwim start))
	 (end (region-end-dwim end))
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

(defun dreg-guess-best-filespec ()
  (cond
   ((eq major-mode 'emacs-lisp-mode) elfs)
   ((eq major-mode 'nl-mode) "\\.nl$")
   ((eq major-mode 'c-mode) cfs)
   )
  )

(defun dreg-dwim (pat when where)
  (interactive
    (list
     (read-from-minibuffer "Pattern: " (region-or-thing))
     (completing-read "When: " '("ever" "year" "month" "week") nil t nil nil "ever")
     (read-from-minibuffer "Where: " (dreg-guess-best-filespec))
     )
    )
  (dregf pat where (read when))
  )

(def-key global-map (kbd "H-g H-g") 'dreg-dwim)
(def-key global-map (kps "78") 'dreg-dwim)

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
