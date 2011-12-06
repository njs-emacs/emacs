(defun buffer-replace (start-tag end-tag s &optional search)
  "Replace text between end of START and beginning of END
with NEWTEXT. If either START or END not found, then fail."
  (or search (setq search 'search-forward))
  (save-excursion
    (let* ((start (and (funcall search start-tag nil t) (match-data)))
	   (end (and (funcall search end-tag nil t) (match-data))))
      (cond
       ((and start end)
	(kill-region (nth 1 start) (nth 0 end))
	(goto-char (nth 1 start))
	(insert s)
	t)
       ))))

(defun buffer-replace-lines (start-tag end-tag s &optional search)
  "Replace text between line containing STARTEXP and line containing ENDEXP
with NEWTEXT. If either STARTEXP or ENDEXP not found, then fail.
If STARTEXP is nil, start on next line."
  (or search (setq search 'search-forward))
  (save-excursion
    (let* ((start (cond
		   (start-tag (and (funcall search start-tag nil t)
				   (sxp (fl 1))))
		   ((sxp (fl 1)))))
	   (end (cond
		 (end-tag (and (funcall search end-tag nil t)
			       (sxp (bol))))
		 ((sxp (fl 1)))))
	   )
      (cond
       ((and start end)
	(and (not (eq start end)) (kill-region start end))
	(goto-char start)
	(insert s)
	(or (bolp) (insert "\n"))
	t)
       ))))
