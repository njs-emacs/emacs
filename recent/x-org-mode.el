(defun org-x-make-file-link ()
  (cond
   ((region-active-p)
    (buffer-substring-no-properties (region-beginning) (region-end))
    )
   (t
    (sx (bol) (rsf "\\s *\\(.*\\)\\s *")
	(substring-no-properties (ms 1)))
    )
   )
  )

(defun org-x-delete-file-link ()
  (cond
   ((region-active-p)
    (kill-region (region-beginning) (region-end))
    )
   (t
    (sx (bol) (rsf "\\s *\\(.*\\)\\s *")
	(kill-region (mb 1) (me 1)))
    )
   )
  )

(defun org-insert-file-link ()
  (interactive)
  (let* ((file (org-x-make-file-link))
	 (path (expand-file-name file))
	 )
    (org-x-delete-file-link)
    (insert (format "[[file:%s][%s]]" path file))
    )
  )
