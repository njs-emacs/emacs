;; Run ediff-action (ediff-files, ediff-merge, ediff-merge-with-ancestors)
;; on a pair of directories (three directories, in case of ancestor).
;; The third argument, REGEXP, is a regular expression that can be used to
;; filter out certain file names.
;; JOBNAME is the symbol indicating the meta-job to be performed.
(defun ediff-directories-internal (dir1 dir2 dir3 regexp 
					action jobname 
					&optional startup-hooks)
  ;; ediff-read-file-name is set to attach a previously entered file name if
  ;; the currently entered file is a directory. This code takes care of that.
  (setq dir1 (if (file-directory-p dir1) dir1 (file-name-directory dir1))
	dir2 (if (file-directory-p dir2) dir2 (file-name-directory dir2)))

  (if (stringp dir3)
      (setq dir3 (if (file-directory-p dir3) dir3 (file-name-directory dir3))))

  (cond ((string= dir1 dir2)
	 (error "Directories A and B are the same: %s" dir1))
	((and (eq jobname 'ediff-directories3)
	      (string= dir1 dir3))
	 (error "Directories A and C are the same: %s" dir1))
	((and (eq jobname 'ediff-directories3)
	      (string= dir2 dir3))
	 (error "Directories B and C are the same: %s" dir1)))

  (let (diffs ; var where ediff-intersect-directories returns the diff list
	file-list meta-buf)
    (setq file-list
      (ediff-file-list-filter-diff (ediff-intersect-directories 
				    jobname 'diffs regexp dir1 dir2 dir3)))
    (setq startup-hooks
	  ;; this sets various vars in the meta buffer inside
	  ;; ediff-prepare-meta-buffer
	  (cons (` (lambda ()
		     ;; tell what to do if the user clicks on a session record
		     (setq ediff-session-action-function (quote (, action)))
		     ;; set ediff-dir-difference-list 
		     (setq ediff-dir-difference-list (quote (, diffs)))))
		startup-hooks))
    (setq meta-buf (ediff-prepare-meta-buffer 
		    'ediff-filegroup-action
		    file-list
		    "*Ediff Session Group Panel"
		    'ediff-redraw-directory-group-buffer
		    jobname
		    startup-hooks))
    (ediff-show-meta-buffer meta-buf)
    ))

;;;

