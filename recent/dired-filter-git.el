;;; ##magic-annotation## can we pick these comments up and transfer
;;; them to org files
;;;
;;; this code is intended to use dired-filter to restrict the dired view
;;; to just files that are modified with respect to git
;;;
;;; it also attempts to create a sorted list of modified files
;;; in modtime order that we can use to process the updates
;;; in file time order
;;;
;;; we have also created a perl script in e:/.p/bin/gitu.pl
;;; which can also provide custom git status reports we can
;;; also use to aid checking in a group of changes
;;;
;;; without reference to _backup files (which are inaccurate anyway)
;;; it won't be possible to separate multiple changes to the same file
;;; they can be checked in separately, but the time order is lost as
;;; newer modifications will shadow older ones.

(defun dired-predicate-git-unstaged (name)
  (debug)
  )

(defun git-ls-read (s)
  (let ((state (substring s 0 1))
	(file (expand-file-name (substring s 2))))
    (cond ((string= state "?") (setq state "U")))
    (list file (intern (downcase state)) (file-attributes file))
    )
  )

; the sort happens to allow 'r' to override 'c'
; but that shouldn't be neccessary if we use gitu.pl etc

(defun git-state (&optional flags)
  (let* ((flags (or flags "-todm"))
	 (s (shell-execute-text (format "git ls-files %s | sort" flags)))
	 (ss (unconcat-lines s))
	 o
	 )
    (dolist (i ss)
     (let ((ii (git-ls-read i)))
;       (debug)
	(setq o (alist-put o (car ii) (cdr ii)))
	)
      )
     
    o
    )
  )

(defun dired-filter-git-state-cache ()
  (setq dired-filter-git-state-cache (git-state))
  )

(defun dired-filter-git-state (file)
  (cdr (assoc file dired-filter-git-state-cache))
  )

(defun dired-filter-git-state-check (file state)
  (let ((fstate (dired-filter-git-state file)))
    (eq fstate state)
    )
  )

(defun dired-filter-git-state-cache-sort ()
  (setq dired-filter-git-state-cache
    (sort dired-filter-git-state-cache
	  '(lambda (a b)
	     (let* ((fun '(lambda (x) (alist-get x `((u . 1) (c . 1) (r . 0)))))
		    (aa (funcall fun (nth 1 a)))
		    (bb (funcall fun (nth 1 b)))
		    )
	       (cond
		((< aa bb))
		((and (= aa bb)
		      (time-less-p
		       (file-attribute-modification-time (nth 2 b))
		       (file-attribute-modification-time (nth 2 a))
		       )
		      )
		 )
		)
	       )
	     )
	  )
    )
  )

(defun git-state-insert (state)
  (let* ((list
	  (mapcar
	   '(lambda (x)
	      (format "%s %s %s" (nth 1 x) (format-time-string "%y%m%d-%H%M%S" (file-attribute-modification-time (nth 2 x))) (nth 0 x))) state)))
    (dolist (i list)
      (insert i "\n")
      )
    )
  )

; (git-state-insert dired-filter-git-state-cache)
