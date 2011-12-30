;(setq write-file-hooks nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; there is a weakness to this system in that the latest file is not backed up
;; which means that if the file gets renamed or moved, there is no copy
;; of the latest version
;; now that md5 checksums are stored in the logs, it is possible to correlate
;; a moved latest file with its old location
;;
;; format of log file changed 090407-104510
;; now includes visited files, and is coded as 'V' for visit 'S' for save, replacing 'F' type

(setq nvc-remote-ntfs t)

(setq nvc-root (filename-format "%s/emacs" backup-root))
(setq nvc-shadow-root (filename-format "%s/shadow" backup-root))

(make-variable-buffer-local 'nvc-method)

(set-default 'nvc-method 'nvc-file-copy-once)

(setq nvc-disable-file-name-pattern-list
  (list
   "/ext/[^/]*\\.h$"
   )
  )

(defun nvc-file-enable (file)
  (not (member-if '(lambda (x) (string-match x file))
		  nvc-disable-file-name-pattern-list)))

; ##limit## 

;
; {##080410-152617 allow ##backup## to override quoted ##nobackup## later #}
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun file-reverted-contents ()
  (cond
   ((file-exists-p (buffer-file-name))
    (let* ((name (buffer-file-name))
	   (buf-b (get-buffer-create (concat (buffer-name) "***")))
	   (str-b)
	   )
    (save-excursion
      (set-buffer buf-b)
      (erase-buffer)
      (insert-file-contents name)
      (setq str-b (buffer-string))
      (kill-buffer buf-b)
      )
    str-b
    )
    )
   )
  )

;; no longer needed 
(defun file-really-changed ()
  (let* ((str-b (file-reverted-contents))
	 (str-a (buffer-string))
	 )
    (not (string-equal str-a str-b))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; trying to resolve issue with e:/xx and //host/e/xx

(defun nvc-name-info (file)
; returns (HOST PATH NAME)
  (let ((x
	 (cond
	  ((string-match "^/home/nick/.gvfs/\\(\\w+\\) on \\(\\w+\\)/" file)
	   (list
	    (substring file (match-beginning 2) (match-end 2))
	    (concat-path
	     (substring file (match-beginning 1) (match-end 1))
	     (substring file (match-end 0))
	     )))
	   
	  ((string-match "^//\\(\\w*\\)" file)
	   (list
	    (substring file (match-beginning 1) (match-end 1))
	    (substring file (match-end 0))))
	  (t
	   (list
	    (downcase system-name)
	    (string-sub file ":" "/")
	    )
	   )
	  )
	 ))
    (list (car x)
	  (filename-clean (file-name-directory (cadr x)))
	  (file-name-nondirectory (cadr x)))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nvc-daily-dirname (&optional time)
  (filename-clean (filename-format "%s/%s" nvc-root (format-time-string "%y/%y%m/%y%m%d" time)))
  )

(defun nvc-dirname (&optional file time)
  (let* ((file (or file (buffer-file-name)))
	 (info (nvc-name-info file))
	 (dir (nvc-daily-dirname time))
	 )
    (filename-clean (filename-format "%s/%s/%s" dir (car info) (cadr info)))
    ))

(defun nvc-filename (file &optional time prefix)
  (let* ((date-string (format-time-string "%y%m%d" time))
	 (time-string (format-time-string "%H%M%S" time))
	 (name (file-name-nondirectory file))
	 (dir (nvc-dirname file time))
	 )
    (make-directory dir t)
    (or prefix (setq prefix ""))
    (format "%s%s@%s%s-%s%s"
	    dir (basename name) prefix date-string time-string
	    (or (file-name-suffix name) ""))
    ))

;;; shadow file added to solve problem of files being renamed
;;; the shadow file is never deleted or moved, but is overwritten
;;; every time

(defun nvc-shadow-file (file)
  (let* ((info (nvc-name-info file))
	 (dir (filename-clean (filename-format "%s/%s/%s" nvc-shadow-root (car info) (cadr info))))
	 (name (file-name-nondirectory file))
	 (shadow-file (filename-format "%s/%s" dir name))
	 )
    (make-directory dir t)
    (write-region (point-min) (point-max) shadow-file)
    ))

; cannot set file time on windows machine
; cannot set file-modes on windows machine

(defun nvc-file-copy-once (file &optional time prefix)
  (nvc-shadow-file file)
  (let* ((file-b (nvc-filename file time prefix)))
    (cond
     (nvc-remote-ntfs
      (and file-b (copy-file file file-b nil nil))
      )
     (t 
      (and file-b (copy-file file file-b nil t))
      (set-file-modes file-b 292)
      )
     )
    )
  nil
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nvc-copy-for-deletion (file)
  (nvc-file-copy-once file nil "deleted-")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nvc-file (file &optional time prefix)
  (funcall nvc-method file time prefix)
  )

; # {#~080414-110701 don't write log activity to log - no other exceptions expected #}

(defun localize-file-name (file)
  (let ((f (downcase file)))
    (cond
     ((string-match "^//\\(\\w*\\)/\\(\\w*\\)" f)
      (let* ((host (substring f (match-beginning 1) (match-end 1)))
	     (drive (substring f (match-beginning 2) (match-end 2)))
	     )
	(cond
	 ((string= host (downcase system-name))
;;;;;;;;;;;; turn network share back into drive
; not working yet
;	  (format "%s:%s" drive (substring file (match-end 2)))

	  file
	  )
	 (file)
	 )
	)
      )
     (file)
     )
    )
  )

(defun write-file-and-refresh-if-visiting (file text)
  (write-string-to-file file text t)
  (let ((buf (find-buffer-visiting file)))
    (cond
     (buf (save-excursion
	    (set-buffer buf)
	    (revert-buffer nil t t)
	    (setq buffer-read-only t)
	    ))))
  )

(defun write-file-and-refresh-if-visiting-fake (file text)
  (let ((buf (find-buffer-visiting file)))
    (cond
     (buf (save-excursion
	    (set-buffer buf)
	    (setq buffer-read-only nil)
	    (eob)
	    (insert text)
	    (set-buffer-modified-p nil)
	    (setq buffer-read-only t)
	    ))))
  (write-string-to-file file text t)
  )

(defun write-log-files (text)
  (let ((coding-system-for-write 'raw-text-unix))

;  (write-file-and-refresh-if-visiting (filename-format "%s/emacs.log" nvc-root) text)
  (write-file-and-refresh-if-visiting-fake (filename-format "%s/emacs.log" nvc-root) text)
  (let* ((d (nvc-daily-dirname))
		 (f (filename-format "%s/.log" d))
	 )
    (or (file-exists-p d) (make-directory d t))
    (cond ((file-exists-p f))
	  ((write-string-to-file f "##version:2##\n##nologvisit##\n##nobackup##\n"))
	  )
    (write-file-and-refresh-if-visiting f text)
    )
  ))

(defun nvc-log-write-save (file &optional time md5-old md5-new)
  (cond
   (nvc-no-log-visit)
   ((let*
       ((log-string
	 (format "%-12s S -------- %s\t%s %s %s\n"
		 (format-time-string "%y%m%d-%H%M%S" time)
		 (downcase (system-name))
		 (localize-file-name file)
		 md5-old
		 md5-new
		 ))
	)
     (write-log-files log-string)
     )
   ))
  )

(defun nvc-file-write-force () (interactive)
  (let* ((file (buffer-file-name))
	 (time (current-time))
	 (new (buffer-string))
	 (old (file-reverted-contents))
	 (md5-new (md5 new))
	 (md5-old (md5 (or old "")))
	 )
    (and
     file
     (file-exists-p nvc-root)
     (nth 0 (file-attributes nvc-root))
     (cond
      ((file-exists-p file)
       (nvc-log-write-save file time md5-old md5-new)
       (nvc-file file time)
       )
      )
     ))
  nil)

(defvar nvc-enable-force nil "If non-nil, override any buffer-specific backup disable directives.
Used to enter a file into edit tracking on creation but not at any other time.")

(defun nvc-file-write-hook ()
  (let* ((file (buffer-file-name))
	 )
    (and
     file
     nvc-global-enable
     nvc-enable
     (file-exists-p nvc-root)
     (nth 0 (file-attributes nvc-root))
     (or
      nvc-enable-force
      (and 
       (nvc-buffer-enable (current-buffer))
       (nvc-file-enable file)
       )
      )
     (let* ((time (current-time))
	    (new (buffer-string))
	    (old (file-reverted-contents))
	    (md5-new (md5 new))
	    (md5-old (md5 (or old "")))
	    (changed (not (string= md5-new md5-old)))
	    (exists (file-exists-p file))
	    )
       (cond (changed
	      (prog1 t (nvc-log-write-save file time md5-old md5-new))
	      (and exists
		(nvc-file file time)
		)
	      ))
       )
     ))
  nil)

(setq nvc-global-enable t)
;(setq nvc-global-enable nil)

(setq nvc-enable t)

(make-variable-buffer-local 'nvc-enable)
(make-variable-buffer-local 'nvc-no-log-visit)

(add-hook 'write-file-functions 'nvc-file-write-hook)

; when nvc is disabled, the intermediate versions between the
; last backed up version and the current one are lost

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; log file visit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nvc-no-log-visit ()
  ;; the nologvisit tag must be in first 1000 characters
  (sx
   (bob)
   (let* ((limit (min (point-max) 1000))
	  (limit (or (sx (rsf "##limit##" limit)) limit))
	  )
     (cond
      ((sx (rsf "##nologvisit##" limit)) t)
      ((file-exists-p ".nologvisit"))
      )
     )
   )
  )

(defun nvc-log-visit* (file &optional time)
  (let*
      ((log-string
	(format "%-12s V -------- %s\t%s\n"
		(format-time-string "%y%m%d-%H%M%S" time)
		(downcase (system-name))
		file))
       )
    (write-log-files log-string)
   )
  )

(defun nvc-log-visit ()
  (setq nvc-no-log-visit (nvc-no-log-visit))
  (cond
   (nvc-no-log-visit)
   (nvc-global-enable
    (nvc-log-visit* (buffer-file-name) (current-time)))
   )
  )

(add-hook 'find-file-hooks 'nvc-log-visit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; {#~* (emacs) interactive arg reader function #}

(defun days-ago (now n)
  (cons (- (car now) n) (nthcdr 1 now))
  )

(defun nvc-query-args ()
  (list (read-from-minibuffer
	 "Spec: "
	 (buffer-file-name) nil nil '(file-name-history . 1))
	(read-from-minibuffer
	 "From: "
	 (format-time-string "%y%m%d" (days-ago (current-time) 10)) nil nil '(nvc-dired-history . 1))
	(read-from-minibuffer
	 "To:   "
	 (format-time-string "%y%m%d" (current-time)) nil nil '(nvc-dired-history . 1))
	)
  )

(defun nvc-dired (date)
  (interactive (list (read-from-minibuffer
		      "Backup Date: "
                                 (format-time-string "%y%m%d" (current-time))
				 nil
				 nil
                                 '(nvc-dired-history . 1))))
  (let ((dir (filename-clean
	      (filename-format "%s/%s/%s/%s"
		      nvc-root
		      (downcase system-name)
		      date
		      (string-sub default-directory ":" "/")
		      ))))
    (dired dir)
    )
  )

(defun nvc-query (&optional start end) (interactive)
  (let* ((file (buffer-file-name))
	 (args (arg-expand '(start end)))
	 (cmd (format "perl -I%s/.meta %s/.meta/backup-ls.pl -target=\"%s\" %s"
		      backup-root backup-root file args))
	 )
    (compile cmd))
  )

(defun nvc-query-i (target start end)
  (interactive (nvc-query-args))
  (let* ((args (arg-expand '(start end)))
	 (cmd (format "perl -I%s/.meta %s/.meta/backup-ls.pl -target=\"%s\" %s"
		      backup-root backup-root target args))
	 )
    (compile cmd))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nvc-ls (target &rest args)
  (compile
   (format "perl %s/.meta/backup-ls.pl -target=%s %s" backup-root target (mconcat args " ")))
  )

(defun nvc-ls-bdiff (target &optional start end)
  (interactive (nvc-query-args))
  (or end (setq end "now()"))
  (or start (setq start "'1970-01-01'"))
  (let* ((cmd
	  (format
	   "perl %s/.meta/backup-ls.pl -format=diff --target=\"%s\" --cmd=between --whenstart=%s --whenend=%s"
	   backup-root target start end)
	  )
	 (s (call-shell cmd))
	 )
    (printf "%s\n" cmd)
    (bdiff s (list 'call-shell cmd))
    )
  )

(defun nvc-ls-bdiff-new (target &optional cmd specs)
  (interactive)
  (or cmd (setq cmd "month"))
  (or specs (or specs ""))
  (let* ((cmd
	  (format
	   "perl %s/.meta/backup-ls.pl -format=diff --target=\"%s\" cmd=%s %s"
	   backup-root target cmd specs)
	  )
	 (s (call-shell cmd))
	 )
    (printf "%s\n" cmd)
    (bdiff s (list 'call-shell cmd))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nvc-grep-args ()
  (list
   (read-from-minibuffer
    "Pattern: "
    "" nil nil '(file-name-history . 1))
   (read-from-minibuffer
    "Spec: "
    (buffer-file-name) nil nil '(file-name-history . 1))
   (read-from-minibuffer
    "From: "
    (format-time-string "%y%m%d" (days-ago (current-time) 10)) nil nil '(nvc-dired-history . 1))
   (read-from-minibuffer
    "To: "
    (format-time-string "%y%m%d" (current-time)) nil nil '(nvc-dired-history . 1))
   )
  )

(defun nvc-grep (pat &optional fpat start end)
  (interactive (nvc-grep-args))
  (let* ((cmd (format "perl %s/.meta/backupgrep.pl %s" backup-root
		    (arg-expand '(pat fpat start end))))
	 )
    (compile cmd)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nvc-buffer-enable (file)
  (sx (bob)
      (let* ((limit (min (point-max) 1000000))
	     (limit (or (sx (rsf "##limit##" limit)) limit))
	     )
	(or
	 (sx (rsf "##backup##" limit))
	 (not (rsf "##\\(generated\\|nobackup\\)##" limit))
	 (file-exists-p ".nobackup")
	 )
	)
      )
  )

(defun nvc-buffer-attribute (file)
  (sx (bob)
      (let* ((limit (min (point-max) 1000000))
	     (limit (or (sx (rsf "##limit##" limit)) limit))
	     )
	(cond
	 ((sx (rsf "##backup:\\(.*\\)##" limit)) (ms 1))
	 ((sx (rsf "##\\(generated\\|nobackup\\)##" limit)) "no")
	 )
	)
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;
;
;
;;;

(setq alternate-key-map '(keymap))
(define-key global-map "\C-x\C-a" alternate-key-map)

(defun nvc-no-backup-save () (interactive)
  (let ((nvc-enable nil))
    (save-buffer)
    )
  )

(define-key alternate-key-map "\C-x\C-s" 'nvc-no-backup-save)
(define-key alternate-key-map "\C-x\C-b" 'nvc-file-write-force)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun localize-buffer-file-name ()
  (set-visited-file-name (localize-file-name (buffer-file-name)))
  (set-buffer-modified-p nil)
  )

(add-hook 'find-file-hooks 'localize-buffer-file-name)
