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

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(setq nvc-remote-ntfs t)

(defun backup-path (s) (filename-concat backup-root s))

(setq nvc-root (filename-format "%s/emacs" backup-root))
(setq nvc-shadow-root (filename-format "%s/shadow" backup-root))

(make-variable-buffer-local 'nvc-method)

(set-default 'nvc-method 'nvc-file-copy-once)

(setq nvc-disable-file-name-pattern-list
  (list
   "/ext/[^/]*\\.h$"
   ".emacs-bmk-bmenu-state.el"
   "e:/home/nick/.emacs"
   "\.bmk"
   )
  )

(defun nvc-file-enable (file)
  (not
   (member-if '(lambda (x) (string-match x file))
	      nvc-disable-file-name-pattern-list)))

; ##limit## 

;
; {##080410-152617 allow ##backup## to override quoted ##nobackup## later #}
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reverted-buffer-md5 ()
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
      (prog1 (md5 buf-b)
	(kill-buffer buf-b)
	)
      )
    )
    )
   (t (md5 ""))
   )
  )

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

(defun nvc-shadow-file-name (&optional file)
  (let* ((file (or file (buffer-file-name)))
	 (info (nvc-name-info file))
	 (dir (filename-clean (filename-format "%s/%s/%s" nvc-shadow-root (car info) (cadr info))))
	 (name (file-name-nondirectory file))
	 (shadow-file (filename-format "%s/%s" dir name))
	 )
    shadow-file
    ))

(defun nvc-shadow-file (file)
  (let* ((shadow-file (nvc-shadow-file-name file))
	 (dir (file-name-directory shadow-file))
	 )
    (with-suppressed-message
      (make-directory dir t)
      (write-region (point-min) (point-max) shadow-file)
      )
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
	    (let ((read-only buffer-read-only))
	      (revert-buffer nil t t)
	      (setq buffer-read-only read-only)
	      )
	    ))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro inhibit-read-only (&rest stuff)
  `(let ()
     (and buffer-read-only (read-only-mode -1))  ; clear if set
     (prog1 (progn ,@stuff) 
       (read-only-mode)
       )
     )
  )

(defun bup-reverse-refresh ()
  (interactive)
  (let* ((n 1000))
    (inhibit-read-only
     (erase-buffer)
     (shell-command (format "tac %s | head -%s" (buffer-file-name) n) (current-buffer))
     (set-buffer-modified-p nil)
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode bup-reverse-mode "Show files in reverse order"
  :init-value nil
  :lighter " R"
;  (debug)
  (cond
   (bup-reverse-mode
    (bup-reverse-refresh)
    (beginning-of-buffer)
    )
   (t
    (inhibit-read-only (revert-buffer t t))
    (end-of-buffer)
    )
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-file-and-refresh-if-visiting-fake (file text)
  (let ((buf (find-buffer-visiting file)))
    (cond
     (buf
      (save-excursion
	(set-buffer buf)
	(inhibit-read-only
	 (cond
	  (bup-reverse-mode
	   (bob)
	   (insert text)
	   )
	  (t 
	   (eob)
	   (insert text)
	   )
	  )
	 )
	(set-buffer-modified-p nil)
	)
      )))
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
       ((tag (file-unique-meta-tag))
	(log-string
	 (format "%-12s S -------%s %s\t%-40s    %s %s %s\n"
		 (format-time-string "%y%m%d-%H%M%S" time)
		 (if tag "T" "-")
		 (downcase (system-name))
		 (localize-file-name file)
		 md5-old
		 md5-new
		 (or tag "")
		 ))
	)
     (write-log-files log-string)
     )
   ))
  )

(defun nvc-file-write-force () (interactive)
  (let* ((file (buffer-file-name))
	 (time (current-time))
	 (md5-new (md5 (current-buffer)))
	 (md5-old (reverted-buffer-md5))
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
     (nth 0 (file-attributes nvc-root))		; is file
     (or
      nvc-enable-force
      (and 
       (nvc-buffer-enable)
       (nvc-file-enable file)
       )
      )
     (let* ((time (current-time))
	    (md5-new (md5 (current-buffer)))
	    (md5-old (reverted-buffer-md5))
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
(defun nvc-no-log-visit-tagged ()
  ;; the nologvisit tag must be in first 1000 characters
  (sx
   (bob)
   (let* ((limit (min (point-max) 1000))
	  (limit (or (sx (rsf "##limit##" limit)) limit))
	  )
     (cond
      ((sx (rsf "##nologvisit##" limit)) t)
      ((locate-up-file-directory ".nologvisit"))
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

(defun nvc-is-backup-file (name)
  (let ((case-fold-search t))
    (cond
     ((string-match-p "e:/_backup/shadow" name))
     ((string-match-p "e:/_backup/emacs/[0-9]" name))
     )
    )
  )

(defun nvc-log-visit ()
  (let* ((name (buffer-file-name))
	 (no-log (or nvc-no-log-visit (nvc-no-log-visit-tagged))))
    (cond
     (no-log)
     (nvc-global-enable
      (nvc-log-visit* name (current-time)))
     )
    (cond
     ((nvc-is-backup-file name)
      (setq buffer-read-only t)
      (message "Setting read-only on shadow file %s" name)
      )
     )
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
(setq nvc-confirm-size-limit 1000000)

(defun nvc-confirm-maybe ()
  (let* ((size (buffer-size)))
    (and
     (or (< size nvc-confirm-size-limit)
	 (y-or-n-p (format "Buffer size is %.1f MB, still backup? " (/ size 1000000.0)))
	 )
     )
    )
  )

(defun nvc-buffer-enable ()
  (sx (bob)
      (let* ((limit (min (point-max) 1000000))
	     (limit (or (sx (rsf "##limit##" limit)) limit))
	     (name (buffer-file-name))
	     )
	(or
	 (sx (rsf "##backup##" limit))
	 (and
	  (not (rsf "##\\(generated\\|nobackup\\)##" limit))
	  (not (locate-up-file-directory ".nobackup"))
	  (nvc-confirm-maybe)
	  )
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
  (let ((name (buffer-file-name)))
    (cond
     (name
      (set-visited-file-name (localize-file-name name))
      (set-buffer-modified-p nil)
      )
     )
    )
  )

(add-hook 'find-file-hooks 'localize-buffer-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun file-unique-meta-tag ()
  (let ((f (locate-up-file ".meta"))
	s ss)
    (cond
     (f
      (setq s (file-contents f))
      (setq ss (string-match "tag:\\s *\\(.*\\)" s))
      (match-string 1 s)
      )
     )
    )
  )

; disable for now

(defun file-unique-meta-tag ())
