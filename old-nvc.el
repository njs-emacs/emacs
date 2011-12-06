;#~:obsolete

;(setq write-file-hooks nil)

(defun find-parent-directory (name dir)
  (let (d)
    (and
     (not (string-match ":$" name))
     (setq d (file-name-directory name))
     (cond
      ((file-exists-p (concat d dir)) d)
      ((find-parent-directory (substring d 0 -1) dir))
      )
     )
    )
  )

(make-variable-buffer-local 'nvc-method)

(set-default 'nvc-method 'nvc-file-append)

(make-variable-buffer-local 'nvc-directory)

(set-default 'nvc-directory "_BACKUP")

(defun nvc-directory (&optional file)
  (let* ((file (or file (buffer-file-name)))
	 (dir (find-parent-directory file nvc-directory)))
    (and dir
	 (concat dir nvc-directory "/"
		 (substring file (length dir))))
    ))

(defun m26 (i) (concat (char-to-string (+ ?a (/ i 26)))
		       (char-to-string (+ ?a (% i 26)))))

(defun nvc-latest (&optional dir)
  (setq dir (or dir (nvc-directory)))
  (length
   (and (file-exists-p dir)
	(file-name-all-completions
	 (format-time-string "%y%m%d" (current-time)) dir)))
  )

(defun nvc-file-name (&optional s)
  (concat (format-time-string "%y%m%d" (current-time)) s))

(defun nvc-file-name (&optional s)
  (let ((x (current-time))) (format "%04x%04x" (car x) (cadr x)))
  )

(defun file-size (name)
  (let ((nokill (get-file-buffer name))
	(buffer (find-file-noselect name)))
    (cond (buffer
	   (prog1
	       (save-excursion (set-buffer buffer) (buffer-size))
	     (or nokill (kill-buffer buffer))
	     )))
    )
  )

(defun nvc-file-discrete (&optional file)
  (let* (name suffix dir new)
    (and
     (setq file (or file (buffer-file-name)))
     (setq dir (nvc-directory file))
     (file-exists-p file)
     (progn
       (setq name (file-name-nondirectory file))
       (setq suffix (file-name-suffix file))
       (make-directory dir t)
       (setq new (nvc-file-name (m26 (nvc-latest dir))))
       (setq name (concat dir "/" new suffix))
       (shell-command-on-region
	(point-min) (point-max) (format "diff -u %s - >%s" file name))
       (and (zerop (file-size name))
	    (delete-file name))
       nil
       )
     )
    )
  )
(defun nvc-file-append (&optional file)
  (let* (name new dir)
    (setq dir (concat (file-name-directory file) nvc-directory))
    (and
     (setq file (or file (buffer-file-name)))
     (file-exists-p file)
     (file-exists-p dir)
     (prog1 t
       (setq dir
	 (format "%s/%s" dir (format-time-string "%y%m%d" (current-time))))
       (or (file-exists-p dir) (make-directory dir))
       )
     (progn
       (setq name (file-name-nondirectory file))
       (shell-command-on-region
	(point-min) (point-max)
	(format "diff -u %s - >>%s/%s" file dir name))
       nil
       )
     )
    )
  )

(defun nvc-file (&optional file)
  (funcall nvc-method file)
  )

(defun nvc-file-enable (file)
  (cond
   ((string-match "/ext/[^/]*\\.h$" file) nil)
   (t))
  )

(defun nvc-file-hook ()
  (let* ((file (buffer-file-name)))
    (and nvc-enable
	 (nvc-file-enable file)
	 (nvc-file file)
	 ))
  )

(setq nvc-enable t)

(make-variable-buffer-local 'nvc-enable)

(add-hook 'write-file-hooks 'nvc-file-hook)

;;;
;
;
;
;;;

(defun nvc-diff (&optional file n m)
  (let* ((file (or file (buffer-file-name)))
	 (dir (nvc-directory file))
	 (n (or n (1- (nvc-latest dir))))
	 (m (or m (1- n)))
	 (suffix (file-name-suffix file))
	 )
    (save-cd dir
      (shell-command
       (format "diff -u B%s%s B%s%s"
	       (format nvc-id-format n) suffix
	       (format nvc-id-format m) suffix
	       ))
      ))
    )

(defun nvc-delta-1 (&optional file n)
  (let* ((file (or file (buffer-file-name)))
	 (dir (nvc-directory file))
	 (max (nvc-latest dir))
	 (n (or n (1- max)))
	 (suffix (file-name-suffix file))
	 )
    (save-cd dir
      (let ((old (nvc-id "B" (1- n) suffix))
	    (new (nvc-id "B" n suffix)))
	(cond
	 ((and (file-exists-p old)
	       (file-exists-p new))
	  (shell-command
	   (format "diff -u %s %s > %s"
		   old new (nvc-id "D" n ".dif")
		   ))
	  ))))
      )
  )

(defun nvc-delta (&optional file)
  (let* ((file (or file (buffer-file-name)))
	 (dir (nvc-directory file))
	 (max (nvc-latest dir))
	 (suffix (file-name-suffix file))
	 (i 1)
	 )
    (save-cd dir
      (while (< i max)
	(message "%s..." i)
	(shell-command
	 (format "diff -u %s %s > %s"
		 (nvc-id "B" (1- i) suffix)
		 (nvc-id "B" i suffix)
		 (nvc-id "D" i ".dif")
		 ))
	(message "%s... done" i)
	(setq i (1+ i))
	))
    )
  )

;;;

(defun vc-quick (arg)
  (interactive "P")
  (let ((comment
	 (cond (arg (read-from-minibuffer "Comment: "))
	       (""))))
    (vc-checkin (buffer-file-name) nil comment)
    (vc-checkout (buffer-file-name) t)
    )
  )
