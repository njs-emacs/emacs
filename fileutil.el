(defun write-string-to-file (filename string &optional append)
  (with-temp-buffer
    (erase-buffer)
    (insert string)
    (let ((coding-system-for-write 'raw-text))
      (write-region (point-min) (point-max) filename append)
      )
    )
  )

(defun write-string-to-file (filename string &optional append)
  (write-region string nil filename append 1)
  )

(defun filename-replace-suffix (suffix &optional name)
  (concat (basename name) suffix)
  )

(defun file-name-identical (a b)
  (equal (expand-file-name a) (expand-file-name b))
  )

(defun sync-default-directory () (interactive)
  (setq default-directory (file-name-directory (buffer-file-name)))
  )

(defun ls (&optional s)
  (unconcat-lines (call-shell (concat "ls " s))))

(defun ls-newer (spec file)
  (let* ((list (ls (concat "-rt " spec))))
    (nreverse (or (cdr (memqu file list)) list))))

(defun ls-find (&optional s path)
  (unconcat-lines
   (call-shell (format "find %s -type f %s -print"
			(or path ".") (or s "")))
   ))

(defun ls-grep (pattern &optional filespec)
  (unconcat-lines
   (call-shell
    (format "grep -l \"%s\" %s" pattern (or filespec "*")))))

(defun save-kill (&optional buf)
  (and buf (set-buffer (get-buffer buf)))
  (and (buffer-modified-p) (save-buffer))
  (kill-buffer (current-buffer))
  )

(defun save-kill-file (file)
  (let ((buf (get-file-buffer file)))
    (cond (buf
	   (set-buffer buf)
	   (and (buffer-modified-p) (save-buffer))
	   (kill-buffer buf))))
  )

(defmacro dofiles (files &rest body)
  (setq files (eval files))
  (let (out file)
    (while files
      (setq file (car files))
      (message (format "next file...%s" file))
      (let ((default-directory default-directory))
	(find-file file)
	(condition-case erc
	    (setq out (cons (eval (cons 'prog1 body)) out))
	  (error nil))
	)
      (setq files (cdr files))
      )
    (list 'quote (nreverse out))
    ))

(defmacro dofile-pairs (list &rest body)
  (setq list (eval list))
  (let (out filea fileb bufa bufb)
    (while list
      (save-cd "."
	(save-window-excursion
	  (setq filea (expand-file-name (caar list)))
	  (setq fileb (expand-file-name (cadar list)))
	  (message (apply 'format "next pair... %s and %s" (car list)))
	  (switch-to-buffer (setq bufa (find-file-noselect filea)))
	  (display-buffer (setq bufb (find-file-noselect fileb)) t)
	  (condition-case erc
	      (setq out (cons (eval (cons 'progn body)) out))
	    (error nil))
	  (and (get-file-buffer filea) (save-kill filea))
	  (and (get-file-buffer fileb) (save-kill fileb))
	  (setq list (cdr list))
	  ))
      )
    (list 'quote (nreverse out))
    ))

(defun touch (files)
  (call-shell (mconcat (cons "touch" files) " ")))

(defun unbury-buffer () (interactive)
  (let ((rev (reverse (buffer-list))))
    (while (= (aref (buffer-name (car rev)) 0) ? ) (setq rev (cdr rev)))
    (switch-to-buffer (car rev))))

(defun bury-other () (interactive)
  (cond ((one-window-p) (split-window-vertically)))
  (other-window 1)
  (bury-buffer))

(defun file-name-suffix (&optional name)
  "Return the suffix of FILENAME.
For example (file-name-suffix \"emacs/modes.el\") returns \".el\""
  (or name (setq name (buffer-file-name)))
  (and (string-match ".*\\(\\..*$\\)" name)
       (substring name (match-beginning 1) (match-end 1))))

(defun basename (&optional name)
  "Returns the basename of FILENAME, i.e with the directory and suffix stripped."
  (setq name (file-name-nondirectory (or name (buffer-file-name))))
  (let ((suffix (file-name-suffix name)))
    (if suffix (substring name 0 (- (length suffix))) name)))

;;;(defun file-contents (name &optional size)
;;;  "Returns the contents of file with name NAME."
;;;  (let ((buffer (get-file-buffer name)))
;;;    (cond 
;;;     (buffer (save-excursion (set-buffer buffer) (buffer-substring-no-properties 1 (or size (point-max)))))
;;;     ((file-exists-p name)
;;;      (setq buffer (find-file-noselect name))
;;;      (prog1 (save-excursion (set-buffer buffer) (buffer-substring-no-properties 1 (or size (point-max))))
;;;	(kill-buffer buffer)))
;;;     ))
;;;  )

(defun file-contents (name &optional size)
  "Returns the contents of file with name NAME."
  (save-excursion 
    (let ((buffer (get-file-buffer name)))
      (cond 
       (buffer
	(set-buffer buffer)
	(buffer-substring-no-properties 1 (or size (point-max))))
       ((file-exists-p name)
	(setq buffer (get-buffer-create name))
	(set-buffer buffer)
	(insert-file-contents-literally name nil)
	(prog1 (buffer-substring-no-properties 1 (or size (point-max)))
	  (kill-buffer buffer)))
       ))
    )
  )

(defun file-contains-pattern (file pat &optional limit)
  (let* ((fc (file-contents file limit)))
    (string-match-string pat fc)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-file (file &rest body)
  `(save-window-excursion
     (find-file ,file)
     (bob)
     ,@body))

(defun make-buffer (name &rest contents)
  (sx (let ((buf (set-buffer (get-buffer-create name))))
	(erase-buffer)
	(insert (cat contents "\n"))
	(bob)
	buf)))

(defun make-file (name contents &optional display hook)
  "Create FILE with CONTENTS. if optional DISPLAY non-nil, display the file
on completion. If optional HOOK is given, call this before closing the file."
  (save-excursion
    (let* ((open (get-file-buffer name))
	   (buf (or open (find-file-noselect name)))
	   )
      (set-buffer buf)
      (erase-buffer)
      (apply 'insert (flatten contents))
      (and hook (funcall hook))
      (save-buffer)
      (if (or open display)
	  (or (get-buffer-window buf) (display-buffer buf))
	(kill-buffer (current-buffer)))
      )))

(defun tabify-buffer ()
  (tabify (point-min) (point-max))
  )

(defun file-print () (interactive)
  (start-process "print" nil "lpr" "-Pdp" (buffer-file-name)))

(defun rename-files-fun (list fun &optional confirm)
  (mapcar '(lambda (old)
	     (let ((new (funcall fun old)))
	       (message (format "renaming %s to %s" old new))
	       (rename-file old new confirm)))))

(defun rename-files (list format &optional confirm)
  (mapcar '(lambda (old)
	     (let ((new (format format old)))
	       (message (format "renaming %s to %s" old new))
	       (rename-file old new confirm))) list))

(defun rename-buffer-file () (interactive)
  (let* ((old (file-name-nondirectory (buffer-file-name)))
	 (new (read-file-name "New file name: " nil nil nil old)))
    (set-visited-file-name new)
    (rename-file old new)
    ))

(defmacro save-cd (dir &rest body)
  `(let ((save default-directory))
       (cd (expand-file-name ,dir))
       ,@body
       (cd save)
       ))

(defun dir-path () (nreverse (unconcat default-directory "/")))
(defun file-path (name) (nreverse (unconcat name "/")))

(defun kill-or-bury-current-buffer (arg) (interactive "P")
  (cond ((and (not arg)
	      (buffer-file-name)
	      (buffer-modified-p)
	      ) (bury-buffer))
	(t (setq killed-buffer (buffer-file-name))
	   (kill-buffer (current-buffer)))
	)
  )

(defun restore-killed-buffer () (interactive)
  (find-file killed-buffer)
  )

(make-local-variable 'kill-current-buffer-hook)

(defun kill-current-buffer (arg) (interactive "P")
  (cond
   ((run-hooks 'kill-current-buffer-hook))
   (t
    (and arg
	 (set-buffer-modified-p nil)
	 )
    (setq killed-buffer (buffer-file-name))
    (kill-buffer (current-buffer))
    )
   )
  )

(defun find-file-force (file)
  (let ((buf (get-file-buffer file)))
	(cond (buf
		   (switch-to-buffer buf)
		   (cond ((verify-visited-file-modtime buf))
				 (
				  (revert-buffer t t))
				 ))
		  ((find-file file))
		  )
	)
  )

(defun exe ()
  (concat (basename) ".exe")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq filename-char "\\(\\sw\\|[-_./:~@]\\)")

(defun filename-at-point (&optional point)
;; todo: improve to look ahead for existing paths
  (save-excursion
    (and point (goto-char point))
    (setq point (point))
    (let (start end)
      (setq start (progn (while (looking-at filename-char) (fc -1)) (point)))
      (cond ((looking-at "\\([\"']\\)")
	     (setq end (sxp (fc 1) (rsf (match-string 1)) (fc -1)))
	     )
	    ((setq end (sxp (goto-char point) (while (looking-at filename-char) (fc 1)))))
	    )
      (buffer-substring (1+ start) end)
      )
    )
  )

(defun funcall-until-first-non-nil (list arg)
  (let (r)
    (while (and list (not r))
      (setq r (funcall (car list) arg))
      (setq list (cdr list))
      )
    r)
  )

(defvar clever-filename-guess-function-list nil "List of filename guess functions to call")
(defvar clever-filename-guess-local-function-list nil "Local list of filename guess functions to call")

(setq clever-filename-guess-function-list '(clever-filename-guess-last))
(make-local-variable 'clever-filename-guess-local-function-list)

(defun perl-use-clever-file (s)
  (let ((pm (format "%s.pm" s)))
    (locate-file-in-path pm (list
			     "d:/p/perl/lib"
			     "e:/perl/lib"
			     )
			 )
    )
  )

(defun clever-filename-guess-last (&optional point)
  "This function is called after all the other clever-filename-guess functions have been tried"
  (let (file (filename-at-point point))
    (sx
     (and point (goto-char point))
     (cond 
      ((bol-looking-at "^use\\s *\\(\\S *\\)")
       (perl-use-clever-file (ms 1))
       )
      ((
	)
       )
      )
     )
    ))

(defun clever-filename-guess (&optional point)
  (or
   (funcall-until-first-non-nil clever-filename-guess-local-function-list point)
   (funcall-until-first-non-nil clever-filename-guess-function-list point)
   )
  )

(defun clever-filename-at-point (&optional point)
  (let ((file (filename-at-point point)))
    (cond
     ((file-exists-p file) file)
     ((clever-filename-guess point))
     )
    )
  )

(defun find-file-at-point (&optional point) (interactive)
  (let ((file (clever-filename-at-point point)))
    (cond 
     (file (find-file-other-window file))
     ((message "Can't find an existing file there"))
     )
    )
  )

(defun find-file-at-point-mouse (arg) (interactive "e")
  (let ((file (sx (mouse-set-point arg) (clever-filename-at-point (point)))))
    (cond 
     (file (find-file-other-window file))
     ((message "Can't find an existing file there"))
     )
    )
  )

(defun find-kill-head () (interactive) "Open file with name in the kill head"
  (find-file (car kill-ring))
  )

(defun filename-unconcat (f)
  (unconcat (filename-canonical f) "/"))

(defun filename-directory-last (&optional f)
  (or f (setq f default-directory))
  (cond
   ((file-directory-p f))
   (t (setq f (file-name-directory f)))
   )
  (car (reverse (filename-unconcat f)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bulkload-1 (file)
  (cond ((file-contains-pattern file "## *nobulkload *##"))
	((load file))
	)
  )

(defun bulkload () (interactive)
  (let* ((d default-directory)
	 (files (directory-files "." nil ".el"))
	 )
    (setq load-path (cons dir load-path))
    (mapcar 'bulkload-1 files)
    (setq load-path (cdr load-path))
    )
  nil
  )

(defun bulkload-directory (dir)
  (let* ((dir (expand-file-name dir))
	 (default-directory dir)
	 (files (directory-files "." nil ".el"))
	 )
    (setq load-path (cons dir load-path))
    (mapcar 'bulkload-1 files)
    (setq load-path (cdr load-path))
    )
  nil
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#~ key bindings should be segregated from code as they may conflict

(setq f-map (make-sparse-keymap))

(defun revert-buffer-no-ask () (interactive) (revert-buffer t t))
(defun dired-quick () (interactive) (dired default-directory))

(define-key f-map "\M-f" 'find-file)
(define-key f-map "\M-d" 'dired-quick)
(define-key f-map "\M-o" 'link-file-visit-other)

(define-key f-map "r" 'revert-buffer-no-ask)
(define-key f-map "p" 'file-print)
(define-key f-map "d" 'delete-file)
(define-key f-map "t" 'visit-tags-table)
(define-key f-map "R" 'rename-buffer-file)
