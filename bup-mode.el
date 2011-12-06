(defun buffer-nibble (n &optional before after)
  (and before (fc before))
  (prog1 
      (bs (point) (progn (fc n) (point)))
    (and after (fc after))
    ))

(defun bup-file-assemble (y m d host time file)
  (format "%s/emacs/%s/%s%s/%s%s%s/%s/%s/" backup-root y y m y m d host file)
  )

(defun bup-parse-line ()
  (sx
   (let* ((y (buffer-nibble 2))
	  (m (buffer-nibble 2))
	  (d (buffer-nibble 2))
	  (time (buffer-nibble 6 1 12))
	  (host (find-match-string-nosave "\\(\\S *\\)\\s *" 1))
	  file date dir dd
	  )
     (goto-char (match-end 0))
     (setq file (bs (point) (sxp (eol))))
     (setq dd (string-sub file ":" ""))
     (setq dd (string-sub dd "/[^/]*$" ""))
     (setq dir (format "%s/emacs/%s/%s%s/%s%s%s/%s/%s/" backup-root y y m y m d host file))
     (list y m d time host file dir)
     )
   )
  )

(defun bup-file-name (file &optional time prefix)
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

(defun bup-diff () (interactive)
  (let* ((bp (bup-parse-line))
	 (file (nth 5 bp))
	 (n (bup-file-name file
	      (nth 5 (file-attributes file))))
	 (x (bup-file-name file
	      (nth 5 (file-attributes n))))
	 )
    (printf "%s\n%s\n" n x)
;    (debug)
    (ediff-files n x)
    )
  )

(defun bup-visit-directory () (interactive)
  (let ((d))
    (sx (bol)
	(let* ((y (buffer-nibble 2))
	       (m (buffer-nibble 2))
	       (d (buffer-nibble 2))
	       (time (buffer-nibble 6 1 12))
	       (host (find-match-string-nosave "\\(\\S *\\)\\s *" 1))
	       file date
	       )
	  (goto-char (match-end 0))
	  (setq file (bs (point) (sxp (eol))))
	  (setq file (string-sub file ":" ""))
	  (setq file (string-sub file "/[^/]*$" ""))
	  (setq d (format "%s/emacs/%s/%s%s/%s%s%s/%s/%s/" backup-root y y m y m d host file))
	  (find-file-other-window d)
	  )
	)
    )
  )

(setq bup-mode-map (make-sparse-keymap))

(define-key bup-mode-map "d" 'bup-visit-directory)
(define-key bup-mode-map "a" 'bup-diff)


(defun bup-mode () (interactive)
  (use-local-map bup-mode-map)
  (cond (buffer-read-only)
	((toggle-read-only))
	)
  )

(defun uniquify-buffer-name (mode)
  (let ((i 0)
       name)
    (while
	(progn
	  (setq name
	    (format "*%s<%s>*" mode i))
	  (get-buffer name))
      (setq i
	(1+ i)))
    name))

(defun boo-hoo (arg) (interactive "SQuery: ")
  (let ((s (call-shell (format "perl %s/.meta/dreg/dred.pl %s" backup-root arg)))
	(buffer (get-buffer-create (uniquify-buffer-name "boo")))
	)
    (switch-to-buffer buffer)
    (erase-buffer)
    (insert s)
    (bob)
    (bup-mode)
    (goto-line 3)
    )
  )

(boo-hoo "")

