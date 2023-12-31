(defun buffer-nibble (n &optional before after)
  (and before (fc before))
  (prog1 
      (bs (point) (progn (fc n) (point)))
    (and after (fc after))
    ))

(defun bup-file-assemble (y m d host time file)
  (format "%s/emacs/%s/%s%s/%s%s%s/%s/%s/" backup-root y y m y m d host file)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	       (y (nth 0 info))
;	       (m (nth 1 info))
;	       (d (nth 2 info))
;	       (time (nth 3 info))
;	       (host (nth 4 info))
;	       (file (nth 5 info))
;	       (backup (nth 6 info))
;	       (shadow (nth 7 info))

(defun bup-parse-line ()
  (sx
   (bol)
   (let* (
	  (time-string (bs (point) (+ (point) 13)))
	  (y (buffer-nibble 2))
	  (m (buffer-nibble 2))
	  (d (buffer-nibble 2))
	  (time (buffer-nibble 6 1 1))
	  (type (buffer-nibble 1 0 10))
	  (host (find-match-string-nosave "\\(\\S *\\)\\s *" 1))
	  (file-start (match-end 0))
	  (end (sxp (eol)))
	  (sum (sx (rsf "\\s *[0-9a-f]\\{32\\} [0-9a-f]\\{32\\}\\s *$" end nil 0 t)))
	  file date dir dd info shadow bup-file file-name-no-path file-basename file-suffix 
	  )
     (and sum (setq end sum))
     (setq file (bs file-start end))
     (setq info (nvc-name-info file))
     (setq dd (string-sub file ":" ""))
     (setq dd (string-sub dd "/[^/]*$" ""))

     (setq dir (format "%s/emacs/%s/%s%s/%s%s%s/%s/%s" backup-root y y m y m d host (nth 1 info)))

     (setq file-name-no-path (nth 2 info))
     (setq file-basename (basename file-name-no-path))
     (setq file-suffix (file-name-suffix file-name-no-path))

     (setq shadow (filename-clean
		   (filename-format "%s/%s/%s/%s" nvc-shadow-root host (nth 1 info) (nth 2 info))))
     (setq bup-file (format "%s/%s@%s%s" dir file-basename time-string file-suffix))
     (list y m d time host file dir shadow bup-file type)
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

(defun bup-visit-bup () (interactive)
  (let* ((bp (bup-parse-line))
	 (bup-file (nth 8 bp))
	 (type (nth 9 bp))
	 )
    (cond ((string= type "S") (find-file-other-window bup-file))
	  ((error "Only save transactions for this command"))
	  )
    )
  )

;; this doesn't work unless the file was edited on the same host

(defun bup-diff () (interactive)
  (let* ((bp (bup-parse-line))
	 (file (nth 5 bp))
	 (bup-file (nth 8 bp))
	 (n (bup-file-name file
	      (nth 5 (file-attributes file))))
	 (x (bup-file-name file
	      (nth 5 (file-attributes n))))
	 )
    (printf "%s\n%s\n" n x)
;    (debug)
    (ediff-files file bup-file)
    )
  )

(defun bup-visit-directory () (interactive)
  (let ((d))
    (sx (bol)
	(let* ((info (bup-parse-line))
	       (b (nth 6 info))
	       )
	  (find-file-other-window b)
	  )
	)
    )
  )

(defun bup-get-file-name ()
  (sx (bol)
      (let* ((parse (bup-parse-line))
	     (file (nth 5 parse)))
	file
	)
      )
  )

(defun bup-find-file () (interactive)
  (let* ((info (bup-parse-line))
	 (file (nth 5 info))
	 (shadow (nth 7 info))
	 )
    (cond
     ((file-exists-p file) (find-file file))
     ((file-exists-p shadow) (find-file-read-only shadow) (message "shadow file (%s)" shadow))
     ((error "file doesn't exist"))
     )
    )
  )

(defun bup-find-file-other-window () (interactive)
  (let ((file (bup-get-file-name)))
    (find-file-other-window file)
    )
  )

(defun bup-qb-define ()
  (interactive)
  (qb-define-read-key (bup-get-file-name))
  )

(setq bup-mode-map (make-sparse-keymap))

(define-key bup-mode-map " " 'next-line)
(define-key bup-mode-map "f" 'bup-find-file)
(define-key bup-mode-map "o" 'bup-find-file-other-window)
(define-key bup-mode-map "d" 'bup-visit-directory)
(define-key bup-mode-map "b" 'bup-visit-bup)
(define-key bup-mode-map "a" 'bup-diff)
(define-key bup-mode-map "q" 'bup-qb-define)

(define-key bup-mode-map "z" 'bup-pipe-z)

(define-key bup-mode-map "r" 'bup-refresh)
(define-key bup-mode-map "R" 'bup-reverse-mode)

(defun bup-pipe-z () (interactive)
  (shell-command-on-region (region-beginning) (region-end) "perl z.pl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bup-special-save ()
  (interactive)
  (cond
   ((yes-or-no-p "You don't really want to save this do you? ")
    t)
   (t (error "Not saving") (top-level))
   ))
	      

(defun bup-mode () (interactive)
  (use-local-map bup-mode-map)
  (setq mode-name "BUP")
  (setq major-mode 'bup-mode)
  (cond (buffer-read-only)
	((toggle-read-only))
	)
  (buffer-disable-undo)
  (setq local-before-save-hooks 'bup-special-save)
  )

(defun uniquify-buffer-name (&optional old-name format)
  (let* ((old-name (or old-name (buffer-name)))
	 (format (or format "*%s<%s>*"))
	 (i 0)
       name)
    (while
	(progn
	  (setq name
	    (format format old-name i))
	  (get-buffer name))
      (setq i
	(1+ i)))
    name))

(defun boo-hoo-shell (&optional arg) (interactive "SQuery: ")
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

(defun boo-hoo-compile (&optional arg) (interactive "SQuery: ")
  (let ((cmd (format "perl %s/.meta/dreg/dred.pl %s" backup-root arg))
	buffer)
    (setq buffer (compilation-start cmd 'bup-mode nil nil))
    (pop-to-buffer buffer)
    (goto-line 6)
    )
  )

(fset 'boo-hoo 'boo-hoo-compile)

;(boo-hoo "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bup-file-history-file-on-current-line ()
  (let ((name (buffer-substring-no-properties (sxp (bol) (fx 1) (fc 1)) (sxp (eol)))))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; want mode which filters emacs.log and can sort

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun time-days-subtract (n &optional time)
  (or time (setq time (current-time)))
  (days-to-time (- (time-to-number-of-days time) n))
  )

(defun bup-tail-insert (buffer args)
  (cond (buffer-read-only (toggle-read-only)))
  (erase-buffer)
  (shell-command
   (format
    "d:/p/perl/bin/perl e:/_backup/.meta/bup-tail.pl --cmd=tail %s"
    args)
   buffer)
  (toggle-read-only)
  )

(defun bup-tail (name args)
  (let* ((buffer (get-buffer-create (format "*bup-%s*" name)))
	 )
    (switch-to-buffer buffer)
    (bup-tail-insert buffer args)
    (bup-mode)
    buffer
    ))

(defun bup-tail-from (from)
  (let* ((args (format "--from=%s" from)))
    (bup-tail from args)
    ))

(defun bup-month () (interactive)
  (let* ((time (time-days-subtract 30))
	 (from (format-time-string "%y%m%d" time))
	 )
    (bup-tail-from from)
    ))

(qb-define (kbd "C-b C-m") '(bup-month))

(defun bup-week () (interactive)
  (let* ((time (time-days-subtract 7))
	 (from (format-time-string "%y%m%d" time))
	 )
    (bup-tail-from from)
    ))

(qb-define (kbd "C-b C-w") '(bup-week))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bup-refresh ()
  (interactive)
  (let ((args (find-match-string "##args: \\(.*\\)" 1)))
    (bup-tail-insert (current-buffer) args)
    )
  )

;; shit
;; add this
