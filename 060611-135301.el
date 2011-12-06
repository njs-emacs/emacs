(defun nvc-old-filename (date time file)
  (let* ((name (file-name-nondirectory file))
	 (dir (nvc-dirname file date))
	 )
    (format "%s%s@%s-%s%s"
	    dir (basename name) date time
	    (or (file-name-suffix name) ""))
    ))

(ediff-files
 (nvc-old-filename "060611-122233" "e:/_backup/emacs/logfilter.pl")
 (nvc-old-filename "060611-122311" "e:/_backup/emacs/logfilter.pl")
 )

(defun bms (n)
  (buffer-substring (match-beginning n) (match-end n))
  )

; #~# :unknown looked at this and don't know what it does
(defun foo (arg) (interactive "P")
  (let ((x
	 (sx
	  (cond
	   ((sx (bol) (looking-at "\\([0-9]+\\)-\\([0-9]+\\) \\S \\{8\\} \\w+\t\\([a-z]:.+\\)"))
	    (let* ((d (match-data))
		   (m (match-beginning 1))
		   )
	      (setq date (bms 1))
	      (setq time (bms 2))
	      (setq file (bms 3))
	      (setq foo (nvc-old-filename date time file))
	      (setq x (nth 5 (file-attributes foo)))
	      (setq pdate (format-time-string "%y%m%d" x))
	      (setq ptime (format-time-string "%H%M%S" x))
	      (setq pfoo (nvc-old-filename pdate ptime file))
	      (other-frame 1)
	      (ediff-files foo pfoo)
	      )
	    )
	   )
	  )
	 )
	)
    )
  )

