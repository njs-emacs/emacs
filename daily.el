;; stop file-class guessing

(setq file-class 'emacs-lisp-init)

(defun daily-path (a) (filename-concat home-daily-root a))

(defun yymmdd-to-time (s)
  (encode-time
   0
   0
   0
   (read (substring s 4 6))
   (read (substring s 2 4))
   (+ 2000 (read (substring s 0 2)))
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun daily-date-path (name &optional time format)
  (and format (setq name (format-time-string name time)))
  (daily-path (format "%s/%s" (format-time-string "%y%m/%d" time) name))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun daily-path-replace (offset &optional name)
  "Replace daily path part of file. Does not modify any date stamp in the filename part" 
  (or name (setq name (buffer-file-name)))
  (let* ((i (string-match "\\([0-9][0-9][0-9][0-9]\\)/" name))
	 (y (substring name i (+ i 2)))
	 (m (substring name (+ i 2) (+ i 4)))
	 (d (substring name (+ i 5) (+ i 7)))
	 (time (encode-time 0 0 0 (string-to-int d) (string-to-int m) (+ 2000 (string-to-int y))))
	 (itime (time-add time (days-to-time offset)))
	 (dtime (decode-time itime))
	 (ftime (format-time-string "%y%m/%d" itime))
	 )
    (concat (substring name 0 i) ftime (substring name (+ i 7)))
    )
  )

(defun daily-linked-file ()
  (let ((offset (alist-get '((prev . -1) (next . 1)) tag)))
    (cond
     (offset (daily-path-replace offset))
     )
    )
  )

(file-class-linked-file-add
 'daily
 '(
   (t . daily-linked-file)
   ))

(defun daily-home-prev ()
  (daily-path-replace -1 name)
  )

(defun daily-home-next ()
  (daily-path-replace 1 name)
  )

(file-class-guess-pattern-add 'daily-html "created by create.php")

(file-class-guess-name-add 'daily-home "e:/daily/[0-9]*/[0-9][0-9]/$")

(file-class-linked-file-add 'daily-home '((prev . daily-home-prev)
					  (next . daily-home-next)
					  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun daily-html-month (&optional time)
  (daily-date-path "%y%m/%y%m.html" time t)
  )

(defun daily-html-time (&optional time)
  (daily-date-path "%y%m/%d/%y%m%d.html" time t)
  )

(defun daily-html+ (s i)
  (let* ((base (substring (basename s) 0 6))
	 (other (format-time-string "%y%m%d" (time-add
				    (yymmdd-to-time base)
				    (days-to-time i))))
	 )
    (daily-path (format "%s/%s/%s.html"
	    (substring other 0 4)
	    (substring other 4 6)
	    other))
    )
  )

(defun daily-html-prev ()
  (let* ((s (buffer-name))
	 )
    (daily-html+ s -1)
    )
  )

(defun daily-html-next ()
  (let* ((s (buffer-name))
	 )
    (daily-html+ s 1)
    )
  )

(file-class-guess-pattern-add 'daily-html "created by create.php")
(file-class-linked-file-add 'daily-html
			    `((next . daily-html-next)
			      (prev . daily-html-prev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq emacs-scratch-command-file (daily-path "0.el"))

(defun emacs-scratch ()
  (let ((s default-directory) f)
    (catch 'done
      (while t
	(setq s (substring s 0 -1))
	(cond
	 ((or (string-match "\\w:$" s)
	      (string-match "//\\w+\\(\\|/\\w+\\)$" s))
	  (throw 'done emacs-scratch-command-file))
	 )
	(setq f (format "%s/zz0.el" s))
	(cond
	 ((file-exists-p f) (throw 'done f))
	 )
	(setq s (file-name-directory s))
	)
      )
    )
  )

(defun home-daily-yesterday () (daily-path (format-time-string "%y%m/%d" (yesterday-time))))
(defun home-daily-today () (daily-path (format-time-string "%y%m/%d")))
(defun home-daily-month () (daily-path (format-time-string "%y%m")))

(defun daily-time-set () (interactive)
  (defun yesterday-time () (time-subtract (current-time) `(1 ,(- 86400 65536))))

  (setq home-daily-month (home-daily-month))
  (setq home-daily-today (home-daily-today))
  (setq home-daily-yesterday (home-daily-yesterday))

  (make-directory home-daily-today t)
  (daily-qb-define)
  )

(defun daily-qb-define ()
  (minibuffer-dir "d" home-daily-today)
  (minibuffer-dir "m" home-daily-month)

  (qb-define (control-key-vector ?d ?m) '(home-daily-month))
  (qb-define (control-key-vector ?d ?d) '(home-daily-today))
  (qb-define (control-key-vector ?d ?y) '(home-daily-yesterday))

  (qb-define (control-key-vector ?h ?m) '(daily-html-time (current-time)))
  (qb-define (control-key-vector ?h ?d) '(daily-html-time (current-time)))
  (qb-define (control-key-vector ?h ?y) '(daily-html-time (yesterday-time)))
  )

(daily-time-set)

