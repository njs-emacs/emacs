;; stop file-class guessing

(setq file-class 'emacs-lisp-init)

(defun daily-path (a &optional force-create)
  (let* ((path (filename-concat home-daily-root a))
	 (dir (file-name-directory path)))
    (cond
     ((not force-create))
     ((file-exists-p dir))
     ((make-directory dir t))
     )
    path
    ))

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
(defun daily-date-path (name &optional time format force-create)
  (and format (setq name (format-time-string name time)))
  (daily-path (format "%s/%s" (format-time-string "%y/%y%m/%d" time) name) force-create)
  )

(defun daily-month-path (name &optional time format force-create)
  (and format (setq name (format-time-string name time)))
  (daily-path (format "%s/%s" (format-time-string "%y/%y%m" time) name) force-create)
  )

(defun daily-date-path-create (name &optional time format)
  (daily-date-path name time format t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun daily-path-replace (offset &optional name)
  (error "fix this")
  "Replace daily path part of file. Does not modify any date stamp in the filename part" 
  (or name (setq name (buffer-file-name)))
  (let* ((i (string-match "\\([0-9][0-9][0-9][0-9]\\)/" name))
	 (y (substring name i (+ i 2)))
	 (m (substring name (+ i 2) (+ i 4)))
	 (d (substring name (+ i 5) (+ i 7)))
	 (time (encode-time 0 0 0 (string-to-int d) (string-to-int m) (+ 2000 (string-to-int y))))
	 (itime (time-add time (days-to-time offset)))
	 (dtime (decode-time itime))
	 (ftime (format-time-string "%y/%y%m/%d" itime))
	 )
    (concat (substring name 0 i) ftime (substring name (+ i 7)))
    )
  )

(defun monthly-path-replace (offset &optional name)
  (error "fix this")
  "Replace daily path part of file. Does not modify any date stamp in the filename part" 
  (or name (setq name (buffer-file-name)))
  (let* ((i (string-match "\\([0-9][0-9][0-9][0-9]\\)/" name))
	 (y (read (substring name i (+ i 2))))
	 (m (read (substring name (+ i 2) (+ i 4))))
	 )
    (setq mm (+ (+ (* 12 y) (1- m)) offset))
    (setq m (1+ (mod mm 12)))
    (setq y (/ mm 12))
    (concat (substring name 0 i) (format "%02d%02d" y m) (substring name (+ i 4)))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun daily-linked-file ()
  (let ((offset (alist-r-get '((prev . -1) (next . 1)) tag)))
    (cond
     (offset (path-replace-until-found 'daily-path-replace offset name 30))
     )
    )
  )

(file-class-linked-file-add
 'daily
 '(
   (t . daily-linked-file)
   ))

(file-class-guess-name-add 'daily (daily-path "[0-9]\\{4\\}/[0-9]\\{2\\}/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(file-class-linked-file-add
 'monthly
 '(
   (t . monthly-linked-file)
   ))

(defun monthly-linked-file ()
  (let ((offset (alist-r-get '((prev . -1) (next . 1)) tag)))
    (cond
     (offset (path-replace-until-found 'monthly-path-replace offset name 12))
     )
    )
  )

(file-class-guess-name-add 'monthly (daily-path "[0-9]\\{4\\}/\\.month/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun daily-home-prev ()
  (daily-path-replace -1 name)
  )

(defun daily-home-next ()
  (daily-path-replace 1 name)
  )

(file-class-guess-name-add 'daily-home (daily-path "[0-9]*/[0-9][0-9]/$"))

(file-class-linked-file-add 'daily-home '((prev . daily-home-prev)
					  (next . daily-home-next)
					  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun daily-html-month (&optional time)
  (daily-month-path "%y%m.html" time t)
  )

(defun daily-html-time (&optional time)
  (daily-date-path "%y%m%d.html" time t)
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

(defun home-daily-yesterday (&optional force-create) (daily-path (format-time-string "%y/%y%m/%d/" (yesterday-time)) force-create))
(defun home-daily-today (&optional force-create) (daily-path (format-time-string "%y/%y%m/%d/") force-create))
(defun home-daily-month (&optional force-create) (daily-path (format-time-string "%y/%y%m/") force-create))

(defun daily-time-set () (interactive)
  (defun yesterday-time () (time-subtract (current-time) `(1 ,(- 86400 65536))))

  (setq home-daily-month (home-daily-month))
  (setq home-daily-today (home-daily-today))
  (setq home-daily-yesterday (home-daily-yesterday))

  (daily-qb-define)
  )

(defun daily-qb-define ()
  (minibuffer-dir "d" home-daily-today)
  (minibuffer-dir "m" home-daily-month)

  (qb-define (kbd "C-d C-m") '(home-daily-month))
  (qb-define (kbd "C-d C-d") '(home-daily-today))
  (qb-define (kbd "C-d C-y") '(home-daily-yesterday))

;  (qb-define (kbd "C-h C-m") '(daily-html-time (current-time)))
;  (qb-define (kbd "C-h C-d") '(daily-html-time (current-time)))
;  (qb-define (kbd "C-h C-y") '(daily-html-time (yesterday-time)))
  )

(defun home-daily-today-file (file)
  (daily-path (concat (format-time-string "%y/%y%m/%d/") file) t)
  )

(defun home-daily-org-file ()
  (concat
   (home-daily-today)
   (format-time-string "%y%m%d.org" (current-time))))

(daily-time-set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-all-month ()
  (let* ((ym (format-time-string "%y%m"))
	 (list (directory-files "." nil "^[0-9][0-9]$")))
    (dolist (i list)
      (let ((file (format "%s/%s%s.el" i ym i)))
	(cond
	 ((file-exists-p file)
	  (load-file-in-directory file)
	  )
	 )
	)
      )
    )
  )

