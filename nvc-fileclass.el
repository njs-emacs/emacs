
(defun nvc-path-replace (offset &optional name)
  "Replace nvc-daily path part of file. Does not change filename part"
  (or name (setq name (buffer-file-name)))
  (cond
   ((string-match "/[0-9]\\{2\\}/[0-9]\\{4\\}/\\([0-9]\\{6\\}\\)/" name)
    (let* ((i (match-beginning 1))
	   (y (substring name i (+ i 2)))
	   (m (substring name (+ i 2) (+ i 4)))
	   (d (substring name (+ i 4) (+ i 6)))
	   (time (encode-time 0 0 0 (string-to-int d) (string-to-int m) (+ 2000 (string-to-int y))))
	   (itime (time-add time (days-to-time offset)))
	   (dtime (decode-time itime))
	   (ftime (format-time-string "%y/%y%m/%y%m%d" itime))
	   )
      (concat (substring name 0 (1+ (match-beginning 0))) ftime (substring name (+ i 6)))
      )
    )
   )
  )

(file-class-guess-name-add 'nvc nvc-root)

(defun nvc-linked-file ()
  (let* ((offset (alist-get '((prev . -1) (next . 1)) tag))
	 )
    (cond
     (offset (path-replace-until-found 'nvc-path-replace offset name 30))
     )
    )
  )

(file-class-guess-pattern-add 'nvc-log "^[0-9]\\{6\\}-[0-9]\\{6\\} [VS] ")

(file-class-linked-file-add
 'nvc-log
 '(
   (t . nvc-linked-file)
   ))

