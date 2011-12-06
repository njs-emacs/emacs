(defun space-ify (s)
  (string-sub s "[_-]" " "))

(defun unspace-ify (s &optional new)
  (string-sub s "\\s +" (or new "_")))

(defun hyphen-rep (s &optional new)
  (string-sub s "-" (or new "_")))

(defun strip-white (s)
  (car (string-parse s "\\s *\\(\\S *\\)\\s *" 1))
  )

(defun strip-white (s)
  (let* ((beg (string-match "[^ \t\n]" s))
	 (end (and beg (or (string-match "[ \t\n]*\\'" s) (length s)))))
    (cond (beg (substring s beg end)) (s))
    ))

(defun unconcat (s &optional sep)
  (let (out (start 0) (end 0) (length (length s)) (sep (or sep "[ \t\n]+")))
    (while (< start length)
      (setq end (or (string-match sep s start) length))
      (setq out (cons (substring s start end) out))
      (setq start (max end (or (match-end 0))))
      )
    (nreverse out)))

(defun unconcat-lines (s) (unconcat s "\n+"))
(defun unconcat-white (s) (unconcat s "[ \t\n]+"))
