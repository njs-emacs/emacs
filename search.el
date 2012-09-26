(defun other-window-search (pat)
  "Search for PATTERN in other windoww"
  (isearch-update-ring pat t)
  (ow
   (isearch-forward-regexp)
   )
  )

(defun qrr (from to &optional arg) (query-replace-regexp from to (not arg)))

(defun xqrr (s)
  (interactive
    (list
     (read-from-minibuffer
      (format "sReplace \"%s\" with: " (x-get-cut-buffer))
      (x-get-cut-buffer)
      )
     )
    )
  (qrr (x-get-cut-buffer) s)
  )

(define-key global-map "\M-$" 'xqrr)

(defun depth (&optional arg lim)
  (let ((arg (or arg (point)))
	(lim (or lim (sx (rsb "^\\s(" nil t)) (point-min))))
    (car (parse-partial-sexp lim (point)))))

(defun string-match-region (string &optional n)
  (setq n (or n 0))
  (substring string (match-beginning n) (match-end n))
  )

(defun match-region (&optional n)
  (list (match-beginning (or n 0)) (match-end (or n 0))))

(defun gmb (&optional n) (goto-char (match-beginning (or n 0))))
(defun gme (&optional n) (goto-char (match-end (or n 0))))

(defun string-match-string (regexp string &optional n)
  "Return the part of STRING which matches N'th (default 0) sub-expression
of match with REGEXP"
  (let ((i (string-match regexp string))
	(n (or n 0)))
    (and i (match-beginning n)
	 (substring string (match-beginning n) (match-end n)))))

(defun find-match-string-nosave (s &optional n search)
  "Scan forward for REGEXP, and return the string matching sub-expression N.
Previous match data is NOT kept intact (see match-data).\n\n\n"
  (setq n (or n 0))
  (save-excursion
     (and
      (funcall (or search 're-search-forward) s nil t)
      (buffer-substring (match-beginning n) (match-end n)))))

(defun find-match-string (s &optional n search)
  "Scan forward for REGEXP, and return the string matching sub-expression N.
Previous match data is kept intact (see match-data).\n\n\n"
  (setq n (or n 0))
  (save-search 
   (save-excursion
     (and
      (funcall (or search 're-search-forward) s nil t)
      (buffer-substring (match-beginning n) (match-end n))))))

(defun find-match-strings (s &rest list)
  (save-search 
   (save-excursion
     (and
      (re-search-forward s nil t)
      (mapcar 'ms list))))
  )

(defun string-parse-r (string list)
  "Parse STRING according to results of previous string-match."
  (mapcar '(lambda (i)
	     (and (match-beginning i)
		  (substring string (match-beginning i) (match-end i)))) list))

(defun string-parse (string regexp &rest n)
  "Parse STRING by applying a search for REGEXP, then taking the match data
according to the remaining arguments."
  (and (string-match regexp string)
       (string-parse-r string n))
  )

(defun sp (string regexp &optional n)
  (car (string-parse string regexp (or n 0))))

(defun string-partition (string regexp &optional n)
  "Partition STRING into sections each matching REGEXP.\n\
Optional third argument is substring to select."
  (let (out (n (or n 0)) (start 0) (end 0) (length (length string)))
    (while (< start length)
      (setq end (or (and (string-match regexp string start)
			 (me n)) length))
      (setq out (cons (substring string (mb n) end) out))
      (setq start end)
      )
    (nreverse out)))

(defun cut-region (start end)
  (prog1 (bs start end) (delete-region start end)))

(defun cut-match (&optional i)
  (setq i (or i 0))
  (cut-region (match-beginning i) (match-end i))
  )

(defun kill-regexp (s &optional i)
  (setq i (or i 0))
  (and (re-search-forward s nil t) (kill-match i)))

(defun kill-match (&optional i)
  (setq i (or i 0))
  (let ((s (bs (match-beginning i) (match-end i))))
    (kill-region (match-beginning i) (match-end i))
    (goto-char (match-beginning i))
    s)
  )

(defun kill-regexp (s &optional i)
  (setq i (or i 0))
  (and (re-search-forward s nil t) (kill-match i)))

(defun kill-to-regexp-start (s &optional i insert)
  (interactive "sKill to regexp start: \nP")
  (let ((p (point)))
    (setq i (or i 0))
    (and (sx (re-search-forward s nil t))
	 (kill-region p (match-beginning i)))
    (and insert (insert insert))
    ))

(defun kill-to-regexp-end (s &optional i insert)
  (interactive "sKill to regexp end: \nP")
  (let ((p (point)))
    (setq i (or i 0))
    (and (sx (re-search-forward s nil t))
	 (kill-region p (match-end i)))
    (and insert (insert insert))
    ))

(fset 'krs 'kill-to-regexp-start)
(fset 'kre 'kill-to-regexp-end)

(defun replace (text &optional end)
  (let ((start (sxp (forward-line 1)))
	(end (sxp (rsf (or end "^/\\*\\*/\\|^##")) (bol))))
    (kill-region start end)
    (goto-char start)
    (insert text)))

(defun replace-one (regexp replace)
  "Replace a single instance of REGEXP with REPLACEMENT"
  (and (rsf regexp nil t)
       (save-restriction
	 (narrow-to-region (match-beginning 0) (match-end 0))
	 (goto-char (point-min))
	 (replace-regexp regexp replace))
    )
  )

(defmacro reps (s old &optional new limit)
  `(let ((buf (make-buffer "*reps*" ,s)))
     (sx (set-buffer buf)
	 (while (rep ,old ,new ,limit))
	 (prog1 (buffer-string) (kill-buffer buf))))
  )

(defmacro rep (old &optional new limit)
  `(cond
      ((rsf ,old ,limit t)
       (let ((new ,new))
	 (delete-region (mb 0) (me 0))
	 (and new (insert new))
	 t)
       )))

(defmacro rep$ (old &optional new)
  `(rep ,old ,new (point$))
  )

(defmacro mrep (old new &optional limit)
  `(while (rep ,old ,new ,limit)))

(defmacro mrep$ (old new &optional limit)
  `(while (rep ,old ,new (point$))))

(defun match-list (rx &optional n)
  "Returns a list of strings which match REGEXP in the current buffer.
Optional N is used to select sub-expression."
  (sx
   (let ((n (or n 0)) result)
     (bob)
     (while (rsf rx nil t)
       (setq result (cons (match-string n) result)))
     result)))

(setq search-map (make-sparse-keymap))
(define-key global-map "\es" search-map)

(define-key search-map "[" 'kill-to-regexp-start)
(define-key search-map "]" 'kill-to-regexp-end)

(defun rsb (s &optional lim e n end)
  (cond ((re-search-backward s lim (or e t))
	 (goto-char
	  (funcall (if end 'match-end 'match-beginning) (or n 0)))
	 (point))))
	       
(defun rsf (s &optional limit error n beg)
"Search for STRING, before LIMIT is reached.
NOERROR is as for re-search-forward. If N is non-nil, move to that
match, and if BEG is non-nil move to the beginning of the match."
  (cond ((re-search-forward s limit (or error t))
	 (goto-char
	  (funcall (if beg 'match-beginning 'match-end) (or n 0)))
	 (point))))

(defmacro MS (exp &optional n)
  `(and ,exp (ms (or ,n 0))))

(defmacro MB (exp &optional n)
  `(and ,exp (goto-char (match-beginning (or ,n 0)))))

(defmacro ME (exp &optional n)
  `(and ,exp (goto-char (match-end (or ,n 0)))))

(defun replace-region (region &rest strings)
  (sx
   (apply 'kill-region region)
   (goto-char (car region))
   (mapcar 'insert strings)
   ))

(defmacro repr (region &optional string)
  `(sx
    (let ((region ,region)
	  (string ,string))
	(apply 'kill-region region)
	(goto-char (car region))
	(and string (insert string))
	)))

(defun search-pattern-bounds (s1 s2 &optional n1 n2)
  (sx
   (let ((p1 (and (re-search-forward s1 nil t)
		  (match-end (or n1 0))))
	 (p2 (and (re-search-forward s2 nil t)
		  (match-beginning (or n2 0)))))
     (list p1 (or p2 p1))
     )))

(defun rtor (s1 s2 &optional n1 n2) (search-pattern-bounds s1 s2 n1 n2))

(defun rtori (s1 s2 &optional n1 n2)
  (sx
   (let ((p1 (and (re-search-forward s1 nil t)
		  (match-beginning (or n1 0))))
	 (p2 (and (re-search-forward s2 nil t)
		  (match-end (or n2 0)))))
     (list p1 (or p2 p1))
     )))

(defun repc (s)
  (repr (rtor "/\\*{.*\n" "/\\*}.*\n") s))

(defmacro map-search (pat &rest body)
  `(sx
    (let ((pat ,pat)
	  (out))
      (bob)
      (while (rsf pat nil t)
	(setq out (cons (progn ,@body) out)))
      (nreverse out)
      )))

;;; this has a problem in emacs 24.2
;;; macros which execute directly rather than return an executable
;;; form cause byte-compilation problems

(defmacro case-match (*s &rest forms)
  (setq *s (eval *s))
  (let ((save (match-data)))
    (prog1 (sx
     (cons 'progn
	   (catch 'done
	     (while forms
	       (let ((car (car forms)))
		 (cond ((string-match (car car) *s)
			(throw 'done (cdr car))))
		 (setq forms (cdr forms))
		 )))))
;      (store-match-data save)
      )))

(defmacro case-match (*s &rest *forms)
  `(let ((save (match-data))
	 (s ,*s)
	 (forms ',*forms)
	 )
     (prog1
	 (sx
	  (progn
	    (catch 'done
	      (while forms
		(let ((car (car forms)))
		  (cond ((string-match (car car) s)
			 (throw 'done (cdr car))))
		  (setq forms (cdr forms))
		  )))))
;      (store-match-data save)
      )))

(defun re-kill-line (pat &optional n)
  (cond ((rsf pat) (bol) (kill-line (or n 1)))))

(defun string-substitute-set (s in cout)
  (save-search
   (let* ((s (copy-sequence (if (symbolp s) (symbol-name s) s)))
	  (i 0)
	  (len (length s)))
     (while (< i len)
       (if (string-match in (char-to-string (aref s i))) (aset s i cout))
       (setq i (1+ i))
       )
     s
     )
   ))

(defun string-substitute (s cin cout)
  (let* ((s (copy-sequence (if (symbolp s) (symbol-name s) s)))
	 (i 0)
	 (len (length s)))
    (while (< i len)
      (if (eq (aref s i) cin) (aset s i cout))
      (setq i (1+ i))
      )
    s
    )
  )

(defmacro map-search (pat &rest body)
 `(let ((r))
    (sx (bob)
	(while (rsf ,pat) (setq r (cons (progn ,@body) r))))
    (nreverse r))
 )

(load-overrides "search")
