(defun slash-back (s) (string-sub s "/" "\\"))
(defun slash-front (s) (string-sub s "\\\\" "/"))
(defun slash-back-double (s) (replace-regexp-in-string "\\\\" "\\&\\&" s))

(defun path-set (list)
  (setenv "PATH" (mconcat list ";"))
  (setq exec-path (mapcar 'slash-front list))
  )

(defun delete-string-from-list-regexp (pat list)
  (let ((case-fold-search t))
    (delete-if '(lambda (x) (string-match (regexp-quote pat) x)) list)
    )
  )

(defun path-element= (a b)
  (let* ((a (downcase a))
	 (b (downcase b))
	 )
    (or (string= a b)
	(string= a (expand-file-name b))
	(string= (expand-file-name a) b)
	(string= (expand-file-name a) (expand-file-name b))
	)
    )
  )

(defun path-split ()
  (let* ((path (getenv "PATH")))
    (unconcat path ";")
    )
  )

(defun path-add (p)
  (let* ((path (getenv "PATH"))
	 (path-list (unconcat path ";"))
	 )
    (cond ((member-if '(lambda (x) (path-element= p x)) path-list))
	  (t (setenv "PATH" (concat p ";" path)))
	  )
    )
  )

(defun path-delete (p)
  (let* ((path (getenv "PATH"))
	 (path-list (unconcat path ";"))
	 )
    (cond
     ((member-if '(lambda (x) (path-element= p x)) path-list)
      (setq path-list (delete-if '(lambda (x) (path-element= p x)) path-list))
      (setenv "PATH" (mconcat path-list ";"))
      )
     )
    )
  )

(defun path-delete-regexp (pat)
  (let* ((path-list (delete-string-from-list-regexp pat (path-split)))
	 )
    (setenv "PATH" (mconcat path-list ";"))
    )
  )

(defun path-show () (interactive)
  (show (mconcat (path-split) "\n"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-split (list item &optional after)
  "Break LIST into two parts so that ITEM is the head of the second part.
If optional AFTER is non-nil then ITEM will be tail of first part."
  (let* ((tail (member item list)) n head)
    (setq n (length tail))
    (cond (after
	   (setq tail (cdr tail))
	   (setq n (1+ n))
	   )
	  )
    (setq head (reverse (nthcdr n (reverse list))))
    (list head tail)
    )
  )

(defun list-split-insert (dest insertion insertion-point-item &optional after)
  (let* ((split (list-split dest insertion-point-item after)))
    (append (car split) insertion (cadr split))
    )
  )
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun path-set-save (path)
  (let* ((s (unconcat path ";"))
	 )
    (setq s (mapcar '(lambda (x) (format "\"%s\"" (slash-back-double x))) s))
    (format "\
(defun path-restore ()
  (path-set
   `(
     %s
     )))" (cat s "\n     "))
    )
  )

; (show (path-set-save (getenv "PATH")))
