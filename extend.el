(defun set-if-not-bound (sym value)
  (or (boundp sym) (set sym value))
  )

(defun file-if-exists (name &optional dir)
  (and dir (setq name (concat dir name)))
  (cond ((file-exists-p name) name)))

(defun locate-alternate-files-in-path (names paths)
  "Return the first file found which satisfies both
membership in NAMES and PATHS"
  (catch 'done
    (mapcar '(lambda (dir)
	       (mapcar '(lambda (name)
			  (let ((f (concat (or dir ".") "/" name)))
			    (cond ((file-exists-p f) (throw 'done f)))
			    )) names)) paths)
    nil
    ))

(defun locate-file-in-path (name path)
  "Return the first file called NAME found in one of the members of PATHS"
  (catch 'done
    (mapcar '(lambda (dir)
	       (let ((f (concat dir "/" name)))
		 (cond ((file-exists-p f) (throw 'done f)))
		 )) path)
    )
  )

(defun locate-up-file-directory (file &optional max)
  "Locate a directory above the current one that contains a particular FILE."
  (let* ((cd default-directory)
	 nd
	 (max (or max 255))
	 )
    (while (and cd (not (file-exists-p (format "%s/%s" cd file))))
      (setq nd (expand-file-name (format "%s/.." cd)))
      (setq max (1- max))
      (cond
       ((< max 0) nil)
       ((string-match "^//\\sw+/*$" nd) (setq cd nil))
       ((string= nd cd) (setq cd nil))
       ((setq cd nd))
       )
      )
    cd)
  )

(defun locate-up-file (file &optional default max)
  "Locate a file in current directory or directory above."
  (let ((cd (locate-up-file-directory file max)))
    (cond (cd (expand-file-name (format "%s/%s" cd file)))
	  (default)
	  )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond ((not (fboundp 'adjoin)) 
       (defun adjoin (x y) (if (member x y) y (cons x y)))
       ))

(defmacro mdt (-i -n &rest body)
  `(let ((result))
     (dotimes (,-i ,-n (nreverse result))
       (setq result (cons (progn ,@body) result)))
     ))

(defmacro mdl (-i -list &rest body)
  `(let ((result))
     (dotimes (,-i ,-list (nreverse result))
       (setq result (cons (progn ,@body) result)))
     ))

(defun adjoin-accessible-directory (dir list)
  (if (file-accessible-directory-p dir)
      (cons dir list) list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-put (list tag val)
  "Like plist-put but acts on alists. Should really be built-in."
  (let ((cell (assoc tag list)))
    (cond
     (cell (setcdr cell val))
     ((setq list (cons (cons tag val) list)))
     )
    list)
  )

(defun alist-r-get (list tag)
  (cdr (assoc tag list)))

(defun alist-r-get-if (list fun)
  (cdr (assoc-if fun list)))

(defun alist-remprop (list tag)
  (delete-if '(lambda (x) (equal (car x) tag)) list))

(defun alist-merge (list &rest lists)
  (dolist (i lists)
    (dolist (a i)
      (setq list (alist-put list (car a) (cdr a)))
      )
    )
  list
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the new plist-merge function isn't really tested yet

(defun plist-merge-old (plist &rest lists)
  "Merge multiple property lists. Later items take priority"
  (dolist (list lists)
    (while list
      (let* ((prop (pop list))
	     (value (pop list))
	     )
	(setq plist (plist-put plist prop value))
	)
      )
    )
  plist
  )

(defun plist-merge (plist &rest list)
  "Merge multiple property lists. Later items take priority"
  (while list
    (let* ((prop (pop list)))
      (cond
       ((consp prop) (setq plist (apply 'plist-merge plist prop)))
       ((eq prop :merge) (setq plist (plist-merge plist (pop list))))
       ((setq plist (plist-put plist prop (pop list))))
       )
      )
    )
  plist
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gget (o tag)
  "Generalized symbol or plist or alist get (gget OBJECT TAG)"
  (cond ((symbolp o) (get o tag))
	((listp o) (cdr (assoc tag o))))
  )

(defun gput (o tag val)
  "Generalized symbol or plist or alist put (gput OBJECT TAG VALUE).
If the object is a possibly empty list it must be assigned on return,
otherwise acts by side-effect"
  (cond
   ((null o) (list (cons tag val)))
   ((listp o) (alist-put o tag val))
   ((symbolp o) (get o tag))
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-slice (alist keys)
  "Return a copy of ALIST containing only elements matching thos in KEYS" 
  (let* ((o (mapcar '(lambda (x) (cons x (alist-get x alist))) keys)))
    o)
  )

(defun alist-sort (alist &optional fun)
  "Sort ALIST using FUN as compare function (by default by key lexical value)
list is modified as side effect, so you should assign this function result.
"
  (or fun (setq fun '(lambda (x y) (string> (car x) (car y)))))
  (setq alist (sort alist fun))
  alist)

(defun alist-to-args (alist)
  "Convert ALIST to a list of program parameter elements"
  (mconcat
   (mapcar '(lambda (x) (format "--%s=\"%s\"" (car x) (cdr x))) alist)
   " "
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun glist-put (list key val &optional fun) 
  (let ((cell (assoc key list fun)))
    (cond
     (cell (setcdr cell val))
     ((setq list (cons (cons key val) list)))
     )
    list)
  )

(defmacro glist-put-sym (place key val &optional fun)
  `(setq ,place
     (glist-put ,place ,key ,val ,fun)
     ))

(defun glist-get (list key &optional fun) 
  (let ((cell (assoc key list fun)))
    (cdr cell)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
