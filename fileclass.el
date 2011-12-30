;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; if a buffer has set its class, that is its class, no argument
; next is any pattern in the buffer contents which identifies its class
; then is a default class identification pattern (not done yet)
; then we try the filename against known patterns
; then we match the major-mode against patterns

(defvar file-class nil)
(make-local-variable 'file-class)

(setq file-class 'emacs-lisp-init)

(defun file-class-guess (name)
  (sx
   (bob)
   (or
    (and (not (file-directory-p name))
	 (catch 'got
	   (dolist (i file-class-guess-pattern-alist)
	     (cond ((rsf (cdr i)) (throw 'got (car i))))
	     )
	   )
	 )
    (car (rassoc-if '(lambda (x) (string-match x name)) file-class-guess-name-alist))
    major-mode
    )
   )
  )

(defun file-class (name)
  (or file-class (file-class-guess name))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq file-class-guess-name-alist)

(defun file-class-guess-name-add (class pattern)
  (setq file-class-guess-name-alist
    (gput file-class-guess-name-alist class pattern))
  )

(setq file-class-guess-pattern-alist)

(defun file-class-guess-pattern-add (class pattern)
  (setq file-class-guess-pattern-alist
    (gput file-class-guess-pattern-alist class pattern))
  )

; this is actually for class and major-mode

(setq file-class-linked-file-alist nil)

(defun file-class-linked-file-add (class plist)
  (setq file-class-linked-file-alist
    (gput file-class-linked-file-alist class plist))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar file-linked-plist nil)
(make-variable-buffer-local 'file-linked-plist)

(set-default 'file-linked-plist nil)

(defun linked-file-search (tag)
  (sx
   (bob)
   (cond
    ((rsf (format "~qb~%s~" tag))
     (let ((s (find-match-string "[^~]*")))
       (cond
	((string-match "(" s) (eval (read s)))
	(s)
	)
       )
     ))
   )
  )

(defun linked-file-class-get (tag name)
  (let* (
	 (class (file-class name))
	 (fun
	  (or
	   (gget1 file-class-linked-file-alist class tag)
	   (gget1 file-class-linked-file-alist class t)
	   )
	  )
	 )
    (cond (fun (funcall fun)))
    )
  )

(defun linked-file-mode-get (tag name)
  (let* ((fun
	  (or
	   (gget1 file-class-linked-file-alist major-mode tag)
	   (gget1 file-class-linked-file-alist major-mode t)
	   )
	 )
    (cond (fun (funcall fun)))
    )
  )

(defun linked-file-put (tag name)
  (setq file-linked-plist (alist-put file-linked-plist tag name))
  )

(defun linked-file-get (tag)
  (alist-get file-linked-plist tag)
  )

(defun linked-file (tag &optional name)
  (or name (setq name (or (buffer-file-name) dired-directory (buffer-name))))
  (or
   (linked-file-get tag)
   (linked-file-search tag)
   (linked-file-class-get tag name)
   (linked-file-mode-get tag name)
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun path-replace-until-found (fun offset name max)
  "Call FUN maximum (MAX) number of times modifying NAME until it returns a file that exists.
OFFSET is passed to FUN."
  (or name (setq name (buffer-file-name)))
  (debug)
  (let ((x))
    (while
	(and (>= (setq max (1- max)) 0) (not x))
      (setq name (funcall fun offset name))
      (cond ((file-exists-p name) (setq x name)))
      )
    x)
  )

