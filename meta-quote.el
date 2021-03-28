;;;; FIX THIS - RUINED

;; every form in the file evaluates to a form
;; (list LIST FUN FORMS ...     ;; !! FUN not currently present
;; (eval EXPR FORMS ...
;; (NAME FUN FORMS ...
;; the FORMS are the mapping of keys to target files
;;   (key . EXPR) expr is evalled to return the target file
;;   when key is used
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mq-map (make-sparse-keymap))
(define-key global-map (kbd "M-'") mq-map)

(setq mq-bmap (make-sparse-keymap))

(defun mq-visit-linked-file (arg) (interactive "P")
  (let* ((key (this-command-keys)))
    (qb-select* key)
    ))

(defun mq-linked-file (key tag)
  (define-key mq-map (kbd key) 'mq-visit-linked-file)
  (define-key mq-bmap (kbd key) tag)
  )

(defun mq-file-match (name list)
  (or
   (member-if '(lambda (y) (equal name (expand-file-name y))) list)
   (member (file-name-nondirectory name) list)
   )
  )

(defun mq-linked-file-name-match (name match &optional fun)
  (cond
   ((eq match t) t)
   ((not name) nil)
   ((eq match nil) name)
   ((eq match 'eval) (eval fun))
   ; fun may be a list of files to try and match
   ((eq match 'list)
    (debug)
    (mq-file-match name fun))
   ((stringp name)
    (setq fun
      (cond
       (fun)
       ((string-match "\\*" match) 'string-match)
       ('string=)
       )
      )
    (or
     (funcall fun match name)
     (funcall fun match (file-name-nondirectory name))
     )
    )
   )
  )

(defvar mq-file-name ".mq.el" "The name of the file containing the meta-quote forms")

(defun mq-file-buffer () (find-file-noselect (locate-up-file mq-file-name)))

(defun mq-visit-linked-file-name (name tag)
  (let ((buffer (mq-file-buffer)))
    (sx 
     (set-buffer buffer)
     (bob)
     (catch 'done
       (while t
	 (let* ((form (eval (read buffer)))
		(match (car form))
		(fun (nth 1 form))
		(forms (nthcdr 2 form))
		a
		)
	   (cond
	    ((null form) (throw 'done nil))
	    ((mq-linked-file-name-match name match fun)
	     (setq a (assq tag forms))
	     (and a (throw 'done (eval (cdr a))))
	     )
	    )
	   )
	 )
       )
     )
    )
  )

(defun mq-visit-linked-file-name-poop (name tag)
  (cond
   ((eq tag 'buried) mq-prev-buffer)
   )
  )

(defun mq-buffer-switch (buffer &optional arg)
  (setq mq-prev-buffer (current-buffer))
  (cond
   (arg (switch-to-buffer-other-window (buffer buffer)))
   (t (switch-to-buffer (buffer buffer)))
   )
  )

(defun mq-visit-linked-file (arg)
  (interactive "P")
  (let* ((keys (this-command-keys))
;	 (key (substring keys (cond (arg 2) (1))))	; not in emacs 25
	 (key (substring keys 1))
	 (tag (lookup-key mq-bmap key))
	 (buffer
	  (or
	   (mq-visit-linked-file-name (buffer-file-name) tag)
	   (mq-visit-linked-file-name-poop (buffer-file-name) tag)
	   )
	  )
	 )
    (mq-buffer-switch buffer arg)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mdotimes (i 26)
  (let* ((c (char-to-string (+ ?a i))))
    (mq-linked-file
     (format "M-%s" c) (intern c))
    ))
		
(mdotimes (i 10)
  (let* ((c (char-to-string (+ ?0 i))))
    (mq-linked-file
     (format "M-%s" c) (intern c))
    ))

(mq-linked-file "<M-backspace>" 'buried)

(mq-linked-file "M-SPC" 'meta)

(mq-linked-file "M-'" 'other)
(mq-linked-file "M-/" 'alt)

(mq-linked-file "M-#" 'next)
(mq-linked-file "M-;" 'prev)

(mq-linked-file "M-." 'down)
(mq-linked-file "M-," 'up)

(mq-linked-file "M-[" 'first)
(mq-linked-file "M-]" 'last)

;(mq-linked-file "M-," 'nil)
;(mq-linked-file "M-." 'nil)
;(mq-linked-file "M-]" 'nil)
;(mq-linked-file "M-{" 'nil)
;(mq-linked-file "M-}" 'nil)
;(mq-linked-file "M-@" 'nil)
;(mq-linked-file "M-:" 'nil)
;(mq-linked-file "M-~" 'nil)
;(mq-linked-file "M-?" 'nil)
;(mq-linked-file "M-<" 'nil)
;(mq-linked-file "M->" 'nil)
;(mq-linked-file "M-=" 'nil)
;(mq-linked-file "M-+" 'nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun file-name-suffix-match (pat name)
  (string= (file-name-suffix name) pat)
  )

(defun file-name-partial (pat name)
  (string= (file-name-suffix name) pat)
  )

(defun file-name-suffix-rename (name suffix)
  (concat (file-name-base name) suffix)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mq-cycle (files)
  (let* ((f (copy-list files))
	 (r (reverse f))
	 )
    (nconc f f)
    (nconc r r)
    `(list ,files
;	   ()
	   (first . (car ',f))
	   (next . (cadr (mq-file-match name ',f)))
	   (prev . (cadr (mq-file-match name ',r)))
	   ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mq-active nil "A set of symbols representing enabled clauses")

(defun mq-active-toggle (tag)
  (cond
   ((memq tag mq-active) (setq mq-active (delq tag mq-active)))
   ((add-to-list 'mq-active tag))
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mq-show () (interactive)
  (let ((name (buffer-file-name))
	(map nil))
    (set-buffer (mq-file-buffer))
    (bob)
    (catch 'done
      (while t
	(let* ((form (eval (read (current-buffer))))
	       (match (car form))
	       (fun (nth 1 form))
	       (forms (nthcdr 2 form))
	       a
	       )
	  (cond
	   ((null form) (throw 'done nil))
	   ((mq-linked-file-name-match name match fun)
	    (dolist (i forms) (setq map (alist-put map (car i) (eval (cdr i)))))
	    )
	   )
	  )
	)
      )
    (setq map (nreverse map))
    (show (cat (flatten (mapcar '(lambda (x) (format "%-10s %s" (car x) (cdr x))) map)) "\n"))
    )
  )

(define-key mq-map (kbd "C-h") 'mq-show)

