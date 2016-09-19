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

(defun mq-linked-file-name-match (name match &optional fun)
  (cond
   ((not name) nil)
   ((eq match nil) name)
   ((eq match 'eval) (eval fun))
   ((eq match 'list) (or (member name fun) (member (file-name-nondirectory name) fun)))
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

(defun mq-visit-linked-file-name (name tag)
  (set-buffer (buffer mq-file-name))
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
	  (setq a (assq tag forms))
	  (and a (throw 'done (eval (cdr a))))
	  )
	 )
	)
      )
    )
  )

(defun mq-visit-linked-file (arg)
  (interactive "P")
  (let* ((keys (this-command-keys))
	 (key (substring keys (cond (arg 2) (1))))
	 (tag (lookup-key mq-bmap key))
	 (buffer (sx (mq-visit-linked-file-name (buffer-file-name) tag)))
	 )
    (cond
     (arg (switch-to-buffer-other-window (buffer buffer)))
     (t (switch-to-buffer (buffer buffer)))
     )
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
		
(mq-linked-file "M-'" 'other)
(mq-linked-file "M-[" 'up)
(mq-linked-file "M-/" 'down)
(mq-linked-file "M-#" 'next)
(mq-linked-file "M-;" 'prev)

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
    `(list ,files ()
	   (next . (cadr (member (file-name-nondirectory name) ',f)))
	   (prev . (cadr (member (file-name-nondirectory name) ',r)))
	   ))
  )
