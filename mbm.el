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

(setq mbm-map (make-sparse-keymap))
(define-key global-map (kbd "M-'") mbm-map)

(setq mbm-bmap (make-sparse-keymap))

(defun mbm-visit-linked-file (arg) (interactive "P")
  (let* ((key (this-command-keys)))
    (qb-select* key)
    ))

(defun mbm-linked-file (key tag)
  (define-key mbm-map (kbd key) 'mbm-visit-linked-file)
  (define-key mbm-bmap (kbd key) tag)
  )

(defun mbm-file-match (name list)
  (or
   (member-if '(lambda (y) (equal name (expand-file-name y))) list)
   (member (file-name-nondirectory name) list)
   )
  )

(defun mbm-linked-file-name-match (name match &optional fun)
  (cond
   ((eq match t) t)
   ((not name) nil)
   ((eq match nil) name)
   ((eq match 'eval) (eval fun))
   ; fun may be a list of files to try and match
   ((eq match 'list)
;    (debug)
    (mbm-file-match name fun))
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

(defvar mbm-file-name ".mbm.el" "The name of the file containing the meta-quote forms")

(defun mbm-file-buffer () (find-file-noselect (locate-up-file mbm-file-name)))

(defun mbm-visit-linked-file-name (name tag)
  (let ((buffer (mbm-file-buffer)))
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
	    ((mbm-linked-file-name-match name match fun)
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

(defun mbm-visit-linked-file-name-poop (name tag)
  (cond
   ((eq tag 'buried) mbm-prev-buffer)
   )
  )

(defun mbm-buffer-switch (buffer &optional arg)
  (setq mbm-prev-buffer (current-buffer))
  (cond
   (arg (switch-to-buffer-other-window (buffer buffer)))
   (t (switch-to-buffer (buffer buffer)))
   )
  )

(defun mbm-buffer-name ()
  (or (buffer-file-name) (buffer-name))
  )

(defun mbm-visit-linked-file (arg)
  (interactive "P")
  (let* ((keys (this-command-keys))
;	 (key (substring keys (cond (arg 2) (1))))	; not in emacs 25
	 (key (substring keys 1))
	 (tag (lookup-key mbm-bmap key))
	 (buffer
	  (or
	   (mbm-visit-linked-file-name (mbm-buffer-name) tag)
	   (mbm-visit-linked-file-name-poop (mbm-buffer-name) tag)
	   )
	  )
	 )
    (mbm-buffer-switch buffer arg)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mdotimes (i 26)
  (let* ((c (char-to-string (+ ?a i))))
    (mbm-linked-file
     (format "M-%s" c) (intern c))
    ))
		
(mdotimes (i 10)
  (let* ((c (char-to-string (+ ?0 i))))
    (mbm-linked-file
     (format "M-%s" c) (intern c))
    ))

(mbm-linked-file "<M-backspace>" 'buried)

(mbm-linked-file "M-SPC" 'meta)

(mbm-linked-file "M-'" 'other)
(mbm-linked-file "M-/" 'alt)

(mbm-linked-file "M-#" 'next)
(mbm-linked-file "M-;" 'prev)

(mbm-linked-file "M-." 'down)
(mbm-linked-file "M-," 'up)

(mbm-linked-file "M-[" 'first)
(mbm-linked-file "M-]" 'last)

;(mbm-linked-file "M-," 'nil)
;(mbm-linked-file "M-." 'nil)
;(mbm-linked-file "M-]" 'nil)
;(mbm-linked-file "M-{" 'nil)
;(mbm-linked-file "M-}" 'nil)
;(mbm-linked-file "M-@" 'nil)
;(mbm-linked-file "M-:" 'nil)
;(mbm-linked-file "M-~" 'nil)
;(mbm-linked-file "M-?" 'nil)
;(mbm-linked-file "M-<" 'nil)
;(mbm-linked-file "M->" 'nil)
;(mbm-linked-file "M-=" 'nil)
;(mbm-linked-file "M-+" 'nil)

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
(defun mbm-cycle (plist files)
  (let* ((f (copy-list files))
	 (r (reverse f))
	 )
    (nconc f f)
    (nconc r r)
    `(list ,files
;	   ()
	   (first . (car ',f))
	   (last . (car ',r))
	   (next . (cadr (mbm-file-match name ',f)))
	   (prev . (cadr (mbm-file-match name ',r)))
	   ))
  )

(defun mbm-list (plist parg &rest forms)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mbm-active nil "A set of symbols representing enabled clauses")

(defun mbm-active-toggle (tag)
  (cond
   ((membm tag mbm-active) (setq mbm-active (delq tag mbm-active)))
   ((add-to-list 'mbm-active tag))
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mbm-show () (interactive)
  (let ((name (mbm-buffer-name))
	(map nil))
    (set-buffer (mbm-file-buffer))
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
	   ((mbm-linked-file-name-match name match fun)
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

(define-key mbm-map (kbd "M-h") 'mbm-show)

