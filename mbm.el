;; some of these functions make use of variables
;; from higher scopes. This is largely avoidable but
;; may be corrected later.
;;
;; the mbm-buffer consists of forms which expand to other forms
;; these are (plist inclusion targets)
;; each target is of the form (tag . spec)
;; where spec may be a filename, or a template which expands
;; to a buffer or filename#
;;

(setq mbm-map (make-sparse-keymap))
(define-key global-map (kbd "M-'") mbm-map)

(setq mbm-bmap (make-sparse-keymap))

(defun mbm-define (key tag)
  (define-key mbm-map (kbd key) 'mbm-visit)
  (define-key mbm-bmap (kbd key) tag)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mdotimes (i 26)
  (let* ((c (char-to-string (+ ?a i))))
    (mbm-define
     (format "M-%s" c) (intern c))
    ))
		
(mdotimes (i 10)
  (let* ((c (char-to-string (+ ?0 i))))
    (mbm-define
     (format "M-%s" c) (intern c))
    ))

(mbm-define "<M-backspace>" 'buried)

;(mbm-define "M-SPC" 'meta)

(mbm-define "M-'" 'other)
(mbm-define "M-/" 'alt)

(mbm-define "M-#" 'next)
(mbm-define "M-;" 'prev)

(mbm-define "M-." 'down)
(mbm-define "M-," 'up)

(mbm-define "M-[" 'first)
(mbm-define "M-]" 'last)

;(mbm-define "M-," 'nil)
;(mbm-define "M-." 'nil)
;(mbm-define "M-]" 'nil)
;(mbm-define "M-{" 'nil)
;(mbm-define "M-}" 'nil)
;(mbm-define "M-@" 'nil)
;(mbm-define "M-:" 'nil)
;(mbm-define "M-~" 'nil)
;(mbm-define "M-?" 'nil)
;(mbm-define "M-<" 'nil)
;(mbm-define "M->" 'nil)
;(mbm-define "M-=" 'nil)
;(mbm-define "M-+" 'nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mbm-file-name ".mbm.el" "The name of the file containing the meta-quote forms")
(defvar mbm-global-file-name ".mbm.el" "The name of the file containing the meta-quote forms")

(defun mbm-file-buffer ()
  (find-file-noselect (locate-up-file mbm-file-name)))

(defun mbm-buffer-name (&optional buffer)
  (or (buffer-file-name) (buffer-name buffer))
  )

(defun mbm-buffer-switch (buffer &optional arg)
  (setq mbm-prev-buffer (current-buffer))
  (cond
   (arg (switch-to-buffer-other-window (buffer buffer)))
   (t (switch-to-buffer (buffer buffer)))
   )
  )

(defun mbm-find-link (name tag)
  (let ((buffer (mbm-file-buffer)))
    (sx 
     (set-buffer buffer)
     (bob)
     (catch 'done
       (while t
	 (let* ((form (eval (read buffer)))
		(result)
		)
;	   (debug)
	   (cond
	    ((null form) (throw 'done nil))
	    ((setq result (mbm-eval form name tag))
	     (and result (throw 'done result))
	     )
	    )
	   )
	 )
       )
     )
    )
  )

(defun mbm-visit (arg)
  (interactive "P")
  (let* ((keys (this-command-keys))
	 (key (substring keys 1))
	 (tag (lookup-key mbm-bmap key))
	 (buffer
	  (or
	   (mbm-find-link (mbm-buffer-name) tag)
	   )
	  )
	 )
    (and buffer (mbm-buffer-switch buffer arg))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mbm-reveal-1 (form element name)
;  (debug)
  (let* ((plist (nth 0 form))
	 (tag (car element))
	 (select (eval (cdr element)))
	 (new)
	 )
    (setq new
      (cond
       ((stringp select) (mbm-expand select name))
       )
      )
    (cons tag new)
    )
  )

(defun mbm-reveal (form name)
  (let* ((plist (nth 0 form))
	 (forms (nthcdr 2 form))
	 (match-fun (mbm--get-match-fun plist))
	 (pac-fun (or (plist-get plist :pac) 'mbm--pac-generic))
	 (regexp (plist-get plist :regexp))
	 (match-arg)

	 (mbm-plist plist)
	 (mbm-forms forms)
	 (mbm-match-fun match-fun)
	 (mbm-pac-fun pac-fun)
	 (mbm-match-arg)
	 (result)
	 )

    (setq match-arg (funcall pac-fun (nth 1 form) forms))
    (setq mbm-match-arg match-arg)

    (setq match-result (funcall match-fun name match-arg regexp))
    (cond
     (match-result
;      (debug)
      (setq result (mapcar '(lambda (x) (mbm-reveal-1 form x name)) forms))
      )
     )
    result
    )
  )

(defun mbm-show* (cbuf)
  (let* ((name (mbm-buffer-name cbuf))
	 (buffer (mbm-file-buffer))
	 (result)
	 )
    (sx 
     (set-buffer buffer)
     (bob)
     (catch 'done
       (while t
;	 (debug)
	 (let* ((form (eval (read buffer)))
		)
;	   (debug)
	   (cond
	    ((null form) (throw 'done nil))
	    ((setq result (append (mbm-reveal form name) result)))
	    )
	   )
	 )
       )
     )
    (let ((show-buffer (get-buffer-create "*mbm*"))
	  (map (copy-keymap mbm-show-map))
	  )
      (set-buffer show-buffer)
      (read-only-mode 0)
      (erase-buffer)
      (insert (format "Directory: %s\n\n" default-directory))
      (dolist (i result)
	(let* ((tag (car i))
	       (target (cdr i))
	       (key (where-is-internal tag mbm-bmap t))
	       (key-desc (key-description key))
	       (fun `(lambda (arg) (interactive "p") (mbm-show-jump ,target)))
	       )
	  (insert (format "%-8s %-10s %s\n" key-desc tag (cdr i)))
	  (define-key map key fun)

;;	  (define-key map (substring key-desc 2) fun)
	  )
	)
      (set-buffer-modified-p nil)
      (read-only-mode)
      (bob)
      (goto-line 3)
      (use-local-map map)
      (pop-to-buffer show-buffer)
      )
    )
  )

(defun mbm-show () (interactive)
  (let* ((cbuf (current-buffer)))
    (mbm-show* cbuf)
    )
  )

;;; display-buffer options abound

(defun mbm-show-find-here ()
  (interactive)
  (let* ((keys (this-command-keys))
	 (name (bs (+ (point^) 20) (point$)))
	 (buffer (get-buffer name))
	 (alist `(nil . ((inhibit-same-window . t))))
	 )
    (cond
     ((bufferp buffer))
     ((setq buffer (find-file-noselect name)))
     )
    (cond
     ((string= keys "o")
      (display-buffer buffer alist)
      )
     ((string= keys "f")
      (display-buffer-reuse-window buffer '((inhibit-same-window . t)))
      )
     ((string= keys (kbd "RET"))
      (display-buffer-same-window buffer nil)
      )
     
     )
    )
  )

(defun mbm-show-refresh ()
  (interactive)
  (let* ((cbuf (swx (other-window 1) (current-buffer))))
    (mbm-show* cbuf)
    )
  )
       

(setq mbm-show-map (make-sparse-keymap))
(define-key mbm-show-map "q" 'kill-current-buffer)
(define-key mbm-show-map "o" 'mbm-show-find-here)
(define-key mbm-show-map "f" 'mbm-show-find-here)
(define-key mbm-show-map "g" 'mbm-show-refresh)
(define-key mbm-show-map (kbd "RET") 'mbm-show-find-here)

(defun mbm-open-target (target)
  (find-file-noselect target)
  )

(defun mbm-show-jump (target)
  "Switch to buffer designated by TARGET. "
  (let* ((buffer (current-buffer))
	 (new-buffer (mbm-open-target target))
	 )
    (cond
     (t
      (display-buffer-use-some-window new-buffer '((inhibit-same-window . t)))
      )
     (t
      (kill-buffer buffer)
      (display-buffer-same-window new-buffer nil)
      )
     )
    )
  )

(define-key mbm-map (kbd "M-h") 'mbm-show)
(define-key mbm-map (kbd "M-SPC") 'mbm-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mbm-cycle (plist files)
  (let* ((f (copy-list files))
	 (r (reverse f))
	 )
    (nconc f (copy-list f))
    (nconc r (copy-list r))
    `(,plist ,files
;	   ()
	   (first . (car ',f))
	   (last . (car ',r))
	   (next . (cadr (mbm--name-member name ',f)))
	   (prev . (cadr (mbm--name-member name ',r)))
	   ))
  )

(defun mbm--name-equal (name target &optional regexp)
  (cond
   (regexp (string-match target name))
   ((equal name target))
   )
  )

(defun mbm--name-member (name list &optional regexp)
  (or
   (member-if '(lambda (y)
		 (mbm--name-equal name (expand-file-name y) regexp)) list)
   (member-if '(lambda (y)
		 (mbm--name-equal name (file-name-nondirectory y) regexp)) list)
   )
  )

(defun mbm--match-generic (buffer-name target &optional regexp)
  (cond
   ((stringp target)
    (mbm--name-equal buffer-name target regexp))
   ((listp target)
    (mbm--name-member buffer-name target regexp))
   )
  )

(defun mbm--match-all (buffer-name target &optional regexp)
  t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pac-fun creates the predicate arglist

(defun mbm--pac-generic (arg forms)
  (let ((list
	 (cond
	  (arg)
	  (t (mapcar 'cdr forms))
	  )))
    list)
  )

(defun mbm-apply (form name tag match) 
;  (debug)
  (let* ((plist (nth 0 form))
	 (forms (nthcdr 2 form))
	 (element (assq tag forms))
	 (select (eval (cdr element)))
	 (new)
	 )
    (setq new
      (cond
       ((stringp select) (mbm-expand select name))
       )
      )
    new
    )
  )

(defun mbm--get-match-fun (plist)
  (let* ((prop (plist-get plist :match)))
    (cond
     ((eq prop t) 'mbm--match-all)
     (prop)
     ('mbm--match-generic)
     )
    )
  )

(defun mbm-eval (form name tag)
  (let* ((plist (nth 0 form))
	 (forms (nthcdr 2 form))
	 (match-fun (mbm--get-match-fun plist))
	 (pac-fun (or (plist-get plist :pac) 'mbm--pac-generic))
	 (regexp (plist-get plist :regexp))
	 (match-arg)

	 (mbm-plist plist)
	 (mbm-forms forms)
	 (mbm-match-fun match-fun)
	 (mbm-pac-fun pac-fun)
	 (mbm-match-arg)
	 )

    (setq match-arg (funcall pac-fun (nth 1 form) forms))
    (setq mbm-match-arg match-arg)

    (setq match-result (funcall match-fun name match-arg regexp))
    (cond
     (match-result (mbm-apply form name tag match-result))
     )
    )
  )

;; uses name from higher scope

(defun mbm--replace (s) ;; &implicit name
;   (debug)
  (save-match-data
    (let ((x (string-match-string "<\\(.*?\\)>" s 1)))
      (cond
       ((string= x "%B") (basename name))
       (x)
       )
      )
    )
  )

(defun mbm-expand (ss name)
  (replace-regexp-in-string "\\(<.*?>\\)" 'mbm--replace ss)
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defin 'mbm-cycle-plus)

(defun mbm-expansion-to-regexp (s)
  "Convert mbm-expansion template to the corresponding regexp.
There will not always be a conversion, so understand how to use this."
 (save-match-data
   (cond
    ((string-match "<%B>" s) (concat "\\" (substring s (match-end 0)) "$"))
    (s)
    )
   )
 )

(defun mbm-apply-cycle ()  ;; implicit name
  (let* ((fun #'(lambda (elt name) (string-match (mbm-expansion-to-regexp (cdr elt)) name)))
	 (filter (delete-if-not '(lambda (x) (stringp (cdr x))) (copy-list forms)))
	 mf mr
	 )
  (save-match-data
;     (debug)
    (setq foo
      (case tag
	(next (list-next filter name fun))
	(prev (list-prev filter name fun))
	(first (first filter))
	(last (car (last filter)))
	)
      )
    (cond
     (foo (mbm-expand (cdr foo) name))
     )
    )
  )
  )

(defun mbm-cycle-plus (plist inclusion target-forms)
  "Create a random access sequence of switch targets."
  (let ((inclusion (or inclusion (mapcar 'cdr target-forms))))
    (setq inclusion (mapcar 'mbm-expansion-to-regexp inclusion))
    (setq plist (plist-put plist :regexp t))
    (setq target-forms
      (append target-forms
	      `((next . (mbm-apply-cycle))
		(prev . (mbm-apply-cycle))
		(first . (mbm-apply-cycle))
		(last . (mbm-apply-cycle))
		)
	      )
      )
    `(,plist ,inclusion ,@target-forms)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mbm-include (name)
  (error "this isn't ready yet")
  )
