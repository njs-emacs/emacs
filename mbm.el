;; name
;; cbuf
;; cbuf-name

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun file-name-prepare-for-compare (name)
  (setq name (string-sub name "\\\\" "/"))
  (setq name (replace-regexp-in-string "^//" "||" name))
  (setq name (replace-regexp-in-string "/+" "/" name))
  (setq name (replace-regexp-in-string "^||" "//" name))
  (setq name (downcase name))
  name)

(defun file-names-equal (a b)
  (setq a (file-name-prepare-for-compare a))
  (setq b (file-name-prepare-for-compare b))
  (string= a b)
  )

;;; (file-name-prepare-for-compare "\\m")
;;; 
;;; (replace-regexp-in-string "/+" "/" "//yo//mama" nil nil nil 2)
;;; (file-name-prepare-for-compare "//yo//mama")
;;; 
;;; (file-names-equal "/hello" "/hello")
;;; (file-names-equal "hello/P" "hello/p")
;;; (file-names-equal "hello/Po" "hello/pO")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mbm-buffer-name (&optional buffer)
  "Return the name (as mbm wishes to treat it) of BUFFER."
  (or (buffer-file-name buffer) (buffer-name buffer))
  )

;;; !!! what happened to REGEXP

(defun mbm--name-equal (cbuf target &optional regexp)
  "Return non-nil if the mbm-buffer-name of CBUF matches TARGET."
  (let* ((b-file-name (mbm-buffer-name cbuf))
	 (b-buf-name (buffer-name cbuf))
	 (x-target (expand-file-name target))
	 )
    (cond
     (regexp
      (string-match target b-buf-name)
      )
     (t
      (cond
       ((file-name-absolute-p target)
	(file-names-equal b-file-name target)      ; target could be anywhere
	)
       ((file-names-equal b-file-name x-target))   ; target relocated to current directory
       )
      )
     )
    )
  )

(defun mbm--name-member (cbuf target &optional regexp)
  (or
   (member-if '(lambda (y)
		 (mbm--name-equal cbuf (expand-file-name y) regexp)) target)
   (member-if '(lambda (y)
		 (mbm--name-equal cbuf (file-name-nondirectory y) regexp)) target)
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mbm-file-name ".mbm.el" "The name of the file containing the meta-quote forms")
(defvar mbm-file-buffer nil "The buffer containing the meta-quote forms. Normally loaded from local-home or $HOME")
(defvar mbm-show-current-buffer nil "The buffer which was active when mbm show buffer was last refreshed")

(defun mbm-file-open (file)
  (interactive (list 
		(let ((default ".mbm.el"))
		  (expand-file-name (read-file-name "File: " nil default t default)))))
  (let ((buffer
	 (cond
	  ((buffer-live-p mbm-file-buffer)
	   (cond
	    ((file-names-equal (buffer-file-name mbm-file-buffer) file)
	      mbm-file-buffer)
	    (t
	     (kill-buffer mbm-file-buffer)
	     (mbm-file-open file)
	     )
	    )
	   )
	  (t
	   (setq mbm-file-name file)
	   (setq mbm-file-buffer (find-file-noselect file))
	   )
	  )
	 ))
    buffer)
    )

(defun mbm-file-buffer ()
  (cond
   ((buffer-live-p mbm-file-buffer) mbm-file-buffer)
   ((call-interactively 'mbm-file-open))
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mbm-open-target (target)
  (find-file-noselect target)
  )

(defun mbm-show-jump (target)
  "Switch to buffer designated by TARGET.
Active in show buffer, mapped to the suffix keys of the global bindings"
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
    (mbm-show-buffer-refresh-maybe new-buffer)
    )
  )

(defun mbm-show-find-here ()
  "Open the target buffer described on the current line.\nActive in show buffer."
  (interactive)
  (let* ((keys (this-command-keys))
	 (tname (bs (+ (point^) 20) (point$)))
	 (buffer (get-buffer tname))
	 (alist `(nil . ((inhibit-same-window . t))))
	 )
    (cond
     ((bufferp buffer))
     ((setq buffer (find-file-noselect tname)))
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
    (mbm-show-buffer-refresh-maybe buffer)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mbm-show-map (make-sparse-keymap))

(define-key mbm-show-map "o" 'mbm-show-find-here)
(define-key mbm-show-map "f" 'mbm-show-find-here)
(define-key mbm-show-map (kbd "RET") 'mbm-show-find-here)

(define-key mbm-show-map "g" 'mbm-show-refresh)
(define-key mbm-show-map "q" 'kill-current-buffer)

(define-key mbm-show-map "n" 'forward-line)
(define-key mbm-show-map "p" 'previous-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; match functions, how to determine if a form applies to this buffer

(defun mbm--match-generic (cbuf target &optional regexp)
  (cond
   ((stringp target)
    (mbm--name-equal cbuf target regexp))
   ((listp target)
    (mbm--name-member cbuf target regexp))
   )
  )

(defun mbm--match-all (cbuf target &optional regexp)
  "Match function to determine that any buffer at all can match"
  t
  )

(defun mbm--get-match-fun (plist)
  "Determine which function will be used to see if form applies to buffer."
  (let* ((prop (plist-get plist :match)))
    (cond
     ((eq prop t) 'mbm--match-all)
     (prop)
     ('mbm--match-generic)
     )
    )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mbm-apply (form cbuf tag match)
  (let* ((cbuf-name (mbm-buffer-name cbuf))
	 (plist (nth 0 form))
	 (forms (nthcdr 2 form))
	 (element (assq tag forms))
	 (select (eval (cdr element)))
	 (new)
	 )
    (setq new
      (cond
       ((stringp select) (mbm-expand select cbuf))
       )
      )
    new
    )
  )

(defun mbm-link-try (form cbuf tag)
  (let* ((cbuf-name (mbm-buffer-name cbuf))
	 (plist (nth 0 form))
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

    (setq match-result (funcall match-fun cbuf match-arg regexp))
    (cond
     (match-result (mbm-apply form cbuf tag match-result))
     )
    )
  )

(defun mbm-expand--replace (s)
  "Expand target replacement template."
  (save-match-data
    (let* (;; cbuf from higher scope
	   (cbuf-name (mbm-buffer-name cbuf))
	   (x (string-match-string "<\\(.*?\\)>" s 1)))
      (cond
       ((string= x "%B") (basename cbuf-name))
       (x)
       )
      )
    )
  )

(defun mbm-expand (ss cbuf)
  (replace-regexp-in-string "\\(<.*?>\\)" 'mbm-expand--replace ss)
  )
  
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mbm-show-buffer-name "*mbm*")

(defun mbm-reveal-1 (form element cbuf)
  "Inner function for mbm-reveal."
  (let* ((plist (nth 0 form))
	 (tag (car element))
	 (select (eval (cdr element)))
	 (new)
	 )
    (setq new
      (cond
       ((stringp select) (mbm-expand select cbuf))
       )
      )
    (cons tag new)
    )
  )

(defun mbm-reveal (form cbuf)
  "Resolve a FORM into a buffer target in the context of CBUF."
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

    (setq match-result (funcall match-fun cbuf match-arg regexp))
    (cond
     (match-result
       (setq result (mapcar '(lambda (x) (mbm-reveal-1 form x cbuf)) forms))
      )
     )
    result
    )
  )

(defun mbm-alist-sort (alist)
  "Sort the ALIST to allow the targets to be displayed in a useful consistent way."
  (let* ((sets
	  `(
	    (1 . (first next prev last))
	    )
	  )
	 (ss)
	 )
    (setq foo (mapcar
	       '(lambda (x)
		  (let* ((aa (member-if '(lambda (z) (member (car x) (cdr z))) sets))
			 (ab)
			 )
		    (cond
		     (aa
		      (let ()
			(setq ab (member (car x) (cdar aa)))
			(cons (+ 100000 (* (caar aa) 1000) (- (length (cdar aa)) (length ab))) x))
		      )
		     ((cons (- (string-to-char (symbol-name (car x)))) x))
		     )
		    )
		  ) alist))
    (setq foo (mapcar 'cdr (sort foo '(lambda (x y) (> (car x) (car y))))))
    )
  )

(defun mbm-show-content-fill (cbuf)
  (let* ((buffer (mbm-file-buffer))
	 (result)
	 )
    (sx 
     (set-buffer buffer)
     (bob)
     (catch 'done
       (while t
	 (let* ((form (eval (read buffer)))
		)
	   (cond
	    ((null form) (throw 'done nil))
	    ((setq result (append (mbm-reveal form cbuf) result)))
	    )
	   )
	 )
       )
     )
    (let ((show-buffer (get-buffer-create mbm-show-buffer-name))
	  (map (copy-keymap mbm-show-map))
	  (rplist)
	  (ralist)
	  (line)
	  )
      (set-buffer show-buffer)
      (setq line (max (line-number-at-pos) 4))
      (read-only-mode 0)
      (erase-buffer)
      (insert (format "Directory:          %s\n" default-directory))
      (insert (format "Current:            %s\n\n" (mbm-buffer-name cbuf)))

      (dolist (i result)
	(setq rplist (cons i rplist))
	)
      (dolist (i (nreverse rplist))
	(setq ralist (alist-put ralist (car i) (cdr i)))
	)

      (setq ralist (mbm-alist-sort ralist))

      (dolist (i ralist)
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
      (use-local-map map)
      (goto-line line)
      (setq mbm-show-current-buffer cbuf)
      show-buffer
      )
    )
  )

(defun mbm-show-open ()
  "Open show buffer window."
  (interactive)
       
  (let* ((cbuf (current-buffer))
	 (show-buffer (mbm-show-content-fill cbuf))
	 )
    (pop-to-buffer show-buffer)
    )
  )

;;; display-buffer options abound

(defun mbm-show-refresh ()
  "Refresh the show window on demand."
  (interactive)
  (let* ((cbuf (swx (other-window 1) (current-buffer))))
    (mbm-show-content-fill cbuf)
    )
  )
       
(defun mbm-show-buffer-refresh-maybe (&optional cbuf)
  "Refresh the show window on buffer change if it is visible."
   (let ((show-buf (get-buffer mbm-show-buffer-name)))
    (cond
     ((buffer-live-p show-buf)
      (mbm-show-content-fill cbuf)
      )
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mbm-buffer-switch (nbuf &optional arg)
  (setq mbm-prev-buffer (current-buffer))
  (let ((nbuf (buffer nbuf)))
    (cond
     (arg (switch-to-buffer-other-window nbuf))
     (t (switch-to-buffer nbuf))
    )
    (mbm-show-buffer-refresh-maybe (buffer nbuf))
    )
  )

(defun mbm-find-link (cbuf tag)
  "Try to find a matching form for CBUF and TAG in the mbm file."
  (let* ((mbuf (mbm-file-buffer)))
    (sx 
     (set-buffer mbuf)
     (bob)
     (catch 'done
       (while t
	 (let* ((form (eval (read mbuf)))
		(result)
		)
	   (cond
	    ((null form) (throw 'done nil))
	    ((setq result (mbm-link-try form cbuf tag))
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
  "Try to visit the linked buffer.\nMapped globally in the metaquote map."
  (interactive "P")
  (let* ((cbuf (current-buffer))
	 (keys (this-command-keys))
	 (key (substring keys 1))
	 (tag (lookup-key mbm-bmap key))
	 (tbuf
	  (or
	   (mbm-find-link cbuf tag)
	   )
	  )
	 )
    (and tbuf (mbm-buffer-switch tbuf arg))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mbm-include (name)
  (error "this isn't ready yet")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (mbm-define
     (format "M-' M-%s" c) (intern (format "alt-%s" c)))
    ))
		
(mdotimes (i 10)
  (let* ((c (char-to-string (+ ?0 i))))
    (mbm-define
     (format "M-%s" c) (intern c))
    ))

(def-key global-map (kbd "M-' M-@") 'mbm-file-open)

(define-key mbm-map (kbd "M-h") 'mbm-show-open)
(define-key mbm-map (kbd "M-SPC") 'mbm-show-open)

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
(defin 'mbm-cycle-plus)

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
	   (next . (cadr (mbm--name-member cbuf ',f)))
	   (prev . (cadr (mbm--name-member cbuf ',r)))
	   ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mbm-match-form-cdr (elt cbuf)
  (let ((cbuf-name (mbm-buffer-name cbuf)))
    (string-match (mbm-expansion-to-regexp (cdr elt)) cbuf-name)
    )
  )

(defun mbm-list-next (list item fun) (list-next list item fun))
(defun mbm-list-prev (list item fun) (list-prev list item fun))

(defun mbm-apply-cycle ()
  (let* (; cbuf from higher scope
	 ; cbuf-name from higher scope
	 (fun #'mbm-match-form-cdr)
	 (filter (delete-if-not '(lambda (x) (stringp (cdr x))) (copy-list forms)))
	 mf mr
	 )
  (save-match-data
    (setq foo
      (case tag
	(next (mbm-list-next filter cbuf fun))
	(prev (mbm-list-prev filter cbuf fun))
	(first (first filter))
	(last (car (last filter)))
	)
      )
    (cond
     (foo (mbm-expand (cdr foo) cbuf))
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

