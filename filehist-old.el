(setq file-history-list nil)
(setq file-history-alist nil)

(defun file-history-get (file tag)
  (let ((a (alist-get file-history-alist file)))
    (alist-get a tag)
    )
  )

(defun file-history-put (file tag attr)
  (let ((a (alist-get file-history-alist file)))
    (setq a (alist-put a tag attr))
    (setq file-history-alist (alist-put file-history-alist file a))
    )
  )

(defun file-history-file-on-current-line ()
  (bs (sxp (bol) (fx 1) (fc 1)) (sxp (eol)))
  )

(defun file-history-display (&optional buf)
  (save-window-excursion
    (or buf (setq buf (current-buffer)))
    (set-buffer buf)
    (let ((p (point)))
      (erase-buffer)
      (dolist (i file-history-list)
	(let ((priority (file-history-get i 'priority)))
	  (insert ".")
	  (cond (priority
		 (put-text-property (1- (point)) (point) 'face
				    (alist-get filehist-face-alist priority))))
	  (insert (format "%s %s\n" priority i))
	  )
	)
      (goto-char p)
      (bol))
    )
  )

(defun file-history-redisplay () (interactive)
  (file-history-display)
  )

(defun file-history-add* (name)
  (setq file-history-list (cons name (delete name file-history-list)))
  (file-history-put name 'last-visit (current-time))
  (file-history-put name 'priority 0)
  (let ((buf (get-buffer "*files*")))
    (cond ((and buf (get-buffer-window-any-frame buf))
	   (file-history-display buf)
	   )
	  )
    )
  )

(defun file-history-add ()
  (file-history-add* (buffer-file-name))
  )

(defun file-history-add-dired ()
  (file-history-add* dired-directory)
  )

(add-hook 'find-file-hooks 'file-history-add)
(add-hook 'dired-mode-hook 'file-history-add-dired)

(defun file-history-file-apply (fun &rest args)
  (let ((file (file-history-file-on-current-line)))
    (apply fun file args)
    )
  )

(defun file-history-delete-entry () (interactive)
  (let ((file (file-history-file-on-current-line)))
    (setq file-history-list (delete file file-history-list))
    )
  (file-history-display)
  )

(defun file-history-find-file () (interactive)
  (file-history-file-apply 'find-file)
  )

(defun file-history-find-file-other-window () (interactive)
  (file-history-file-apply 'find-file-other-window)
  )

(defun file-history-mouse-find-file () (interactive)
  (mouse-set-point last-command-event)
  (file-history-file-apply 'find-file)
  )

(defun file-history-dired () (interactive)
  (dired (file-name-directory (file-history-file-on-current-line)))
  )

(defun file-history-sort-name () (interactive)
  (setq file-history-sort-key 'name)
  
  (setq file-history-list (sort file-history-list 'string-lessp))
  (file-history-display)
  )

(defun file-history-sort-priority () (interactive)
  (setq file-history-sort-key 'priority)
  (setq file-history-list
    (sort file-history-list
	  '(lambda (x y)
	     (> (file-history-get x 'priority) (file-history-get y 'priority)))
	  ))
  (file-history-display)
  )

(defun file-history-sort-time () (interactive)
  (setq file-history-sort-key 'time)
  (setq file-history-list
    (sort file-history-list
	  '(lambda (x y)
	     (time-less-p (file-history-get x 'last-visit) (file-history-get y 'last-visit)))
	  ))
  (file-history-display)
  )

(defun file-history-electric-key () (interactive)
  (let* ((keys (this-command-keys)))
    (cond
     ((zerop (current-column))
      (let* ((binding (lookup-key file-history-electric-map keys)))
	(and binding (call-interactively binding))
	)
      )
     (t (let* ((binding (lookup-key global-map keys)))
	(and binding (call-interactively binding))
	)
	)
     )
    )
  )

(defun file-history-electric-key-define (key command)
  (define-key file-history-map key 'file-history-electric-key)
  (define-key file-history-electric-map key command)
  )

(defun file-history-set-priority () (interactive)
  (let* ((keys (this-command-keys)))
;    (debug)
    (file-history-file-apply
     '(lambda (file priority) (file-history-put file 'priority priority))
     (- (string-to-char keys) 48)
     )
    )
  )

(defun file-history-init ()
  (setq file-history-map (make-sparse-keymap))
  (setq file-history-electric-map (make-sparse-keymap))
  (file-history-electric-key-define "l" 'file-history-redisplay)
  (file-history-electric-key-define "t" 'file-history-sort-time)
  (file-history-electric-key-define "p" 'file-history-sort-priority)
  (file-history-electric-key-define "s" 'file-history-sort-name)
  (file-history-electric-key-define "o" 'file-history-find-file-other-window)
  (file-history-electric-key-define "f" 'file-history-find-file)
  (file-history-electric-key-define "x" 'file-history-delete-entry)
  (file-history-electric-key-define "d" 'file-history-dired)
  (file-history-electric-key-define [mouse-2] 'file-history-mouse-find-file)
  (dotimes (i 9)
    (file-history-electric-key-define
     (char-to-string (+ ?0 i)) 'file-history-set-priority))
  )

(defun file-history-open () (interactive)
  (or (boundp 'file-history-map) (file-history-init))
  (setq file-history-buffer (get-buffer-create "*files*"))
;  (or (get-buffer-window-any-frame file-history-buffer) (display-buffer file-history-buffer))
  (file-history-display file-history-buffer)
  (switch-to-buffer file-history-buffer)
  (use-local-map file-history-map)
  )

(defun file-history-grep (pat &optional fpat)
  (let* ((list file-history-list)
	 cmd)
    (and fpat (setq list (subset-if '(lambda (x) (string-match-string fpat x)) list)))
    (setq list (subset-if 'file-exists-p list))
    (setq list (subset-if-not 'file-directory-p list))
    (setq cmd (format "grep -n \"%s\" %s" pat (mconcat list " ")))
    (compile cmd 'grep-mode)
    )
  )

(fset 'fhg 'file-history-grep)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq filehist-1-face (make-face 'filehist-1-face))
(set-face-foreground 'filehist-1-face "black")
(set-face-background 'filehist-1-face "green")

(setq filehist-2-face (make-face 'filehist-2-face))
(set-face-foreground 'filehist-2-face "black")
(set-face-background 'filehist-2-face "yellow")

(setq filehist-3-face (make-face 'filehist-3-face))
(set-face-foreground 'filehist-3-face "white")
(set-face-background 'filehist-3-face "orange")

(setq filehist-4-face (make-face 'filehist-4-face))
(set-face-foreground 'filehist-4-face "white")
(set-face-background 'filehist-4-face "red")

(setq filehist-face-alist
  `(
    (0 . default)
    (1 . filehist-1-face)
    (2 . filehist-2-face)
    (3 . filehist-3-face)
    (4 . filehist-4-face)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(file-history-grep "DATE_FORMAT" "backup")
file-history-alist
