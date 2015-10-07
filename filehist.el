; this was smarter than I gave myself credit for when I started to rewrite it
;

(setq file-history-list nil)
(setq file-history-alist nil)

(defun file-history-recover-list (file tag)
  (setq file-history-list (mapcar 'car file-history-alist))
  )

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

;;;;;;;;;;;;;;;;
(defun file-history-electic-column-limit () (+ 4 12))

(defun file-history-file-on-current-line ()
  (bs (sxp (bol) (rsf "\t")) (sxp (eol)))
  )

(defun file-history-paint-line (name priority)
  (insert "\t" (or name "?"))
  )

;;;;;;;;;;;;;;;;

(defun file-history-display (&optional buf)
  (save-window-excursion
    (or buf (setq buf (current-buffer)))
    (set-buffer buf)
    (setq tab-width 1)
    (let ((p (point)))
      (erase-buffer)
      (dolist (name file-history-list)
	(let ((priority (file-history-get name 'priority)))
;	  (insert ". ")
	  (insert (format "%-8s " (qb-mapping name)))
	  (cond (priority
		 (put-text-property (- (point) 1) (- (point) 0) 'face
				    (alist-get filehist-face-alist priority))))
	  (and name (file-history-paint-line name priority))
	  (insert "\n")
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
  (let ((defaults `((priority . 0))))
    (dolist (i defaults)
      (or (file-history-get name (car i)) (file-history-put name 'priority (cdr i)))
      )
    )
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

(defun file-history-find-file-other-frame () (interactive)
  (file-history-file-apply 'find-file-other-frame)
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
  (bob)
  )

(defun file-history-sort-priority () (interactive)
  (setq file-history-sort-key 'priority)
  (setq file-history-list
    (sort file-history-list
	  '(lambda (x y)
	     (> (file-history-get x 'priority) (file-history-get y 'priority)))
	  ))
  (file-history-display)
  (bob)
  )

(defun file-history-sort-quick () (interactive)
  (setq file-history-sort-key 'quick)
  (setq file-history-list
    (sort file-history-list
	  '(lambda (x y)
	     (string< (or (qb-where-is-string x) "z") (or (qb-where-is-string y) "z")))
	  ))
  (file-history-display)
  (bob)
  )

(defun fs--sort-by-recentf (f1 f2)
  "Function for sorting files by recentf order."
  (let ((1-index (-elem-index f1 recentf-list))
        (2-index (-elem-index f2 recentf-list)))
    (when (and 1-index 2-index (< 1-index 2-index)) t)))

(defun file-history-sort-time () (interactive)
  (setq file-history-sort-key 'time)
  (setq file-history-list
    (sort file-history-list
	  '(lambda (x y)
	     (fs--sort-by-recentf x y))
	  ))
  (file-history-display)
  (bob)
  )

(defun file-history-electric-key () (interactive)
  (let* ((keys (this-command-keys)))
    (cond
     ((< (current-column) (file-history-electic-column-limit))
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
  (file-history-redisplay)
  )

(defun file-history-priority-raise () (interactive)
  (file-history-file-apply
   '(lambda (file) (file-history-put file 'priority (1+ (file-history-get file 'priority))))
   )
  (file-history-redisplay)
  )

(defun file-history-priority-lower () (interactive)
  (file-history-file-apply
   '(lambda (file) (file-history-put file 'priority (1- (file-history-get file 'priority))))
   )
  (file-history-redisplay)
  )

(defun file-history-qb-define () (interactive)
  (file-history-file-apply 'qb-define-read-key)
  (file-history-redisplay)
  )

(defun file-history-copy () (interactive)
  (copy-region-as-kill (sxp (bol) (rsf "\t")) (sxp (eol)))
  )
  

;;
;; want to replace all self-insert-command keys with file-history-electric-key
;;

(defun file-history-map-init ()
  (setq file-history-map (make-sparse-keymap))
  (setq file-history-electric-map (make-sparse-keymap))
  (file-history-electric-key-define " " 'next-line)
  (file-history-electric-key-define "+" 'file-history-priority-raise)
  (file-history-electric-key-define "-" 'file-history-priority-lower)
  (file-history-electric-key-define "l" 'file-history-redisplay)
  (file-history-electric-key-define "t" 'file-history-sort-time)
  (file-history-electric-key-define "p" 'file-history-sort-priority)
  (file-history-electric-key-define "s" 'file-history-sort-name)
  (file-history-electric-key-define "b" 'file-history-sort-quick)
  (file-history-electric-key-define "o" 'file-history-find-file-other-window)
  (file-history-electric-key-define "f" 'file-history-find-file)
  (file-history-electric-key-define "w" 'file-history-find-file-other-frame)
  (file-history-electric-key-define "x" 'file-history-delete-entry)
  (file-history-electric-key-define "d" 'file-history-dired)
  (file-history-electric-key-define "k" 'file-history-copy)
  (file-history-electric-key-define "q" 'file-history-qb-define)
  (file-history-electric-key-define [mouse-2] 'file-history-mouse-find-file)
  (dotimes (i 9)
    (file-history-electric-key-define
     (char-to-string (+ ?0 i)) 'file-history-set-priority))
  )

(defun file-history-mode () (interactive)
  (or (boundp 'file-history-map) (file-history-map-init))
  (use-local-map file-history-map)
  (setq major-mode 'file-history-mode)
  (setq mode-name "HISTORY")
  )

(defun file-history-open () (interactive)
  (setq file-history-buffer (get-buffer-create "*files*"))
;  (or (get-buffer-window-any-frame file-history-buffer) (display-buffer file-history-buffer))
  (file-history-display file-history-buffer)
  (switch-to-buffer file-history-buffer)
  (file-history-mode)
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

(setq filehist-5-face (make-face 'filehist-5-face))
(set-face-foreground 'filehist-5-face "white")
(set-face-background 'filehist-5-face "blue")

(setq filehist-face-alist
  `(
    (0 . default)
    (1 . filehist-1-face)
    (2 . filehist-2-face)
    (3 . filehist-3-face)
    (4 . filehist-4-face)
    (5 . filehist-5-face)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(file-history-grep "DATE_FORMAT" "backup")
file-history-alist
