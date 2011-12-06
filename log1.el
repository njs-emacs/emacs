(defun ftime (time)
  (/ (+ (* 1000000.0 (nth 1 time)) (nth 2 time)) 1000000)
  )

(defun time-normalize (d s u)
  (cond ((< u 0)
	 (setq s (+ s (1- (/ u 1000000))))
	 (setq u (+ 1000000 (% u 1000000)))
	 ))
  (cond ((< s 0)
	 (setq d (+ d (1- (/ s 86400))))
	 (setq s (+ 86400 (% s 86400)))
	 ))
  (list d s u)
  )

(defun time-sub (a b)
  (time-normalize (- (nth 0 a) (nth 0 b))
		  (- (nth 1 a) (nth 1 b))
		  (- (nth 2 a) (nth 2 b))
		  )
  )

;;;
;;;
;;;

(setq log-file-directory (expand-file-name "~/LOG"))

(defun log-file-name (name)
  (format "%s/%s/%s" log-file-directory name
	  (format-time-string "%y%m%d" (current-time)))
  )

(defun log (name &rest stuff)
  (condition-case x
      (save-excursion
	(set-buffer (find-file-noselect (log-file-name name)))
	(eob)
	(insert (format-time-string "%y%m%d %H%M%S " (current-time)))
	(mapcar 'insert stuff)
	(or (bolp) (insert "\n"))
	(basic-save-buffer)
	(kill-buffer (current-buffer))
	)
    (error (message "logging error"))
    )
  )

(setq log-suppress nil)

(defun log-suppress-file (name)
  (cond
   ((eq (string-match log-file-directory name) 0))
   )
  )

(defun log-file (tag)
  (cond ((or log-suppress (log-suppress-file (buffer-file-name))))
	((log "files" (format "%s \"%s\"" tag (buffer-file-name)))
	 )
	)
  )

(defun log-find-file ()
  (log-file "V")
  )

(defun log-write-file ()
  (log-file "S")
  nil
  )

(add-hook 'find-file-hooks 'log-find-file)
(add-hook 'write-file-hooks 'log-write-file)

(defun log-event (s) (interactive "sEvent: ")
  (log "events" s)
  )

(setq tasks nil)

(defun task-name (x) (cond ((zerop (length (nth 1 x))) (nth 4 x)) ((nth 1 x))))
(defun task-start (x) (nth 0 x))
(defun task-end (x) (nth 3 x))
(defun task-duration (x) (time-sub (task-end x) (task-start x)))
(defun task-subs (x) (nth 2 x))

(defun log-task (&rest stuff)
  (save-excursion
    (set-buffer (find-file-noselect (log-file-name "tasks")))
    (eob)
    (insert (format-time-string "%y%m%d %H%M%S " (current-time)))
    (mapcar 'insert stuff)
    (or (bolp) (insert "\n"))
    (basic-save-buffer)
;    (kill-buffer (current-buffer))
    )
  )

(defun task-push (comment) (interactive "sComment: ")
  (let ((time (current-time)))
    (log-task (make-string (length tasks) ? ) "[ " comment)
    (setq tasks (cons (list time comment) tasks))
    )
  )

(defun task-pop (arg) (interactive "P")
  (let* ((task (car tasks))
	 (comment (cadr task))
	 (time)
	 diff
	 )
    (cond
     (arg
      (setq comment
	(read-from-minibuffer "Comment: " (concat comment " (aborted)")))
      )
     ((zerop (length comment))
      (setq comment (read-from-minibuffer "Comment: "))
      )
     )
    (setq time (current-time))
    (setq diff (- (ftime time) (ftime (car task))))
    (log-task (make-string (length tasks) ? ) "] "
	      (format "%5.03g " diff) comment)
    (setq tasks (cdr tasks))
    )
  )

(defun task-re (comment) (interactive "sComment: ")
  (task-push comment)
  (recursive-edit)
  (task-pop nil)
  )

(defun task-lock (comment) (interactive "sComment: ")
  (task-push comment)
  (yes-or-no-p "Are you done yet? ")
  (task-pop nil)
  )

(defun log-compile ()
  (log "compile" (format "%s \"%s\"" default-directory compile-command))
  )
(add-hook 'compilation-mode-hook 'log-compile)

;;;
;;;
;;;

(setq log-map (make-sparse-keymap))

(define-key log-map "[" 'task-push)
(define-key log-map "]" 'task-pop)
(define-key log-map "\C-o" 'task-push)
(define-key log-map "\C-k" 'task-pop)

(define-key log-map "\C-e" 'log-event)
(define-key log-map "\C-r" 'task-re)
(define-key log-map "\C-l" 'task-lock)

(define-key log-map "\C-b" '(lambda () (interactive) (task-lock "break")))
(define-key log-map "\C-p" '(lambda () (interactive) (task-lock "phone")))

;(define-key global-map nil log-map)

