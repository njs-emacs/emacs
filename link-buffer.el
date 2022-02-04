;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(setq link-file-visit-hook nil) 
(make-local-variable 'link-file-visit-hook)

(defun link-file-visit (tag &optional arg)
  (let ((f (linked-file tag)))
    (cond
     (f (cond
	 (arg (find-file-other-window f))
	 ((find-file f))
	 )
	(run-hooks 'link-file-visit-hook)
	))))

(qb-define (control-key-vector ?l ?u)
	   '(linked-file 'up)
	   )

(qb-define (control-key-vector ?l ?d)
	   '(linked-file 'down)
	   )

(qb-define (control-key-vector ?l ?f)
	   '(linked-file 'first)
	   )

(qb-define (control-key-vector ?l ?p)
	   '(linked-file 'prev)
	   )

(qb-define (control-key-vector ?l ?n)
	   '(linked-file 'next)
	   )

(qb-define (control-key-vector ?l ?o)
	   '(linked-file 'other)
	   )

(qb-define (control-key-vector ?,)
	   '(linked-file 'prev)
	   )

(qb-define (control-key-vector ?.)
	   '(linked-file 'next)
	   )

(qb-define (control-key-vector ?l ?k)
	   '(linked-file 'keymap-help)
	   )

(dolist (i '(q w e r t y))
  (qb-define (control-key-vector ?l  (string-to-char (symbol-name i))) `(linked-file ',i)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun link-file-visit-prev (&optional arg) (interactive "P") (link-file-visit 'prev arg))
(defun link-file-visit-next (&optional arg) (interactive "P") (link-file-visit 'next arg))
(defun link-file-visit-other (&optional arg) (interactive "P") (link-file-visit 'other arg))
(defun link-file-visit-first (&optional arg) (interactive "P") (link-file-visit 'first arg))

(define-key global-map [C-kp-left] 'link-file-visit-prev)
(define-key global-map [C-kp-right] 'link-file-visit-next)
(define-key global-map [C-kp-divide] 'link-file-visit-other)
(define-key global-map [C-kp-home] 'link-file-visit-first)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; file-ring uses buffer-local file-linked-plist which
; overrides all other methods
; visit each file in the ring and set the next and prev
; links accordingly

(defun sx-linked-file-merge (file alist)
  (sx (set-buffer (find-file-noselect file))
      (setq file-linked-plist
	(alist-merge file-linked-plist alist)
	)
      )
  )

(defun file-ring-create (files &optional hook)
  (let ((a files) prev (first (car files)))
    (while a
      (let ((this (pop a)) (next (car a)))
	(sx-linked-file-merge 
	 this `((next . ,next) (prev . ,prev) (first . ,first))
	 )
	(setq prev this)
	)
      )
    (let ((first (car files))
	  (last (car (last files)))
	  )
      (sx-linked-file-merge first  `((prev . ,last)))
      (sx-linked-file-merge last   `((next . ,first)))
      )
    (find-file first)
    )
  (let ((buffers (mapcar 'get-file-buffer files)))
    (mapcar '(lambda (x) (set-buffer x) (add-hook 'link-file-visit-hook hook)) buffers)
    buffers)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun link-buffer-by-predicate (list fun)
  (let* ((cur (current-buffer))
	 (new)
	 )
    (setq new 
      (catch 'done (dolist (i list)
		     (cond ((funcall fun i)
			    (throw 'done i))))))
    )
  )

(defun link-buffer-next-by-predicate (fun &optional arg)
  (let ((buf (link-buffer-by-predicate (cdr (buffer-list)) fun)))
    (cond
     (buf
      (bury-buffer (current-buffer))
      (cond
       (arg (switch-to-buffer-other-window buf))
       (t (switch-to-buffer buf))
       )
      )
     )
    )
  )

(defun link-buffer-prev-by-predicate (fun &optional arg)
  (let ((buf (link-buffer-by-predicate (reverse (cdr (buffer-list))) fun)))
    (cond
     (buf
      (cond
       (arg (switch-to-buffer-other-window buf))
       (t (switch-to-buffer buf))
       )
      )
     )
    )
  )

(defun def-link-buffer-*-by-predicate (name fun)
  (eval
   `(progn
      (defun ,(intern (format "link-buffer-prev-by-%s" name)) (arg) (interactive "P")
	(link-buffer-prev-by-predicate ',fun arg))
      (defun ,(intern (format "link-buffer-next-by-%s" name)) (arg) (interactive "P")
	(link-buffer-next-by-predicate ',fun arg))
      )
   )
  )

;; demo code follows ;;
(defun link-buffer-by-major-mode (new)
  (eq major-mode (sbx new major-mode)))

(def-link-buffer-*-by-predicate 'major-mode 'link-buffer-by-major-mode)

(define-key global-map [H-left] 'link-buffer-prev-by-major-mode)
(define-key global-map [H-right] 'link-buffer-next-by-major-mode)
