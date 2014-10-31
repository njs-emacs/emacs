;
; cm structure is ((comp-buffer . point) . (target-file . line))
;
; comp-buffer-point is the original message if the cm was built from a string
; comp-buffer-point and target-buffer-line can be converted into
; markers with cm-marker
;
; cmc is compilation buffer info
; cmt is target info

(setq cm-pat "^(\\|^\\([^: \n]+\\):\\(line\\|\\)\\s *\\([-.0-9 ]*\\):\\(.*\\)")

(defun cmc-compilation-buffer (x) (car x))
(defun cmc-location (x) (cdr x))

(defun cmt-target-file (x) (car x))
(defun cmt-target-location (x) (cdr x))

(fset 'cm-cmc 'car)
(fset 'cm-cmt 'cadr)
(fset 'cm-msg 'caddr)

(defun cmc ()
  (cons (buffer) (point))
  )

(defun cmt (file line)
  (cons file line)
  )

(defun cm-build (cmc cmt &optional msg)
  (list cmc cmt msg)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cmc-goto (cmc)
  (cond
   ((markerp cmc) (goto-marker cmc))
   (t (set-buffer (buffer (cmc-compilation-buffer cmc)))
	  (goto-char (cmc-location cmc))
	  )
   ))

(defun goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char marker)
  (marker-buffer marker)
  )

(defun cmt-goto (cmt)
  (setq default-directory (cm-eval (default-directory)))
  (cond
   ((markerp cmt)
    (goto-marker cmt)
    )
   (t (switch-to-buffer (buffer (cmt-target-file cmt)))
	  (goto-line (cmt-target-location cmt))
	  )
   )
  )

(defun cmc-marker (cmc)
  (set-marker (make-marker) (sxp (goto-char (cmc-location cmc))) (buffer (cmc-compilation-buffer cmc)))
  )

(defun cmt-marker (cmt)
  (sx (set-buffer (buffer (cmt-target-file cmt)))
      (set-marker (make-marker) (sxp (goto-line (cmt-target-location cmt))))
      ))

(defun cm-string-l (s)
  (let* ((form (read s))
	 (file (nth 0 form))
	 (unit (nth 1 form))
	 (pos (nth 2 form))
	 )
    (cm-build (nth 3 form) (cmt file pos))
    ))

(defun cm-string-c (s)
  (let* ((stuff (string-parse s cm-pat 1 3 4))
	 (file (nth 0 stuff))
	 (line (read (nth 1 stuff)))
	 (msg (nth 2 stuff)))
    (cm-build msg (cmt file line))
    ))

(defun cm-string (s)
  "Make compilation marker from string"
  (if (eq (aref s 0) 40)
      (cm-string-l s))
  (cm-string-c s))

(defun cm-buf-l ()
  (let* ((form (readc))
		 (file (nth 0 form))
		 (unit (nth 1 form))
		 (pos (nth 2 form))
		 )
	(cm-build (cmc) (cmt file pos) nil)
	))

(defun cm-buf-c ()
  (let* ((file (ms 1))
	 (pos (read (ms 3)))
	 (msg (ms 4)))
    (cm-build (cmc) (cmt file pos) msg)
    )
  )

(defun cm-getbuf (&optional buf)
  (cond
   ((bufferp buf) buf)
   ((eq buf nil) compilation-last-buffer)
   (t (current-buffer))
   )
  )

(defmacro cm-sx (&rest body)
  (apply 'list 'sbx '(cm-getbuf) body)
  )

(defun cm-buf (&optional buf)
  "Make compilation marker from string at point, or return nil if none
This will move point in the compilation buffer"
  (sbx (cm-getbuf buf)
	   (and
	    (re-search-forward cm-pat nil t)
	    (sx
	     (goto-char (match-beginning 0))
	     (cond ((eq (char-after (point)) 40)
		    (cm-buf-l))
		   ((cm-buf-c))))
	    ))
  )

(defun cm-buf-list (&optional pred buf)
  (or pred (setq pred 'identity))
  (sbx (cm-getbuf buf)
	(bob)
	(let (result next)
      (while (setq next (cm-buf))
		(and (funcall pred next)
			 (setq result (cons next result))))
      (nreverse result)
      ))
  )

(defun cm-next ()
  (cm-buf nil)
  )

(defun cmc-msg (cmc)
  (cond ((stringp cmc) cmc)
	(t (sx (cmc-goto cmc) (fms cm-pat 4)))
	)
  )

(defun cmt-progn* (cm body)
  (let* ((default-directory default-directory)
		 (cmc (cm-cmc cm))
		 (cmt (cm-cmt cm))
		 (msg (cm-msg cm))
		 (file (cmt-target-file cmt))
		 (line (cmt-target-location cmt))
		 )
	 (save-excursion 
	   (eval body)
	   )
	 ))

(defmacro cmt-progn (cm &rest body)
  (list 'cmt-progn* cm (list 'quote (cons 'progn body)))
  )

(defun cm-follow (&optional m)
  (set-window-start (display-buffer compilation-last-buffer) (or m cmc))
  )

(defun mmb* (body)
  (let* (result)
	(cm-sx (bob))
	(let (cm)
	  (while (setq cm (cm-next))
		(setq result (cons (cmt-progn* cm body) result))
		))
	(nreverse result)
	))

(defmacro mmb (&rest body)
  "Process compilation messages in compilation-last-buffer
Files are not visited.
The BODY is executed for each message."
  (list 'mmb* (list 'quote (apply 'list 'progn body))))

(defmacro mmq (&rest body)
  "Process compilation messages in compilation-last-buffer
Files are visited, but markers are not used.
The BODY is executed for each message."
  (list 'mmb* (list 'quote (apply 'list 'progn (cons '(cmt-goto cmt) body))))
  )

(defun mm* (body)
  (let ((list (mmb (cm-build cmc (cmt-marker cmt)))))
	(mapcar
	 '(lambda (cm)
	    (cmt-goto (cm-cmt cm))
	    (eval body)
	    ) list)))

(defmacro mm (&rest body)
  (list 'mm* (list 'quote (apply 'list 'progn body))))

(dolist (i '(mmb mmq mm cmt-progn)) (defin i 'defun))

(defmacro cm-eval (x)
  (apply 'list 'sx '(set-buffer compilation-last-buffer) x))

(defmacro cm-qm (get list)
  (` (let (cm (default-directory (cm-eval (default-directory))))
	   (cm-sx (bob))
	   (query
		 (cond ((setq cm (cm-next)) (cmt-goto (cm-cmt cm)) t))
		 (unwind-protect
			 (save-restriction (,@ get))
		   (widen))
		 (,@ list)
		 )
	   )
	 ))

(defmacro mmqr (a b)
"Replace regexp A with replacement B for each line in the compilation buffer"
(` (mmq (rep$ (, a) (, b)))))
