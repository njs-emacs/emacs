(defun blammo ()
  (let ((bracket (te-get-char))
	c
	(numlist (list 0))
	cmd)
    (catch 'done
      (while t
	(setq c (te-get-char))
	(cond
	 ((and (>= c ?0) (<= c ?9))
	  (setcar numlist (+ (* 10 (car numlist)) (- c ?0))))
	 ((= c ?\;)
	  (setq numlist (cons 0 numlist)))
	 ((let ((fun (or (cdr (assq c
				    '((?f . vt-cursor)
				      (?J . vt-clear-screen)
				      (?K . vt-clear-line)
				      (?M . vt-delete-line)
				      (?L . vt-insert-line)
				      ))) 'vt-unknown)))
	    (funcall fun c (nreverse numlist))
	    (throw 'done nil)
	    ))
	 ))
      )))

(defun te-blank-line (&optional n)
  (apply 'concat (make-list (or n 1) (concat (make-string te-width ? ) "\n"))))

(defun vt-delete-line (key args)
  (kill-line (nth 0 args))
  (eob)
  (insert (te-blank-line (nth 0 args)))
  )

(defun vt-insert-line (key args)
  (insert (te-blank-line (nth 0 args)))
  (eob)
  (kill-line (nth 0 args))
  )

(defun vt-cursor (key args)
  (goto-line (nth 0 args))
  (move-to-column (1- (nth 1 args)))
  )

(defun vt-clear-line (key args)
  (te-clear-rest-of-line)
  )

(defun vt-clear-screen (key args)
  (te-clear-screen)
  )

(defun vt-unknown (key args)
  (message (format "key=%c args=%s" key args)))


