(defun highlight-trailing-whitespace () (interactive)
  (let ()
    (sx (bol)
	(let ((pat ".*?\\([ \t]+\\)$"))
	  (looking-at pat)
	  ))
    (let* ((s (match-beginning 1))
	   (e (match-end 1))
	   (ov (make-overlay  s e nil t)))
;      (message "%s %s" s e)
      (overlay-put ov 'face 'magit-diff-whitespace-warning)
      (overlay-put ov 'priority 2))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indent-to-column-rigid (col)
  (just-one-space)
  (indent-to-column col)
  )

(defun indent-pattern-to-column (pat col &optional noquery)
  (interactive "sPattern: \nPColumn: ")
  (let (start end (map query-replace-map))
    (cond ((region-active-p)
	   (setq start (region-beginning))
	   (setq end (set-marker (make-marker) (region-end)))
	   column
	   )
	  ((setq start (point)) (setq end (point-max-marker)))
	  )
    (or col (setq col goal-column) (setq col (current-column)))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (rsf pat end nil nil t))
	(setq column (cond ((symbolp col) (funcall col)) (col)))
	(cond (noquery (setq def 'act))
	      ((message "indent to column %s? " column)
	       (setq key (read-event))
	       (setq def (lookup-key map (vector key)))
	       )
	      )
	(cond
	 ((eq def 'skip) (end-of-line))
	 ((eq def 'act) (indent-to-column-rigid column) (end-of-line))
	 ((eq def 'automatic) (indent-to-column-rigid column) (setq noquery t))
	 ((eq def 'exit) (eob))
	 ((eq def 'recenter) (recenter))
	 ((eq def 'backup) (rsb pat start nil nil t) (bol) )
	 ((funcall def))
	 )
	)
      )
    )
  )

(global-set-key (kbd "M-s M-i") 'indent-pattern-to-column)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

(global-set-key (kbd "M-s M-=") 'occur-dwim)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-this-buffer-window (arg)
  (interactive "P")
  (delete-other-windows)
  (split-window-below)
  )

(define-key c-z-map (kbd "C--") 'split-this-buffer-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bulkload-directory (filename-concat user-emacs-home "recent/2103"))

