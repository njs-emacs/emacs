(defun region-beginning-if-active (&optional otherwise)
  (cond ((region-active-p) (region-beginning))
	(otherwise))	
  )

(defun region-end-if-active (&optional otherwise)
  (cond ((region-active-p) (region-end))
	(otherwise))
  )

(defun avy-goto-word-this-line (&optional arg)
  (interactive "P")
  (avy-with avy-goto-word-0
    (avy-goto-word-0 arg (point^) (point$))))

(defun avy-ns-region-begin (&optional beg)
  (or beg (region-beginning-if-active) (window-start))
  )

(defun avy-ns-region-end (&optional beg)
  (or beg (region-end-if-active) (window-end))
  )

(defun avy-sexp-candidates (&optional beg end)
  (let* ((avy-all-windows nil)
	 (beg (avy-ns-region-begin beg))
	 (end (avy-ns-region-end end))
	 )
    (avy--regex-candidates "(" beg end)
    )
  )

(defun avy-sexp-copy-action (pt)
  (sx
   (goto-char pt)
   (copy-sexp-as-kill)
   )
  (yank)
  )

(defun avy-sexp-copy () (interactive)
  (let* ((avy-action 'avy-sexp-copy-action)
	 (x (avy-process
	    (avy-sexp-candidates)
	    )
	   ))
    )
  )

;(display-monitor-attributes-list)
;(frame-monitor-geometry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun avy-copy-symbol-1 (char &optional ARG BEG END SYMBOL)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (let ((s
	 (save-excursion
	   (avy-goto-word-1 char)
	   (symbol-at-point)
	   )
	 ))
    (insert (format "%s" s))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun avy-copy-symbol-1-other-window (char &optional ARG BEG END SYMBOL)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (let ((s
	 (save-excursion
	   (save-window-excursion
	     (other-window 1)
	     (avy-goto-word-1 char)
	     (symbol-at-point)
	     )
	   )
	 ))
    (insert (format "%s" s))
    )
  )

