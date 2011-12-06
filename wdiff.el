(defun flash () (interactive) "Switch between windows until some input arrives"
  (while (sit-for 1) (other-window 1)))

(defun wdiff-sync-try (len)
  (let ((s (buffer-substring (point) (+ (point) len))))
    (other-window 1)
    (if (search-forward s nil t) (progn (backward-char len) t))
    ))

(defun wdiff-sync () (interactive)
  (while (not (or (wdiff-sync-try 16) (wdiff-sync-try 16)))
    (forward-char 1)))

(defun wdiff-cmp ()
  (compare-windows)
  (not (and (eobp) (ow (eobp))))
  )

(defun wdiff-next () (interactive)
  (wdiff-sync)
  (compare-windows)
  (cond ((wdiff-cmp) (flash) t))
  )


