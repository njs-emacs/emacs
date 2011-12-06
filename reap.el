(make-variable-buffer-local 'reap-protect)
(set-default 'reap-protect t)

(defun reap-by-mode (mode)
  (dolist (x (buffer-list))
	  (sx (set-buffer x)
	      (and (equal major-mode mode)
		   (not reap-protect)
		   (kill-buffer x))))
  )

(defun reap-show () (interactive)
  (reap-by-mode 'show)
  )

(provide 'reap)
