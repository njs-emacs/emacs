;#~ looks like terminal only

(defun hide-region (from to)
  (save-excursion
    (goto-char from)
    (if (re-search-forward "\n" to t)
	(subst-char-in-region from to ?\n ?\^M t)
      (subst-char-in-region from to ?\^M ?\n t)))
  )

(defun hide-sexp () (interactive)
  (apply 'hide-region (rsexp)))

(define-key global-map "[211z" 'hide-sexp)
