(defun hide () (interactive)
  (setq hide-map (make-sparse-keymap))
  (define-key hide-map "[224z" 'hide-next)
  (use-local-map hide-map)
  (make-local-variable 'hide-open-pat)
  (setq hide-open-pat
	(read-from-minibuffer "Hide open pattern: " "%("))
  (setq selective-display t)
  )

(defun hide-pat () (interactive)
  (save-excursion
    (if (not (looking-at hide-open-pat)) (re-search-backward hide-open-pat))
    (let* ((from (point))
	   (to (save-excursion (re-search-forward "\\s(") (backward-char 1)
			       (forward-sexp 1) (point))))
      (hide-region from to)
    )))

(defun hide-region (from to)
  (save-excursion
    (goto-char from)
    (if (re-search-forward "\n" to t)
	(subst-char-in-region from to ?\n ?\^M t)
      (subst-char-in-region from to ?\^M ?\n t)))
  )

(defun hide-next () (interactive)
  (re-search-forward hide-open-pat)
  (hide-pat)
  )


