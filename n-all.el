;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all-mode-recenter ()
  "Move point to the corresponding position in the original buffer."
  (interactive)
  (let ((buffer (current-buffer))
	(pos (all-mode-find (point))))
    (if pos
	(pop-to-buffer all-buffer)
      (error "This text is not from the original buffer"))
    (goto-char pos)
    (recenter)
    (pop-to-buffer buffer)
    )
  )

(defun ns-all-mode-hook ()
  (define-key all-mode-map "\C-c\C-l" 'all-mode-recenter)
  (define-key all-mode-map [f1] 'all-mode-recenter)
  (remove-hook 'all-load-hook 'ns-all-mode-hook)
  )

(add-hook 'all-load-hook 'ns-all-mode-hook)

(defun property-find-next-face (face)
  (sx
   (catch 'done
     (while (not (eobp))
       (let* ((plist (text-properties-at (point)))
	      (next-change
	       (or (next-property-change (point) (current-buffer))
		   (point-max)))
	      (f (plist-get plist 'face))
	      )
	 (cond ((eq f face) (throw 'done (point))))
	 (goto-char next-change)))
     )
   )
  )

(defun goto-face (face fun)
  (let ((p (funcall fun face)))
    (cond
     (p (goto-char p))
     )
    )
  )

(defun goto-next-face (face)
  (goto-face face 'property-find-next-face)
  )

(defun goto-next-face-match () (interactive) (goto-next-face 'match))
