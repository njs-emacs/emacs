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
  (define-key all-mode-map "\C-n" 'goto-next-face-match)
  (define-key all-mode-map "\C-p" 'goto-next-face-match-not)
  (remove-hook 'all-load-hook 'ns-all-mode-hook)
  )

(add-hook 'all-load-hook 'ns-all-mode-hook)

(defun property-find-next-face-not** (face)
   (catch 'done
     (while (not (eobp))
       (let* ((plist (text-properties-at (point)))
	      (next-change
	       (or (next-property-change (point) (current-buffer))
		   (point-max)))
	      (f (plist-get plist 'face))
	      )
	 (or (eq f face) (throw 'done (point)))
	 (goto-char next-change)))
     )
   )

(defun property-find-next-face** (face)
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

(defun property-find-next-face-not* (face)
  (property-find-next-face** face)
  (property-find-next-face-not** face)
  )

(defun property-find-next-face* (face)
  (property-find-next-face-not** face)
  (property-find-next-face** face)
  )

(defun property-find-prev-face* (face)
  (property-find-prev-face-not** face)
  (property-find-prev-face** face)
  )

(defun property-find-next-face (face)
  (sx (property-find-next-face* face))
  )

(defun property-find-next-face-not (face)
  (sx (property-find-next-face-not* face))
  )

(defun property-find-prev-face (face)
  (sx (property-find-prev-face* face))
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

(defun goto-next-face-not (face)
  (goto-face face 'property-find-next-face-not)
  )

(defun goto-next-face-match () (interactive) (goto-next-face 'match))
(defun goto-next-face-match-not () (interactive) (goto-next-face-not 'match))
