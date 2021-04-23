; old code to ediff nvc backup versions

(setq bdiff-mru-buffer nil)

(make-local-variable 'bdiff-state)
(make-local-variable 'bdiff-refresh-command)

(setq bdiff-map (make-sparse-keymap))

(suppress-keymap bdiff-map)
(define-key bdiff-map "e" 'bdiff-ediff-files)
(define-key bdiff-map "a" 'bdiff-mark)
(define-key bdiff-map "b" 'bdiff-mark)
(define-key bdiff-map "u" 'bdiff-clear-mark)
(define-key bdiff-map "f" 'bdiff-find-file)
(define-key bdiff-map "o" 'bdiff-find-file-other-window)
(define-key bdiff-map "." 'bdiff-mark-2)
(define-key bdiff-map " " 'next-line)
(define-key bdiff-map "n" 'bdiff-next)
(define-key bdiff-map "s" 'bdiff-sort)
(define-key bdiff-map "g" 'bdiff-refresh)
(define-key bdiff-map "h" 'bdiff-help)

(defun bdiff-help () (interactive)
  (describe-mode)
  )

(defun bdiff-refresh () (interactive)
  (let ((s (eval bdiff-refresh-command)))
    (alist-put bdiff-state "A" nil)
    (alist-put bdiff-state "B" nil)
    (erase-buffer)
    (insert s)
    (bob)
    )
  )

(defun bdiff-sort () (interactive)
  (error "not done yet")
  )

(defun bdiff-marked-file (mark)
  (let* ((pos (alist-get bdiff-state mark))
	 )
    (cond
     (pos (sx (goto-char pos) (bol)
	    (rsf "\t")
	    (bs (point) (sxp (eol)))
	    )
	  )
     )
    )
  )

(defun bdiff-original-file ()
  (sx
   (move-to-column 16)
   (let ((start (point))
	 (end (sx (rsf " *\t") (match-beginning 0))))
     (bs start end)
     )
   )
  )

(defun bdiff-file (pos)
  (sx (goto-char pos) (bol)
      (rsf "\t")
      (bs (point) (sxp (eol)))
      )
  )

(defun bdiff-marked-file (mark)
  (let* ((pos (alist-get bdiff-state mark))
	 )
    (cond
     (pos (bdiff-file pos))
     )
    )
  )

(defun bdiff-ediff-files () (interactive)
  (let* ((a (bdiff-marked-file "A"))
	 (b (bdiff-marked-file "B"))
	 )
    (cond
     ((not (or a b))
      (setq a (bdiff-file (point)))
      (setq b (bdiff-original-file))
      )
     ((not a) (setq a (bdiff-original-file)))
     ((not b) (setq b (bdiff-original-file)))
     )
    (ediff-files a b)
    )
  )

(defun bdiff-show-mark (pos mark)
  (save-excursion
    (goto-char pos)
    (bol)
    (delete-char 1)
    (insert mark)
    )
  )

(defun bdiff-find-file () (interactive)
  (find-file (bdiff-file (point)))
  )

(defun bdiff-find-file-other-window () (interactive)
  (find-file-other-window (bdiff-file (point)))
  )

(defun bdiff-clear-mark-1 (pos mark) (interactive)
  (setq bdiff-state (alist-put bdiff-state mark nil))
  (bdiff-show-mark pos " ")
  )

(defun bdiff-clear-mark () (interactive)
  (let ((this))
    (save-excursion
      (bol)
      (cond ((looking-at "[AB]")
	     (setq this (bsp 1))
	     (bdiff-clear-mark-1 (point) this)
	     )
	    )
      )
    )
  )

(defun bdiff-mark-1 (this)
  (let* ((curr (alist-get bdiff-state this))
	 (here (sxp (bol)))
	 )
    (cond
     (curr
      (bdiff-show-mark curr " ")
      )
     )
    (bdiff-show-mark here this)
    (setq bdiff-state (alist-put bdiff-state this here))
    )
  )

(defun bdiff-mark () (interactive)
  (bdiff-mark-1 (upcase (this-command-keys)))
  )

(defun bdiff-next () (interactive)
  (let ((file (bdiff-original-file)))
    (while
	(and
	 (zerop (forward-line 1))
	 (not (string-equal (bdiff-original-file) file))
	 )
      )
      )
  )

(defun bdiff-mark-2 () (interactive)
  (bdiff-clear-mark)
  (bdiff-mark-1 "A")
  (sx
   (forward-line 1)
   (bdiff-mark-1 "B")
   )
  )

(defun bdiff-kill-hook ()
  (setq bdiff-mru-buffer nil)
  )

(defun bdiff-mode ()
  "Backup Diff mode.

\\{bdiff-map}
" (interactive)
  (use-local-map bdiff-map)
  (setq major-mode 'bdiff-mode)
  (setq mode-name "bdiff")
  (setq bdiff-state nil)
  (add-hook 'kill-buffer-hook 'bdiff-kill-hook t t)
  )

(defun bdiff (s cmd) (interactive)
  (switch-to-buffer (generate-new-buffer "*bdiff*"))
  (insert s)
  (bob)
  (bdiff-mode)
  (setq bdiff-mru-buffer (current-buffer))
  (setq bdiff-refresh-command cmd)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'ediff-quit-hook 'bdiff-ediff-cleanup t)

(defun bdiff-ediff-cleanup ()
  (cond ((bufferp bdiff-mru-buffer)
	 (switch-to-buffer bdiff-mru-buffer)
	 (delete-other-windows)
	 )
	)
  )

