(defun copy-text-region- (start end &optional point)
  (interactive "p")
  (copy-region-as-kill start end)
  (goto-char (or point end))
  (yank)
  )

(defun copy-text-line-down (&optional arg)
  "Copy the current line(s) down."
  (interactive "p")
  (sx (copy-text-region- (point^) (sxp (fl arg))))
  )

(defun copy-text-down (&optional start end arg)
  "Duplicate the region."
  (interactive (move-text-get-region-and-prefix))
  (if (region-active-p)
      (copy-text-region- start end)
    (copy-text-line-down)
    )
  )

(def-key-global (kbd "<s-M-down>") 'copy-text-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dubious

(defun copy-paragraph (&optional arg)
  "Duplicate the current paragraph."
  (interactive "p")
  (mark-paragraph)
  (copy-region-as-kill nil nil t)
  (yank)
  )

