(defun dos-buf ()
  (bob)
  (catch 'done
    (while t
      (eol)
      (or (eq (preceding-char) 13) (insert "\C-m"))
      (or (zerop (forward-line 1)) (throw 'done nil))
      )
    )
  (eob)
  )

(defun undos-buf ()
  (bob)
  (catch 'done
    (while t
      (eol)
      (while (eq (preceding-char) 13) (backward-delete-char 1))
      (or (zerop (forward-line 1)) (throw 'done nil))
      )
    )
  (bob)
  (and (rsf "\C-z" nil t) (delete-region (match-beginning 0) (point-max)))
  )

(defun dos-file-op (op name)
  (cond ((file-directory-p name))
	(t
	 (message name)
	 (find-file name)
	 (funcall op)
	 (save-buffer)
	 (kill-buffer (current-buffer))
	 )))

(defun dos-file (name) (dos-file-op 'dos-buf name))
(defun undos-file (name) (dos-file-op 'undos-buf name))

(defun dos-dir (name)
  (save-cd name
    (mapcar 'dos-file (ls))
    )
  )

(defun undos-dir (name)
  (save-cd name
    (mapcar 'undos-file (ls))
    )
  )
