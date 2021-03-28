(defun insert-key-description (keys) (interactive "kKey: ")
  (insert (key-description keys))
  )

(defun insert-key-description () (interactive)
  (insert (key-description (read-key-sequence "Keys: ")))
  )

; (insert-key-description)
; (read-key-sequence "Keys: ")
