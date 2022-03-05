(defun insert-key-description (keys) (interactive "kKey: ")
  (insert (key-description keys))
  )

; alternate interactive spec

(defun insert-key-description () (interactive)
  (insert (key-description (read-key-sequence "Keys: ")))
  )

; (insert-key-description)
; (read-key-sequence "Keys: ")

(defun key-binding-command (key) (interactive "kKey: ")
  (let* ((b (key-binding key))
	 (sk (format "\n(define-key global-map \"%s\" " key))
	 (sb (prin1-to-string b)))
    (concat sk (format (if (symbolp b) "'%s)\n" "\n  '%s)\n") sb)))
  )

(defun insert-key-binding-command (key) (interactive "kKey: ")
  (insert (key-binding-defun-string key)))

(defun insert-key-binding (k) (interactive "kKey: ")
  "Insert name of function bound to key in current buffer."
  (insert (symbol-name (key-binding k)))
  )

(defun insert-key-name (k) (interactive "kKey: ")
  "Insert name of key string."
  (insert (prin1-to-string k))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key l-map "\C-x\C-b" 'insert-key-binding)
(define-key l-map "\C-x\C-k" 'insert-key-name)
(define-key l-map "\C-x\C-d" 'insert-key-binding-command)

