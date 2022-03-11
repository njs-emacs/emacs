;; find first occurence of pattern in files which match this file's suffix
;; can be used to find template files

(defun drf* (pat &optional spec since)
  (let ((spec (or spec (format "\\.%s" (file-name-extension (buffer-file-name)))))
	(since (or since "ever"))
	)
    (dregf pat spec since "--one=1")
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun drf-interactive-args ()
  (list (read-from-minibuffer "Spec: " 
			      (format "\\.%s" (file-name-extension (buffer-file-name)))
			      )
	(read-from-minibuffer "Since: "  "ever"))
  )

(defun drf-interactive-args-maybe (&optional arg)
  (cond
   (arg (drf-interactive-args))
   )
  )

(defun drfx (&optional spec since)
 (drf* "." spec since)
 )

(defun drf (pattern &optional spec since)
  (interactive
    (apply 'list (read-from-minibuffer "Pattern: ")
	   (drf-interactive-args-maybe current-prefix-arg)
	   )
    )
  (drf* pattern spec since)
  )

(defun drf-template (&optional arg)
  (interactive "P")
  (let ((args (drf-interactive-args-maybe arg)))
    (apply 'drf* "##template" args)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq drf-map (make-sparse-keymap))
(define-key global-map (kbd "C-v C-f") drf-map)

(def-key drf-map (kbd "C-d") 'drf-template)

