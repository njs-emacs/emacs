(defvar yas-x-map nil "Expand map for yas-x")

(set-default 'yas-x-map (make-sparse-keymap))

(make-variable-buffer-local 'yas-x-map)

(defun yas-x-define (key name)
  (local-set-key key 'yas-x-expand-command)
  (define-key yas-x-map key name)
  )

(defun yas-x-expand (name)
  (setq templates
    (mapcan #'(lambda (table)
		(yas--fetch table name))
	    (yas--get-snippet-tables)))
  (yas--expand-or-prompt-for-template templates)
  )

(defun yas-x-expand-command (arg) (interactive "p")
  (let* ((keys (this-command-keys))
	 (binding (lookup-key yas-x-map keys))
	 )
    (cond
     (binding (yas-x-expand binding))
     )
    )
  )

(provide 'yas-x)
