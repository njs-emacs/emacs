;; yasnippet extension allows locally mapping a key combination to
;; a snippet

(defvar yas-x-map nil "Expand map for yas-x")

(set-default 'yas-x-map (make-sparse-keymap))

(make-variable-buffer-local 'yas-x-map)

(defun yas-x-expand (name)
  "Do the low-level dirty work for yas-x-expand-command."
  (setq templates
    (mapcan #'(lambda (table)
		(yas--fetch table name))
	    (yas--get-snippet-tables)))
  (yas--expand-or-prompt-for-template templates)
  )

(defun yas-x-expand-command (arg) (interactive "p")
  "Expand the snippet that was mapped to the current command key."
  (let* ((keys (this-command-keys))
	 (binding (lookup-key yas-x-map keys))
	 )
    (cond
     (binding (yas-x-expand binding))
     )
    )
  )

(defun yas-x-define (key name)
  "Locally define KEY to invoke YASNIPPET."
  (local-set-key key 'yas-x-expand-command)
  (define-key yas-x-map key name)
  )

(provide 'yas-x)
