(defvar def-key-map-alist nil "Reverse mapping of keymap to prefix")
(defvar def-key-pretty nil "Format elements in def-key-show in even columns")
(defvar def-key-sort nil "Sort order def-key-show")

(defun def-key-map (key map)
  (setq def-key-map-alist (alist-put def-key-map-alist map key))
  )

(defvar def-key-history nil "Saved history of unevalled args to def-key")

(defmacro def-key (keymap key def)
  "Identical in functionality to define-key, with the side-effect that the
 definition is logged (unevaluated) to a list which gets saved on exit
 or on demand"
  (eval `(setq def-key-history
	   (cons (list ',keymap ,key ',def) def-key-history)))
  `(progn 
     (define-key ,keymap ,key ,def)
     )
  )

(defun def-key-fill-buffer () (interactive)
  (let ((buffer (get-buffer-create "*def-key*")))
    (set-buffer buffer)
    (erase-buffer)
    (mapcar
     '(lambda (x)
	(condition-case erc
	    (let ()
	      (insert
	       (format
		"(def-key %s (kbd \"%s\") '%S)\n"
		(car x)
		(key-description (cadr x))
		(eval (caddr x)))))
	  (error nil)
	  ))
	def-key-history)
    (shell-command-on-region (point-min) (point-max) "sort -u" 1 t)
    buffer
    )
  )

(defun def-key--save ()
  (interactive)
  (let* ((name (daily-month-path
	       (format-time-string "key-def--save-%y%m%d-%H%M%S.el"))
	      )
	 (buffer (def-key-fill-buffer))
	)
    (save-excursion (set-buffer buffer) (write-file name))
    (message "written key-def history to %s" name)
    )
  )

;(describe-variable 'def-key-history)

(add-hook 'kill-emacs-hook 'def-key--save)

;; (dregf "define-key" elfs "ever")
