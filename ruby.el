(set-default 'ruby-options nil)
(make-variable-buffer-local 'ruby-options)

(defun ruby-command (file)
  (let ((opts (mconcat ruby-options " ")))
    (format "ruby %s \"%s\"" opts file)
    )
  )

(defun ruby-compile () (interactive)
  (save-buffer)
  (save-excursion
    (set-buffer (compile (ruby-command (buffer-file-name))))
    (setq compile-protect t)
    )
  )

(defun eval-ruby-buffer () (interactive)
  (save-buffer)
  (shell-command (ruby-command (buffer-file-name)))
  )

;(put 'ruby-mode 'eval-buffer-modal 'eval-ruby-buffer)
(put 'ruby-mode 'eval-buffer-modal 'ruby-compile)

(defun ruby-mode-hook-ns ()
  (bob)
  (while (rsf "^#\\+\\(.*\\)") (eval (read (ms 1))))
  )

(add-hook 'ruby-mode-hook 'ruby-mode-hook-ns)

(defun ruby-mode () (interactive)
  (setq major-mode 'ruby-mode)
  (setq mode-name "ruby")
  (setq grep-spec "*.rb")
  (run-hooks 'ruby-mode-hook)
  )

