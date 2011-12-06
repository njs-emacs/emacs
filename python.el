(setq compile-save-modes (adjoin 'python-mode compile-save-modes))

(set-default 'python-options nil)
(make-variable-buffer-local 'python-options)

(set-default 'python-script-options nil)
(make-variable-buffer-local 'python-script-options)

(setq python-exec "d:/I/inkscape-48.0.1/python/python")

(defun python-command (file)
  (let ((opts (mconcat python-options " ")))
    (format "%s %s \"%s\" %s" python-exec opts file python-script-options)
    )
  )

(defun python-compile () (interactive)
  (save-buffer)
  (save-excursion
    (set-buffer (compile (python-command (buffer-file-name))))
    (setq compile-protect t)
    )
  )

(defun eval-python-buffer () (interactive)
  (save-buffer)
  (shell-command (python-command (buffer-file-name)))
  )

;(put 'python-mode 'eval-buffer-modal 'eval-python-buffer)
(put 'python-mode 'eval-buffer-modal 'python-compile)

(defun python-mode-hook-ns ()
 ()
 )
