(defun a (cmd) (interactive)
  (shell-command-on-region (point) (point-max) cmd)
  )
