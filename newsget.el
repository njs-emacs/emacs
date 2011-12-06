(add-mode "\\.get$" 'get-mode)

(defun get-mode () (interactive)
  (run-hooks 'get-mode-hook)
  (setq mode-name "get-mode")
  (setq major-mode 'get-mode)
  )

(defun get-compile () (interactive)
  (let ((name (basename)))
    (save-buffer)
    (save-excursion
      (set-buffer (compile (format "perl get.pl %s" (buffer-file-name))))
      (setq compile-protect t)
      (rename-buffer name)
      )
    )
  )

(put 'get-mode 'eval-buffer-modal 'get-compile)

