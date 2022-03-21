(make-variable-buffer-local 'ps1-args)
(set-default 'ps1-args "")

(defun bash-command (cmd)
  (shell-command (format "bash -c \"%s\"" cmd))
  )

(defun ps1-eval-buffer () (interactive)
  (save-buffer)
;; there's a problem with cmdproxy invoking powershell
;; if we use bash in between it works, don't know why

;  (shell-command (format "powershell -Noninteractive -Command %s %s" (buffer-file-name) ps1-args))
  (bash-command (format "powershell -Noninteractive -Command %s %s" (buffer-file-name) ps1-args))
  )

(put 'ps1-mode 'eval-buffer-modal 'ps1-eval-buffer)

(defun ps1-mode ()
  (setq major-mode 'ps1-mode)
  (setq mode-name "PS1")
  )
