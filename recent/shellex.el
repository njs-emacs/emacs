(defun shell-execute-text (s &rest plist)
  (let* ((nvc-global-enable nil)
	 (shell-program (or (plist-get plist :shell-program) "bash"))
	 (file-name (or (plist-get plist :file-name)
			(format "%s/bash-%d.tmp" (temporary-file-directory) (emacs-pid))))
	 (output-buffer-name (or (plist-get plist :output-buffer-name) "*shell-execute*"))
	 (output-buffer (get-buffer-create output-buffer-name))
	 shell-command
	 shell-output
	 exec-buffer
	 )

    (cond
     ((string= shell-program "powershell")
      (setq file-name (slash-back (format "%s.ps1" file-name)))
      (setq shell-command (format "%s %s" shell-program file-name))
      )
     ((string= shell-program "cmd")
      (setq file-name (slash-back (format "%s.bat" file-name)))
      (setq shell-command (format "%s /c %s" shell-program file-name))
      )
     (t
      (setq shell-command (format "%s %s" shell-program file-name))
      )
     )

    (save-excursion
      (setq exec-buffer (find-file-noselect file-name))
      (set-buffer exec-buffer)
      (set-buffer-file-coding-system 'unix)
      (erase-buffer)
      (insert s)
;      (with-suppressed-message (write-file file-name))
      (write-region nil nil file-name nil 0)
      (set-buffer-modified-p nil)
      )
    (shell-command shell-command output-buffer)
    (setq shell-output (get-buffer-string output-buffer))
    (cond
     ((plist-get plist :show))
     ((plist-get plist :kill)
      (kill-buffer output-buffer)
      )
     ((replace-buffer-in-windows output-buffer))
     )
    shell-output
    )
  )

(defun shell-execute-region-text (start end)
  (interactive "r")
  (shell-execute-text (bs start end))
  )

